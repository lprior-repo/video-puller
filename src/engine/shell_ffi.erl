-module(shell_ffi).

-export([
    open_streaming_port/2,
    read_line/1,
    read_line_timeout/2,
    close_port/1,
    kill_port_tree/1
]).

%% Open a port for streaming command output
%% Returns {ok, Port} or {error, Reason}
open_streaming_port(Command, Args) ->
    try
        CommandChars = binary_to_list(Command),
        case os:find_executable(CommandChars) of
            false ->
                case filelib:is_file(CommandChars) of
                    false ->
                        ExecutableError =
                            list_to_binary("command `" ++ CommandChars ++ "` not found"),
                        {error, ExecutableError};
                    true ->
                        do_open_port(CommandChars, Args)
                end;
            Executable ->
                do_open_port(Executable, Args)
        end
    catch
        error:Reason ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

do_open_port(ExecutableChars, Args) ->
    %% Get current PATH and ensure common Node locations are included
    Env = build_env_with_node_path(),
    PortSettings = [
        {args, Args},
        {line, 65536},  % Max line length
        exit_status,
        hide,
        stderr_to_stdout,
        eof,
        {env, Env}
    ],
    try
        Port = open_port({spawn_executable, ExecutableChars}, PortSettings),
        {os_pid, OsPid} = erlang:port_info(Port, os_pid),
        Owner = self(),
        Guardian = spawn(fun() -> guard_process(Owner, Port, OsPid) end),
        put({shell_guardian, Port}, Guardian),
        {ok, Port}
    catch
        error:Reason ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Build environment with Node in PATH for yt-dlp JS interpreter
build_env_with_node_path() ->
    CurrentPath = os:getenv("PATH", "/usr/bin:/bin"),
    Home = os:getenv("HOME", ""),
    %% Common Node.js installation paths including mise, nvm, fnm, volta
    NodePaths = [
        "/usr/local/bin",
        "/usr/bin",
        Home ++ "/.local/share/mise/shims",
        Home ++ "/.nvm/versions/node/current/bin",
        Home ++ "/.local/bin",
        Home ++ "/.volta/bin",
        Home ++ "/.fnm/aliases/default/bin",
        "/opt/homebrew/bin",
        "/opt/nodejs/bin"
    ],
    %% Filter out empty paths and combine
    ValidNodePaths = [P || P <- NodePaths, P =/= "", P =/= "/bin"],
    NewPath = string:join([CurrentPath | ValidNodePaths], ":"),
    [{"PATH", NewPath}].

%% Read a line from the port (no timeout version for stream_loop)
%% Returns Gleam StreamLine type directly (not wrapped in Result):
%% {output_line, Binary} | end_of_stream | {process_exit, Int} | {stream_error, Binary}
read_line(Port) ->
    %% 5 minutes timeout - yt-dlp can be slow (metadata fetch, rate limiting, etc.)
    case read_line_timeout(Port, 300000) of
        {ok, Result} -> Result;
        {error, timeout} -> {stream_error, <<"timeout">>}
    end.

read_line_timeout(Port, Timeout) ->
    receive
        {Port, {data, {eol, Bytes}}} ->
            {ok, {output_line, list_to_binary(Bytes)}};
        {Port, {data, {noeol, Bytes}}} ->
            {ok, {output_line, list_to_binary(Bytes)}};
        {Port, eof} ->
            {ok, end_of_stream};
        {Port, {exit_status, Code}} ->
            {ok, {process_exit, Code}};
        {'EXIT', Port, Reason} ->
            {ok, {stream_error, list_to_binary(io_lib:format("~p", [Reason]))}}
    after Timeout ->
        {error, timeout}
    end.

%% Keep cleanup independent of the caller: a crashed/killed worker cannot
%% execute its own timeout cleanup, and closing its port does not kill a child.
%% Explicit close releases the guardian; every unexpected close kills the tree.
guard_process(Owner, Port, OsPid) ->
    OwnerRef = monitor(process, Owner),
    PortRef = monitor(port, Port),
    receive
        {release, Owner, Port} -> ok;
        {'DOWN', OwnerRef, process, Owner, _} -> kill_process_tree(OsPid);
        {'DOWN', PortRef, port, Port, _} -> kill_process_tree(OsPid)
    end,
    demonitor(OwnerRef, [flush]),
    demonitor(PortRef, [flush]).

%% Freeze a parent before discovering its children, then recursively freeze
%% and kill descendants before the parent. This prevents a live ancestor from
%% spawning more children while cleanup runs. ps and kill work on Linux/macOS;
%% pkill -P alone only kills one generation and leaves ffmpeg grandchildren.
kill_port_tree(Port) ->
    try erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} -> kill_process_tree(OsPid);
        _ -> ok
    catch
        error:badarg -> ok
    end.

kill_process_tree(OsPid) when is_integer(OsPid), OsPid > 1 ->
    Pid = integer_to_list(OsPid),
    %% Only continue if the process still exists; never walk from an absent PID.
    case string:trim(os:cmd("kill -STOP " ++ Pid ++ " 2>/dev/null; echo $?")) of
        "0" ->
            Rows = string:split(os:cmd("ps -axo pid=,ppid="), "\n", all),
            Children = lists:filtermap(fun(Row) ->
                case string:lexemes(Row, " \t\r") of
                    [Child, Parent] ->
                        case {string:to_integer(Child), string:to_integer(Parent)} of
                            {{ChildPid, ""}, {OsPid, ""}} when ChildPid > 1 ->
                                {true, ChildPid};
                            _ -> false
                        end;
                    _ -> false
                end
            end, Rows),
            lists:foreach(fun kill_process_tree/1, Children),
            _ = os:cmd("kill -KILL " ++ Pid ++ " 2>/dev/null"),
            ok;
        _ -> ok
    end.

%% Close the port gracefully
close_port(Port) ->
    case erase({shell_guardian, Port}) of
        undefined -> ok;
        Guardian -> Guardian ! {release, self(), Port}
    end,
    try
        Port ! {self(), close},
        receive
            {Port, closed} ->
                ok
        after 1000 ->
            erlang:port_close(Port),
            ok
        end,
        % Drain any remaining messages
        receive
            {'EXIT', Port, _} ->
                ok
        after 100 ->
            ok
        end,
        ok
    catch
        _:_ ->
            ok
    end.
