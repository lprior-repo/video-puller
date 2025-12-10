-module(shell_ffi).

-export([open_streaming_port/2, read_line/1, close_port/1, is_port_alive/1]).

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

%% Read a line from the port with timeout
%% Returns Gleam StreamLine type:
%% {output_line, Binary} | end_of_stream | {process_exit, Int} | {stream_error, Binary}
read_line(Port) ->
    %% 5 minutes timeout - yt-dlp can be slow (metadata fetch, rate limiting, etc.)
    read_line_timeout(Port, 300000).

read_line_timeout(Port, Timeout) ->
    receive
        {Port, {data, {eol, Bytes}}} ->
            {output_line, list_to_binary(Bytes)};
        {Port, {data, {noeol, Bytes}}} ->
            {output_line, list_to_binary(Bytes)};
        {Port, eof} ->
            end_of_stream;
        {Port, {exit_status, Code}} ->
            {process_exit, Code};
        {'EXIT', Port, Reason} ->
            {stream_error, list_to_binary(io_lib:format("~p", [Reason]))}
    after Timeout ->
        {stream_error, <<"timeout">>}
    end.

%% Close the port gracefully
close_port(Port) ->
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

%% Check if port is still alive
is_port_alive(Port) ->
    try
        erlang:port_info(Port) =/= undefined
    catch
        _:_ ->
            false
    end.
