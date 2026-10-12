-module(shell_ffi).

-include_lib("kernel/include/file.hrl").

-export([
    open_streaming_port/2,
    read_line/1,
    read_line_timeout/2,
    close_port/1,
    kill_port_tree/1,
    stop_download_processes/0,
    recover_orphan_downloads/0
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
                        do_open_port(CommandChars, CommandChars, Args)
                end;
            Executable ->
                do_open_port(CommandChars, Executable, Args)
        end
    catch
        error:Reason ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

do_open_port(CommandChars, ExecutableChars, Args) ->
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
        case register_download_port(CommandChars, OsPid) of
            {ok, RegistryPath} ->
                put({shell_download_registry, Port}, RegistryPath),
                Guardian = spawn(fun() -> guard_process(Owner, Port, OsPid, RegistryPath) end),
                put({shell_guardian, Port}, Guardian),
                {ok, Port};
            {error, RegistrationReason} ->
                _ = kill_process_tree(OsPid),
                close_port_quietly(Port),
                {error, list_to_binary(io_lib:format("cannot register downloader process: ~p", [RegistrationReason]))}
        end
    catch
        error:OpenReason ->
            {error, list_to_binary(io_lib:format("~p", [OpenReason]))}
    end.

close_port_quietly(Port) ->
    try erlang:port_close(Port) of
        _ -> ok
    catch
        _:_ -> ok
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
guard_process(Owner, Port, OsPid, RegistryPath) ->
    OwnerRef = monitor(process, Owner),
    PortRef = monitor(port, Port),
    receive
        {release, Owner, Port} -> ok;
        {'DOWN', OwnerRef, process, Owner, _} ->
            cleanup_guarded_process(OsPid, RegistryPath);
        {'DOWN', PortRef, port, Port, _} ->
            cleanup_guarded_process(OsPid, RegistryPath)
    end,
    demonitor(OwnerRef, [flush]),
    demonitor(PortRef, [flush]).

%% Freeze a parent before discovering its children, then recursively freeze
%% and kill descendants before the parent. This prevents a live ancestor from
%% spawning more children while cleanup runs. ps and kill work on Linux/macOS;
%% pkill -P alone only kills one generation and leaves ffmpeg grandchildren.
kill_port_tree(Port) ->
    RegistryPath = erase({shell_download_registry, Port}),
    release_guardian(Port),
    case RegistryPath of
        undefined -> kill_port_tree_unregistered(Port);
        download_exited -> ok;
        Path -> _ = terminate_registered_download(Path), ok
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
            wait_for_process_exit(OsPid, 100);
        _ -> process_is_stopped(OsPid)
    end.

kill_port_tree_unregistered(Port) ->
    try erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} -> _ = kill_process_tree(OsPid), ok;
        _ -> ok
    catch
        error:badarg -> ok
    end.

%% Close the port gracefully
close_port(Port) ->
    RegistryPath = erase({shell_download_registry, Port}),
    release_guardian(Port),
    case RegistryPath of
        undefined -> ok;
        download_exited -> ok;
        Path -> _ = terminate_registered_download(Path)
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

%% Called by the BEAM storage guard before shutting the VM down.
stop_download_processes() ->
    lists:foreach(fun kill_port_tree/1, erlang:ports()),
    nil.

%% Recover only download processes registered by a previous VM. Process birth
%% and command fingerprints prevent killing a different process after PID reuse.
recover_orphan_downloads() ->
    case registry_directory() of
        {ok, Directory} ->
            case file:list_dir(Directory) of
                {ok, Names} ->
                    lists:foldl(fun(Name, Count) ->
                        case filename:extension(Name) of
                            ".record" ->
                                Path = filename:join(Directory, Name),
                                Count + recover_registry_record(Path);
                            _ -> Count
                        end
                    end, 0, Names);
                {error, enoent} -> 0;
                {error, _} -> 0
            end;
        _ -> 0
    end.

recover_registry_record(Path) ->
    case read_registry_record(Path) of
        {ok, {EntryVmPid, EntryVmFingerprint, RootPid, RootBirth, CommandHash, _Kind}} ->
            case vm_owner_is_alive(EntryVmPid, EntryVmFingerprint) of
                true -> 0;
                false ->
                    case process_details(RootPid) of
                        {ok, RootBirth, CommandHash} ->
                            case kill_process_tree(RootPid) of
                                true ->
                                    _ = file:delete(Path),
                                    1;
                                false -> 0
                            end;
                        _ ->
                            _ = file:delete(Path),
                            0
                    end
            end;
        error -> 0
    end.

vm_owner_is_alive(VmPid, VmFingerprint) ->
    case process_details(VmPid) of
        {ok, Birth, CommandHash} ->
            hash_hex(Birth ++ CommandHash) =:= VmFingerprint;
        _ -> false
    end.

register_download_port(Command, OsPid) ->
    Kind = filename:basename(Command),
    case lists:member(Kind, ["yt-dlp", "ytdl-sub"]) of
        false -> {ok, undefined};
        true ->
            case {process_details(OsPid), current_vm_identity(), registry_directory()} of
                {{ok, RootBirth, CommandHash}, {ok, VmPid, VmFingerprint}, {ok, Directory}} ->
                    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
                    Name = integer_to_list(VmPid) ++ "-" ++ Unique ++ ".record",
                    Path = filename:join(Directory, Name),
                    Temp = Path ++ ".tmp",
                    Contents = registry_record(VmPid, VmFingerprint, OsPid, RootBirth, CommandHash, Kind),
                    case write_registry_record(Temp, Path, Contents) of
                        ok -> {ok, Path};
                        {error, Reason} -> {error, Reason}
                    end;
                {{error, process_gone}, _, _} -> {ok, download_exited};
                {{error, Reason}, _, _} -> {error, Reason};
                {_, {error, Reason}, _} -> {error, Reason};
                {_, _, {error, Reason}} -> {error, Reason}
            end
    end.

write_registry_record(Temp, Path, Contents) ->
    try
        ok = filelib:ensure_dir(Temp),
        ok = file:write_file(Temp, Contents, [raw, binary, exclusive]),
        ok = file:change_mode(Temp, 8#600),
        ok = file:rename(Temp, Path),
        ok
    catch
        error:Reason ->
            _ = file:delete(Temp),
            {error, Reason};
        throw:Reason ->
            _ = file:delete(Temp),
            {error, Reason}
    end.

registry_record(VmPid, VmFingerprint, RootPid, RootBirth, CommandHash, Kind) ->
    iolist_to_binary(io_lib:format(
        "v1~nvm_pid=~B~nvm_fp=~s~nroot_pid=~B~nroot_birth=~s~ncmd_hash=~s~nkind=~s~n",
        [VmPid, VmFingerprint, RootPid, RootBirth, CommandHash, Kind]
    )).

read_registry_record(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, size = Size}} when Size > 0, Size =< 4096 ->
            case file:read_file(Path) of
                {ok, Contents} -> parse_registry_record(Contents);
                _ -> error
            end;
        _ -> error
    end.

parse_registry_record(Contents) ->
    Lines = [Line || Line <- string:tokens(binary_to_list(Contents), "\r\n"), Line =/= []],
    case Lines of
        ["v1", VmPidLine, VmFingerprintLine, RootPidLine, RootBirthLine, CommandHashLine, KindLine] ->
            case {
                parse_record_integer(VmPidLine, "vm_pid="),
                parse_record_hash(VmFingerprintLine, "vm_fp="),
                parse_record_integer(RootPidLine, "root_pid="),
                parse_record_hash(RootBirthLine, "root_birth="),
                parse_record_hash(CommandHashLine, "cmd_hash="),
                parse_record_kind(KindLine)
            } of
                {{ok, VmPid}, {ok, VmFingerprint}, {ok, RootPid}, {ok, RootBirth}, {ok, CommandHash}, {ok, Kind}} ->
                    {ok, {VmPid, VmFingerprint, RootPid, RootBirth, CommandHash, Kind}};
                _ -> error
            end;
        _ -> error
    end.

parse_record_integer(Line, Prefix) ->
    case lists:prefix(Prefix, Line) of
        true ->
            case string:to_integer(lists:nthtail(length(Prefix), Line)) of
                {Value, []} when Value > 1 -> {ok, Value};
                _ -> error
            end;
        false -> error
    end.

parse_record_hash(Line, Prefix) ->
    case lists:prefix(Prefix, Line) of
        true ->
            Value = lists:nthtail(length(Prefix), Line),
            case length(Value) =:= 64 andalso lists:all(fun is_lower_hex/1, Value) of
                true -> {ok, Value};
                false -> error
            end;
        false -> error
    end.

parse_record_kind("kind=yt-dlp") -> {ok, "yt-dlp"};
parse_record_kind("kind=ytdl-sub") -> {ok, "ytdl-sub"};
parse_record_kind(_) -> error.

is_lower_hex(Char) ->
    (Char >= $0 andalso Char =< $9) orelse (Char >= $a andalso Char =< $f).

read_registry_record_for_cleanup(Path) -> read_registry_record(Path).

terminate_registered_download(Path) ->
    case file:read_file_info(Path) of
        {error, enoent} -> true;
        _ ->
            case read_registry_record_for_cleanup(Path) of
                {ok, {_VmPid, _VmFingerprint, RootPid, RootBirth, CommandHash, _Kind}} ->
                    case process_details(RootPid) of
                        {ok, RootBirth, CommandHash} ->
                            case kill_process_tree(RootPid) of
                                true -> _ = file:delete(Path), true;
                                false -> false
                            end;
                        _ ->
                            _ = file:delete(Path),
                            true
                    end;
                error -> false
            end
    end.

cleanup_guarded_process(OsPid, undefined) -> _ = kill_process_tree(OsPid), ok;
cleanup_guarded_process(_OsPid, download_exited) -> ok;
cleanup_guarded_process(OsPid, RegistryPath) ->
    case terminate_registered_download(RegistryPath) of
        true -> ok;
        false -> _ = kill_process_tree(OsPid), ok
    end.

release_guardian(Port) ->
    case erase({shell_guardian, Port}) of
        undefined -> ok;
        Guardian -> Guardian ! {release, self(), Port}
    end.

current_vm_identity() ->
    try list_to_integer(os:getpid()) of
        VmPid ->
            case process_details(VmPid) of
                {ok, Birth, CommandHash} ->
                    {ok, VmPid, hash_hex(Birth ++ CommandHash)};
                {error, Reason} -> {error, Reason}
            end
    catch
        _:_ -> {error, invalid_vm_pid}
    end.

process_details(Pid) when is_integer(Pid), Pid > 1 ->
    PidText = integer_to_list(Pid),
    Birth = string:trim(os:cmd("ps -p " ++ PidText ++ " -o lstart= 2>/dev/null")),
    Command = string:trim(os:cmd("ps -p " ++ PidText ++ " -o command= 2>/dev/null")),
    case Birth =/= [] andalso Command =/= [] of
        true -> {ok, hash_hex(Birth), hash_hex(Command)};
        false -> {error, process_gone}
    end;
process_details(_) -> {error, process_gone}.

hash_hex(Value) ->
    binary_to_list(binary:encode_hex(crypto:hash(sha256, unicode:characters_to_binary(Value)), lowercase)).

registry_directory() ->
    DataDir = case os:getenv("DATA_DIR") of
        false -> "./data";
        Value -> Value
    end,
    Directory = filename:absname(filename:join([DataDir, "run", "processes"])),
    case filelib:ensure_dir(filename:join(Directory, ".keep")) of
        ok -> {ok, Directory};
        {error, Reason} -> {error, Reason}
    end.

wait_for_process_exit(_Pid, 0) -> false;
wait_for_process_exit(Pid, Attempts) ->
    case process_is_stopped(Pid) of
        true -> true;
        false -> timer:sleep(10), wait_for_process_exit(Pid, Attempts - 1)
    end.

process_is_stopped(Pid) ->
    Status = string:trim(os:cmd("ps -p " ++ integer_to_list(Pid) ++ " -o stat= 2>/dev/null")),
    Status =:= [] orelse lists:prefix("Z", Status).
