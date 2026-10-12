-module(shell_test_ffi).
-export([
    new_pid_file/0,
    descendant_stopped/1,
    guardian_kills_tree/0,
    set_env/2,
    unset_env/1,
    process_running/1,
    delete_tree/1
]).

new_pid_file() ->
    list_to_binary("/tmp/vp-shell-test-" ++ os:getpid() ++ "-" ++
        integer_to_list(erlang:unique_integer([positive, monotonic]))).

descendant_stopped(Path) ->
    Result = case file:read_file(Path) of
        {ok, Contents} ->
            Pid = string:trim(binary_to_list(Contents)),
            Stopped = await_stopped(Pid, 100),
            %% Clean up even on failure so regression runs never leak fixtures.
            case Stopped of
                true -> ok;
                false -> os:cmd("kill -KILL " ++ Pid ++ " 2>/dev/null")
            end,
            Stopped;
        _ -> false
    end,
    file:delete(Path),
    Result.

await_stopped(_, 0) -> false;
await_stopped(Pid, Attempts) ->
    case string:trim(os:cmd("ps -p " ++ Pid ++ " -o stat=")) of
        "" -> true;
        [$Z | _] -> true; %% exited, awaiting the system's orphan reaper
        _ -> timer:sleep(10), await_stopped(Pid, Attempts - 1)
    end.

guardian_kills_tree() ->
    Path = new_pid_file(),
    Parent = self(),
    Owner = spawn(fun() ->
        Script = <<"sh -c 'echo $$ > ", Path/binary,
                   "; exec sleep 30' & echo ready; wait">>,
        {ok, Port} = shell_ffi:open_streaming_port(<<"sh">>, [<<"-c">>, Script]),
        {output_line, <<"ready">>} = shell_ffi:read_line(Port),
        Parent ! {ready, self()},
        receive finish -> shell_ffi:close_port(Port) end
    end),
    receive {ready, Owner} -> ok after 2000 -> exit(Owner, kill) end,
    Ready = await_file(Path, 100),
    exit(Owner, kill),
    Stopped = descendant_stopped(Path),
    Ready andalso Stopped.

set_env(Key, Value) ->
    os:putenv(binary_to_list(Key), binary_to_list(Value)).

unset_env(Key) ->
    os:unsetenv(binary_to_list(Key)).

process_running(PidValue) ->
    case string:to_integer(binary_to_list(PidValue)) of
        {Pid, []} when Pid > 1 ->
            Status = string:trim(os:cmd("ps -p " ++ integer_to_list(Pid) ++ " -o stat= 2>/dev/null")),
            Status =/= [] andalso not lists:prefix("Z", Status);
        _ -> false
    end.

delete_tree(PathValue) ->
    case file:del_dir_r(binary_to_list(PathValue)) of
        ok -> true;
        _ -> false
    end.

await_file(_, 0) -> false;
await_file(Path, Attempts) ->
    case file:read_file(Path) of
        {ok, <<_, _/binary>>} -> true;
        _ -> timer:sleep(10), await_file(Path, Attempts - 1)
    end.
