-module(service_supervisor_test_ffi).
-export([supervisor_child/2, supervisor_child_by_type/2, exit_process/2,
         stop_supervisor/1, wait_for_routes/2, initial_module/1, poll_guard_pid/0,
         kill_poll_guard/0]).

kill_poll_guard() ->
    Pid = whereis(subscription_manager_poll_guard),
    Monitor = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive {'DOWN', Monitor, process, Pid, _} -> nil after 1000 -> error(timeout) end.

initial_module(Pid) ->
    {Module, _, _} = proc_lib:translate_initial_call(Pid),
    atom_to_binary(Module, utf8).

poll_guard_pid() -> whereis(subscription_manager_poll_guard).

supervisor_child(Supervisor, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(Supervisor)) of
        {Id, Pid, _Type, _Modules} when is_pid(Pid) -> {ok, Pid};
        _ -> {error, nil}
    end.

supervisor_child_by_type(Supervisor, Type) ->
    Expected = case Type of <<"supervisor">> -> supervisor; _ -> worker end,
    case [Pid || {_Id, Pid, ChildType, _Modules} <- supervisor:which_children(Supervisor),
                 ChildType =:= Expected, is_pid(Pid)] of
        [Pid | _] -> {ok, Pid};
        [] -> {error, nil}
    end.

exit_process(Pid, Reason) ->
    exit(Pid, binary_to_atom(Reason, utf8)),
    nil.

stop_supervisor(Pid) ->
    Monitor = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Monitor, process, Pid, _Reason} -> nil
    after 10000 ->
        erlang:demonitor(Monitor, [flush]),
        nil
    end.

wait_for_routes(Attempts, Delay) ->
    _ = application:ensure_all_started(inets),
    wait_for_routes(Attempts, Delay, false).

wait_for_routes(0, _Delay, _LastResult) -> false;
wait_for_routes(Attempts, Delay, _LastResult) ->
    Health = request_status("http://127.0.0.1:8098/health"),
    Live = request_status("http://127.0.0.1:8098/subscriptions/live"),
    case Health =:= 200 andalso Live =:= 200 of
        true -> true;
        false ->
            timer:sleep(Delay),
            wait_for_routes(Attempts - 1, Delay, false)
    end.

request_status(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 1000}], [{body_format, binary}]) of
        {ok, {{_Version, Status, _Reason}, _Headers, _Body}} -> Status;
        _ -> 0
    end.
