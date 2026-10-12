-module(watchdog_ffi).
-export([read_config/0, observe/1, load_state/1, save_state/2,
         with_lock/2, log_event/3, same_process/1, send_signal/2]).

read_config() ->
    try
        Path = os:getenv("WATCHDOG_CONFIG"),
        {ok, Bytes} = file:read_file(Path),
        C = json:decode(Bytes),
        App = maps:get(<<"app_dir">>, C),
        Dir = maps:get(<<"state_dir">>, C),
        Grace = maps:get(<<"startup_grace">>, C, 300),
        Threshold = maps:get(<<"failure_threshold">>, C, 3),
        Cooldown = maps:get(<<"restart_cooldown">>, C, 600),
        Term = maps:get(<<"termination_grace">>, C, 15),
        Home = unicode:characters_to_binary(os:getenv("HOME")),
        Label = maps:get(<<"agent_label">>, C, <<"local.video-puller">>),
        Plist = maps:get(<<"agent_plist">>, C, <<Home/binary, "/Library/LaunchAgents/", Label/binary, ".plist">>),
        true = is_binary(App) andalso is_binary(Dir),
        true = is_binary(Label) andalso is_binary(Plist),
        {match, _} = re:run(Label, <<"^local\\.video-puller[.a-z0-9_-]*$">>),
        true = lists:all(fun is_integer/1, [Grace, Threshold, Cooldown, Term]),
        true = Grace >= 0 andalso Threshold >= 1 andalso Cooldown >= 0 andalso Term >= 0,
        {ok, {config, App, Dir, Grace, Threshold, Cooldown, Term, Label, Plist}}
    catch _:_ -> {error, <<"Invalid WATCHDOG_CONFIG file">>} end.

observe({config, App, _, _, _, _, _, Label, AgentPlist}) ->
    try
        Uid = output(<<"/usr/bin/id">>, [<<"-u">>]),
        Candidates = [{<<"gui/", Uid/binary, "/", Label/binary>>, AgentPlist},
                      {<<"system/local.video-puller.boot">>,
                       <<"/Library/LaunchDaemons/local.video-puller.boot.plist">>}],
        Jobs = lists:filtermap(fun(Candidate) -> resolve(Candidate, App, Uid) end, Candidates),
        case Jobs of
            [] -> waiting;
            [Service] -> {running, Service, healthy(Service)};
            _ -> {fault, <<"Both login and boot pullers are running; resolve the duplicate">>}
        end
    catch
        throw:{watchdog_error, Reason} -> {fault, Reason};
        _:_ -> {fault, <<"Cannot safely validate the loaded service">>}
    end.

resolve({Target, Plist}, App, Uid) ->
    case exec(<<"/bin/launchctl">>, [<<"print">>, Target]) of
        {ok, 0, Text} ->
            Parts = binary:split(Target, <<"/">>, [global]),
            Label = lists:last(Parts),
            Domain = iolist_to_binary(lists:join(<<"/">>, lists:droplast(Parts))),
            Disabled = output(<<"/bin/launchctl">>, [<<"print-disabled">>, Domain]),
            Pattern = iolist_to_binary([<<"\"">>, escape_re(Label), <<"\"\\s*=>\\s*disabled">>]),
            case {re:run(Disabled, Pattern), value(Text, <<"pid = ">>)} of
                {{match, _}, _} -> false;
                {_, undefined} -> false;
                {nomatch, PidBytes} ->
                    require(value(Text, <<"path = ">>) =:= Plist, <<"Loaded plist path changed">>),
                    Config = json:decode(output(<<"/usr/bin/plutil">>, [<<"-convert">>, <<"json">>, <<"-o">>, <<"-">>, Plist])),
                    Entrypoint = <<App/binary, "/build/erlang-shipment/entrypoint.sh">>,
                    require(maps:get(<<"KeepAlive">>, Config, false) =:= true,
                            <<"Refusing to stop a service without KeepAlive=true">>),
                    require(maps:get(<<"ProgramArguments">>, Config, []) =:= [Entrypoint, <<"run">>],
                            <<"Loaded service entrypoint changed">>),
                    require(maps:get(<<"WorkingDirectory">>, Config, <<>>) =:= App,
                            <<"Loaded service working directory changed">>),
                    Env = maps:get(<<"EnvironmentVariables">>, Config, #{}),
                    Port = binary_to_integer(maps:get(<<"PORT">>, Env, <<"8080">>)),
                    require(Port > 0 andalso Port < 65536, <<"Invalid local health port">>),
                    Pid = binary_to_integer(PidBytes),
                    case process_identity(Pid) of
                        undefined -> false;
                        Identity ->
                            Fields = re:split(Identity, <<"\\s+">>, [{return, binary}, trim]),
                            %% uid, weekday, month, day, time, year, argv[0], arguments.
                            %% Keep args last: macOS truncates a preceding comm column.
                            require(length(Fields) >= 8, <<"Incomplete process identity">>),
                            require(hd(Fields) =:= Uid, <<"Service belongs to another user">>),
                            Exe = lists:nth(7, Fields),
                            require(filename:basename(Exe) =:= <<"beam.smp">>, <<"Service is not BEAM yet">>),
                            require(binary:match(Identity, <<App/binary, "/">>) =/= nomatch,
                                    <<"BEAM arguments do not match the expected app">>),
                            {true, {service, Target, Pid, Identity, Port}}
                    end
            end;
        {ok, _, _} -> false;
        {error, Reason} -> throw({watchdog_error, Reason})
    end.

require(true, _) -> ok;
require(false, Reason) -> throw({watchdog_error, Reason}).

escape_re(Value) ->
    re:replace(Value, <<"([.])">>, <<"\\\\\\1">>, [global, {return, binary}]).

value(Text, Prefix) ->
    Matches = [binary:part(Line, byte_size(Prefix), byte_size(Line) - byte_size(Prefix))
               || Raw <- binary:split(Text, <<"\n">>, [global]),
                  Line <- [string:trim(Raw)],
                  binary:match(Line, Prefix) =:= {0, byte_size(Prefix)}],
    case Matches of [First|_] -> string:trim(First); [] -> undefined end.

process_identity(Pid) ->
    case exec(<<"/bin/ps">>, [<<"-p">>, integer_to_binary(Pid), <<"-o">>, <<"uid=,lstart=,args=">>]) of
        {ok, 0, Text} -> case string:trim(Text) of <<>> -> undefined; Identity -> Identity end;
        _ -> undefined
    end.

same_process({service, _, Pid, Identity, _}) -> process_identity(Pid) =:= Identity.

send_signal(Service = {service, _, Pid, _, _}, Signal) when Signal =:= <<"TERM">>; Signal =:= <<"KILL">> ->
    case same_process(Service) of
        false -> {error, <<"Process identity changed before signal">>};
        true ->
            case exec(<<"/bin/kill">>, [<<"-", Signal/binary>>, integer_to_binary(Pid)]) of
                {ok, 0, _} -> {ok, nil};
                _ -> {error, <<"Could not signal the validated BEAM process">>}
            end
    end.

healthy({service, _, _, _, Port}) ->
    try
        {ok, _} = application:ensure_all_started(inets),
        Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/health",
        case httpc:request(get, {Url, [{"connection", "close"}]},
                           [{timeout, 5000}, {connect_timeout, 2000}, {autoredirect, false}],
                           [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} when byte_size(Body) =< 16384 ->
                Data = json:decode(Body),
                maps:get(<<"status">>, Data) =:= <<"ok">>
                andalso maps:get(<<"database">>, Data) =:= <<"connected">>;
            _ -> false
        end
    catch _:_ -> false end.

load_state(Dir) ->
    try
        {ok, Bytes} = file:read_file(filename:join(Dir, <<"state.term">>)),
        State = {state, Instance, First, Last, Failures, Restart, Status} = binary_to_term(Bytes, [safe]),
        true = is_integer(First) andalso is_integer(Last) andalso is_integer(Failures) andalso Failures >= 0,
        true = is_binary(Status),
        true = valid_instance(Instance),
        true = case Restart of none -> true; {some, R} -> is_integer(R); _ -> false end,
        State
    catch _:_ -> {state, none, 0, 0, 0, none, <<"new">>} end.

valid_instance(none) -> true;
valid_instance({some, {service, Target, Pid, Identity, Port}}) ->
    is_binary(Target) andalso is_integer(Pid) andalso Pid > 0 andalso is_binary(Identity)
    andalso is_integer(Port) andalso Port > 0 andalso Port < 65536;
valid_instance(_) -> false.

save_state(Dir, State) ->
    Path = filename:join(Dir, <<"state.term">>),
    Temporary = <<Path/binary, ".next">>,
    case file:write_file(Temporary, term_to_binary(State), [sync]) of
        ok ->
            _ = file:change_mode(Temporary, 8#600),
            case file:rename(Temporary, Path) of
                ok -> {ok, nil};
                _ -> {error, <<"Cannot atomically save watchdog state">>}
            end;
        _ -> {error, <<"Cannot persist restart cooldown; recovery skipped">>}
    end.

with_lock(Dir, Fun) ->
    _ = filelib:ensure_dir(filename:join(Dir, <<"placeholder">>)),
    _ = file:change_mode(Dir, 8#700),
    Lock = filename:join(Dir, <<"lock">>),
    Owner = filename:join(Lock, <<"owner.term">>),
    case take_lock(Lock, Owner, true) of
        true ->
            try Fun()
            after file:delete(Owner), file:del_dir(Lock) end;
        false -> nil
    end.

take_lock(Lock, Owner, Retry) ->
    case file:make_dir(Lock) of
        ok ->
            Pid = list_to_integer(os:getpid()),
            Identity = process_identity(Pid),
            Write = case is_binary(Identity) of
                true -> file:write_file(Owner, term_to_binary({Pid, Identity}), [sync]);
                false -> {error, no_identity}
            end,
            case Write of
                ok -> true;
                _ -> file:del_dir(Lock), false
            end;
        {error, eexist} when Retry ->
            %% A killed watchdog can leave a lock. Remove it only after checking
            %% the recorded process birth fingerprint; never steal a live lock.
            case file:read_file(Owner) of
                {ok, Bytes} ->
                    try
                        {Pid, Identity} = binary_to_term(Bytes, [safe]),
                        case process_identity(Pid) =:= Identity of
                            true -> false;
                            false -> file:delete(Owner), file:del_dir(Lock), take_lock(Lock, Owner, false)
                        end
                    catch _:_ -> clear_stale_lock(Lock, Owner) end;
                _ ->
                    %% Another process may still be writing its owner record.
                    %% A stale empty lock is handled by its directory age.
                    clear_stale_lock(Lock, Owner)
            end;
        _ -> false
    end.

clear_stale_lock(Lock, Owner) ->
    Now = erlang:system_time(second),
    case file:read_file_info(Lock, [{time, posix}]) of
        {ok, Info} when erlang:element(6, Info) < Now - 120 ->
            file:delete(Owner), file:del_dir(Lock), take_lock(Lock, Owner, false);
        _ -> false
    end.

log_event(Dir, Event, Detail) ->
    Path = filename:join(Dir, <<"watchdog.log">>),
    case file:read_file_info(Path) of
        {ok, Info} when erlang:element(2, Info) >= 1000000 ->
            _ = file:delete(<<Path/binary, ".3">>),
            _ = file:rename(<<Path/binary, ".2">>, <<Path/binary, ".3">>),
            _ = file:rename(<<Path/binary, ".1">>, <<Path/binary, ".2">>),
            _ = file:rename(Path, <<Path/binary, ".1">>);
        _ -> ok
    end,
    Line = json:encode(#{time => erlang:system_time(second), event => Event, detail => Detail}),
    _ = file:write_file(Path, [Line, <<"\n">>], [append]),
    nil.

output(Executable, Args) ->
    case exec(Executable, Args) of
        {ok, 0, Bytes} -> string:trim(Bytes);
        _ -> throw({watchdog_error, <<"Native service inspection failed">>})
    end.

exec(Executable, Args) ->
    try
        Port = open_port({spawn_executable, binary_to_list(Executable)},
                         [binary, use_stdio, stderr_to_stdout, exit_status,
                          {args, [binary_to_list(Arg) || Arg <- Args]}]),
        Deadline = erlang:monotonic_time(millisecond) + 10000,
        collect(Port, Deadline, <<>>)
    catch _:_ -> {error, <<"Cannot execute native service inspection">>} end.

collect(Port, Deadline, Acc) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, Bytes}} when byte_size(Acc) + byte_size(Bytes) =< 131072 ->
            collect(Port, Deadline, <<Acc/binary, Bytes/binary>>);
        {Port, {data, _}} -> close_port(Port), {error, <<"Inspection output too large">>};
        {Port, {exit_status, Code}} -> {ok, Code, Acc}
    after Timeout -> close_port(Port), {error, <<"Native inspection timed out">>}
    end.

close_port(Port) ->
    try port_close(Port) catch _:_ -> ok end.
