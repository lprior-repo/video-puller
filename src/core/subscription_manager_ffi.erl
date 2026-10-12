-module(subscription_manager_ffi).
-export([stop_worker/1, await_previous_poll_cleanup/1, guard_poll_worker/3]).

-define(POLL_GUARD, subscription_manager_poll_guard).

%% A long-lived unlinked process owns the poll-worker monitors. When a manager
%% dies, it stops the worker and its linked downloader tree before releasing
%% the name. start_named/2 asks this process to clear old ownership first, so
%% a RestForOne restart cannot overlap the previous poll's lock ownership.
await_previous_poll_cleanup(Name) ->
    case ensure_poll_guard() of
        {ok, Guard} ->
            Ref = make_ref(),
            Guard ! {await_clear, self(), Ref, Name},
            receive
                {Ref, ready} -> true;
                {Ref, busy} -> false
            after 15000 -> false
            end;
        error -> false
    end.

guard_poll_worker(Name, Manager, Worker) ->
    case ensure_poll_guard() of
        {ok, Guard} -> Guard ! {track, Name, Manager, Worker}, nil;
        error -> nil
    end.

ensure_poll_guard() ->
    case whereis(?POLL_GUARD) of
        undefined ->
            Pid = spawn(fun() -> poll_guard_loop([]) end),
            try register(?POLL_GUARD, Pid) of
                true -> {ok, Pid}
            catch
                error:badarg ->
                    exit(Pid, kill),
                    case whereis(?POLL_GUARD) of
                        undefined -> error;
                        Existing -> {ok, Existing}
                    end
            end;
        Pid -> {ok, Pid}
    end.

poll_guard_loop(Entries) ->
    receive
        {track, Name, Manager, Worker} ->
            OwnerRef = erlang:monitor(process, Manager),
            WorkerRef = erlang:monitor(process, Worker),
            poll_guard_loop([
                {Name, Manager, Worker, OwnerRef, WorkerRef} | Entries
            ]);
        {await_clear, From, Ref, Name} ->
            Matching = [Entry || Entry = {EntryName, _, _, _, _} <- Entries, EntryName =:= Name],
            case lists:any(fun({_, Manager, _, _, _}) -> is_process_alive(Manager) end, Matching) of
                true ->
                    From ! {Ref, busy},
                    poll_guard_loop(Entries);
                false ->
                    Remaining = lists:foldl(fun(Entry, Acc) ->
                        case entry_name(Entry) =:= Name of
                            true ->
                                stop_guarded_worker(Entry),
                                remove_entry(Entry, Acc);
                            false -> Acc
                        end
                    end, Entries, Matching),
                    From ! {Ref, ready},
                    poll_guard_loop(Remaining)
            end;
        {'DOWN', Ref, process, _Pid, _Reason} ->
            case find_entry_by_owner_monitor(Ref, Entries) of
                {ok, Entry} ->
                    stop_guarded_worker(Entry),
                    poll_guard_loop(remove_entry(Entry, Entries));
                error ->
                    case find_entry_by_worker_monitor(Ref, Entries) of
                        {ok, Entry} -> poll_guard_loop(remove_entry(Entry, Entries));
                        error -> poll_guard_loop(Entries)
                    end
            end
    end.

entry_name({Name, _, _, _, _}) -> Name.

stop_guarded_worker({_Name, _Manager, Worker, _OwnerRef, _WorkerRef}) ->
    stop_worker(Worker).

remove_entry({_Name, _Manager, _Worker, OwnerRef, WorkerRef}, Entries) ->
    erlang:demonitor(OwnerRef, [flush]),
    erlang:demonitor(WorkerRef, [flush]),
    [Entry || Entry = {_, _, _, OR, WR} <- Entries,
        OR =/= OwnerRef orelse WR =/= WorkerRef].

find_entry_by_owner_monitor(Ref, Entries) ->
    find_entry(fun({_, _, _, OwnerRef, _}) -> OwnerRef =:= Ref end, Entries).

find_entry_by_worker_monitor(Ref, Entries) ->
    find_entry(fun({_, _, _, _, WorkerRef}) -> WorkerRef =:= Ref end, Entries).

find_entry(_Predicate, []) -> error;
find_entry(Predicate, [Entry | Rest]) ->
    case Predicate(Entry) of
        true -> {ok, Entry};
        false -> find_entry(Predicate, Rest)
    end.

%% Suspend the linked channel-worker tree before killing any OS processes.
%% This also prevents linked children from opening new ports during cleanup.
stop_worker(Worker) ->
    erlang:unlink(Worker),
    Frozen = freeze_tree(Worker, [self()]),
    lists:foreach(fun({_Pid, _Ref, Ports}) ->
        lists:foreach(fun shell_ffi:kill_port_tree/1, Ports)
    end, Frozen),
    lists:foreach(fun({Pid, _Ref, _Ports}) -> erlang:exit(Pid, kill) end, Frozen),
    lists:foreach(fun({Pid, Ref, _Ports}) ->
        receive {'DOWN', Ref, process, Pid, _} -> ok end
    end, Frozen),
    nil.

freeze_tree(Pid, Seen) ->
    case lists:member(Pid, Seen) of
        true -> [];
        false ->
            Ref = erlang:monitor(process, Pid),
            try
                true = erlang:suspend_process(Pid),
                Links = case erlang:process_info(Pid, links) of
                    {links, Values} -> Values;
                    undefined -> []
                end,
                Ports = [Port || Port <- Links, is_port(Port)],
                Children = [Child || Child <- Links, is_pid(Child), not lists:member(Child, [Pid|Seen])],
                [{Pid, Ref, Ports} | lists:append([freeze_tree(Child, [Pid|Seen]) || Child <- Children])]
            catch
                error:badarg -> [{Pid, Ref, []}]
            end
    end.
