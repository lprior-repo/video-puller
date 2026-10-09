-module(subscription_manager_ffi).

-export([stop_worker/1]).

%% Freeze the worker before inspecting its ports so it cannot open another
%% downloader between inspection and termination. Killing a BEAM process alone
%% only closes its ports, which can leave ytdl-sub and ffmpeg alive on the OS.
stop_worker(Worker) ->
    Ref = erlang:monitor(process, Worker),
    try
        true = erlang:suspend_process(Worker),
        case erlang:process_info(Worker, links) of
            {links, Links} ->
                lists:foreach(
                    fun(Port) when is_port(Port) ->
                        shell_ffi:kill_port_tree(Port);
                       (_) -> ok
                    end,
                    Links
                );
            undefined -> ok
        end
    catch
        error:badarg -> ok
    after
        erlang:exit(Worker, kill)
    end,
    %% Do not free the poll slot until its worker has actually terminated.
    receive
        {'DOWN', Ref, process, Worker, _Reason} -> nil
    end.
