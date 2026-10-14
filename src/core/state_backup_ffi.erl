-module(state_backup_ffi).
-include_lib("kernel/include/file.hrl").
-export([day_key/1, prepare/3, copy_file/2, sync_file/1, publish/2, abort/1, prune/1]).

-define(MARKER, ".state-backup-marker").
-define(FILES, ["database.sqlite", "archive.txt", "channels.txt", "desired-channels.txt", "channel_ids.txt", ?MARKER]).

day_key(Seconds) ->
    {{Year, Month, Day}, _} = calendar:system_time_to_universal_time(Seconds, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])).

prepare(DataRoot0, BackupRoot0, Day0) ->
    DataRoot = filename:absname(binary_to_list(DataRoot0)),
    BackupRoot = filename:absname(binary_to_list(BackupRoot0)),
    Day = binary_to_list(Day0),
    case outside(DataRoot, BackupRoot) of
        false -> {error, <<"backup directory must be outside data directory">>};
        true -> case {safe_directory(DataRoot, false), safe_directory(BackupRoot, true)} of
            {ok, ok} -> prepare_snapshot(BackupRoot, Day);
            _ -> {error, <<"backup paths must be safe real directories">>}
        end
    end.

prepare_snapshot(Root, Day) ->
    Final = filename:join(Root, Day),
    Stage = filename:join(Root, Day ++ ".partial"),
    case {file:read_link_info(Final), file:read_link_info(Stage)} of
        {{ok, #file_info{type = directory}}, _} ->
            case marker(Final, "complete") of
                true -> {ok, {false, <<>>, list_to_binary(Final)}};
                false -> {error, <<"snapshot destination already exists">>}
            end;
        {{error, enoent}, {ok, #file_info{type = directory}}} ->
            case marker(Stage, "staging") of
                true -> case remove_owned(Stage) of ok -> make_stage(Stage, Final); Error -> Error end;
                false -> {error, <<"unowned partial snapshot exists">>}
            end;
        {{error, enoent}, {error, enoent}} -> make_stage(Stage, Final);
        _ -> {error, <<"unsafe snapshot path exists">>}
    end.

make_stage(Stage, Final) ->
    case file:make_dir(Stage) of
        ok ->
            case file:change_mode(Stage, 8#700) of
                ok -> case write_marker(Stage, "staging") of
                ok -> {ok, {true, list_to_binary(Stage), list_to_binary(Final)}};
                Error -> _ = file:del_dir(Stage), Error
                end;
                {error, Why} -> _ = file:del_dir(Stage), {error, reason(Why)}
            end;
        {error, Why} -> {error, reason(Why)}
    end.

publish(Stage0, Final0) ->
    Stage = binary_to_list(Stage0),
    Final = binary_to_list(Final0),
    case {marker(Stage, "staging"), file:read_link_info(Final)} of
        {true, {error, enoent}} ->
            case write_marker(Stage, "complete") of
                ok -> case file:rename(Stage, Final) of ok -> {ok, nil}; {error, Why} -> {error, reason(Why)} end;
                Error -> Error
            end;
        _ -> {error, <<"snapshot cannot be published safely">>}
    end.

copy_file(Source0, Dest0) ->
    Source = binary_to_list(Source0),
    Dest = binary_to_list(Dest0),
    case file:read_link_info(Source) of
        {error, enoent} -> {ok, false};
        {ok, #file_info{type = regular}} ->
            case safe_directory(filename:dirname(Source), false) of
                ok ->
                    case file:copy(Source, Dest) of
                        {ok, _} ->
                            case sync_file(Dest0) of
                                {ok, nil} -> {ok, true};
                                Error -> Error
                            end;
                        {error, Why} -> {error, reason(Why)}
                    end;
                _ -> {error, <<"backup source path is unsafe">>}
            end;
        _ -> {error, <<"backup source is not a regular file">>}
    end.

sync_file(Path0) ->
    Path = binary_to_list(Path0),
    case file:change_mode(Path, 8#600) of
      ok -> case file:open(Path, [read, raw, binary]) of
        {ok, Io} ->
            Result = file:sync(Io),
            _ = file:close(Io),
            case Result of ok -> {ok, nil}; {error, Why} -> {error, reason(Why)} end;
        {error, Why} -> {error, reason(Why)}
      end;
      {error, Why} -> {error, reason(Why)}
    end.

abort(Stage0) ->
    Stage = binary_to_list(Stage0),
    case filename:extension(Stage) =:= ".partial" andalso
            (marker(Stage, "staging") orelse marker(Stage, "complete")) of
        true -> as_gleam_result(remove_owned(Stage));
        false -> {error, <<"refusing unowned staging path">>}
    end.

prune(Root0) ->
    Root = binary_to_list(Root0),
    case file:list_dir(Root) of
        {ok, Names} ->
            Snapshots = lists:sort([{Name, filename:join(Root, Name)} || Name <- Names,
                valid_day(Name), marker(filename:join(Root, Name), "complete")]),
            Old = lists:sublist(Snapshots, max(0, length(Snapshots) - 3)),
            as_gleam_result(remove_old(Old));
        {error, Why} -> {error, reason(Why)}
    end.

remove_old([]) -> ok;
remove_old([{_Name, Path} | Rest]) ->
    case remove_owned(Path) of ok -> remove_old(Rest); Error -> Error end.

remove_owned(Path) ->
    case {file:read_link_info(Path), marker(Path, "staging") orelse marker(Path, "complete")} of
        {{ok, #file_info{type = directory}}, true} ->
            case check_known(Path) of
              ok -> case delete_known(Path, ?FILES) of
                ok -> case file:del_dir(Path) of ok -> ok; {error, Why} -> {error, reason(Why)} end;
                Error -> Error
              end;
              Error -> Error
            end;
        _ -> {error, <<"refusing to remove unowned backup directory">>}
    end.

delete_known(_Path, []) -> ok;
delete_known(Path, [Name | Rest]) ->
    File = filename:join(Path, Name),
    case file:read_link_info(File) of
        {error, enoent} -> delete_known(Path, Rest);
        {ok, #file_info{type = regular}} ->
            case file:delete(File) of ok -> delete_known(Path, Rest); {error, Why} -> {error, reason(Why)} end;
        _ -> {error, <<"refusing to delete unexpected backup entry">>}
    end.

write_marker(Path, Value) ->
    Marker = filename:join(Path, ?MARKER),
    case file:write_file(Marker, list_to_binary(Value ++ "\n"), [raw, binary, sync]) of
        ok -> file:change_mode(Marker, 8#600);
        Error -> Error
    end.

marker(Path, Expected) ->
    Marker = filename:join(Path, ?MARKER),
    case {file:read_link_info(Path), file:read_link_info(Marker)} of
        {{ok, #file_info{type = directory}}, {ok, #file_info{type = regular}}} ->
            case file:read_file(Marker) of {ok, Content} -> Content =:= list_to_binary(Expected ++ "\n"); _ -> false end;
        _ -> false
    end.

safe_directory(Path, Create) -> safe_components(filename:split(Path), [], Create).
safe_components([], _Built, _Create) -> ok;
safe_components([Part | Rest], [], Create) when Part =:= "/" -> safe_components(Rest, [Part], Create);
safe_components([Part | _Rest], _Built, _Create) when Part =:= "."; Part =:= ".." ->
    {error, <<"dot path components are not allowed">>};
safe_components([Part | Rest], Built, Create) ->
    Current = filename:join(Built ++ [Part]),
    case file:read_link_info(Current) of
        {ok, #file_info{type = directory}} -> safe_components(Rest, Built ++ [Part], Create);
        {error, enoent} when Create ->
            case file:make_dir(Current) of
                ok -> safe_components(Rest, Built ++ [Part], Create);
                {error, eexist} -> safe_components([Part | Rest], Built, Create);
                {error, Why} -> {error, reason(Why)}
            end;
        _ -> {error, <<"unsafe directory component">>}
    end.

outside(DataRoot, BackupRoot) ->
    Prefix = case lists:last(DataRoot) of $/ -> DataRoot; _ -> DataRoot ++ "/" end,
    BackupRoot =/= DataRoot andalso not lists:prefix(Prefix, BackupRoot).

check_known(Path) ->
    case file:list_dir(Path) of
        {ok, Names} ->
            case lists:all(fun(Name) -> lists:member(Name, ?FILES) andalso
                case file:read_link_info(filename:join(Path, Name)) of
                    {ok, #file_info{type = regular}} -> true;
                    _ -> false
                end
            end, Names) of
                true -> ok;
                false -> {error, <<"backup directory contains unknown entries">>}
            end;
        {error, Why} -> {error, reason(Why)}
    end.

valid_day([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2]) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, [Y1,Y2,Y3,Y4,M1,M2,D1,D2]);
valid_day(_) -> false.

reason(Why) -> iolist_to_binary(io_lib:format("~p", [Why])).

as_gleam_result(ok) -> {ok, nil};
as_gleam_result({error, _} = Error) -> Error.
