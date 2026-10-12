-module(download_history_ffi).
-export([remember/2, remember_many/2, read_archive/1]).

remember(DataRoot, VideoId) ->
    Path = archive_path(DataRoot),
    Lock = {{?MODULE, Path}, self()},
    case global:trans(Lock, fun() ->
        case filelib:ensure_dir(Path) of
            ok -> append_once(Path, VideoId);
            {error, Reason} -> {error, format_error(Reason)}
        end
    end) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason};
        aborted -> {error, <<"download history lock unavailable">>}
    end.

remember_many(DataRoot, VideoIds) ->
    Path = archive_path(DataRoot),
    Lock = {{?MODULE, Path}, self()},
    case global:trans(Lock, fun() ->
        case filelib:ensure_dir(Path) of
            ok -> append_missing(Path, VideoIds);
            {error, Reason} -> {error, format_error(Reason)}
        end
    end) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason};
        aborted -> {error, <<"download history lock unavailable">>}
    end.

read_archive(DataRoot) ->
    Path = archive_path(DataRoot),
    Lock = {{?MODULE, Path}, self()},
    case global:trans(Lock, fun() ->
        case file:read_file(Path) of
            {ok, Contents} -> {ok, Contents};
            {error, enoent} -> {ok, <<>>};
            {error, Reason} -> {error, format_error(Reason)}
        end
    end) of
        {ok, _} = Result -> Result;
        {error, _} = Error -> Error;
        aborted -> {error, <<"download history lock unavailable">>}
    end.

append_once(Path, VideoId) ->
    Line = <<"youtube ", (unicode:characters_to_binary(VideoId))/binary, "\n">>,
    case file:read_file(Path) of
        {ok, Contents} ->
            case archive_contains(Contents, VideoId) of
                true -> ok;
                false -> append_line(Path, Line)
            end;
        {error, enoent} -> append_line(Path, Line);
        {error, Reason} -> {error, format_error(Reason)}
    end.

append_missing(_Path, []) -> ok;
append_missing(Path, VideoIds) ->
    case file:read_file(Path) of
        {ok, Contents} -> write_missing(Path, Contents, VideoIds);
        {error, enoent} -> write_missing(Path, <<>>, VideoIds);
        {error, Reason} -> {error, format_error(Reason)}
    end.

write_missing(Path, Existing, VideoIds) ->
    Missing = [
        <<"youtube ", (unicode:characters_to_binary(Id))/binary, "\n">>
        || Id <- VideoIds,
           not archive_contains(Existing, Id)
    ],
    case Missing of
        [] -> ok;
        _ -> append_line(Path, iolist_to_binary(Missing))
    end.

append_line(Path, Line) ->
    case file:open(Path, [append, raw, binary]) of
        {ok, File} ->
            Result = case file:write(File, Line) of
                ok -> file:sync(File);
                Error -> Error
            end,
            _ = file:close(File),
            case Result of
                ok -> ok;
                {error, Reason} -> {error, format_error(Reason)}
            end;
        {error, Reason} -> {error, format_error(Reason)}
    end.

archive_contains(Contents, VideoId) ->
    Id = unicode:characters_to_binary(VideoId),
    lists:any(fun(Line0) ->
        Line = trim_line(Line0),
        Line =:= <<"youtube ", Id/binary>>
    end, binary:split(Contents, <<"\n">>, [global])).

trim_line(Line) ->
    case byte_size(Line) > 0 andalso binary:last(Line) =:= $\r of
        true -> binary:part(Line, 0, byte_size(Line) - 1);
        false -> Line
    end.

archive_path(DataRoot) ->
    Root = unicode:characters_to_list(DataRoot),
    filename:join([Root, "ytdl-sub", "download-history", "archive.txt"]).

format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).
