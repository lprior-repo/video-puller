-module(plex_ffi).

-include_lib("xmerl/include/xmerl.hrl").

-export([refresh_library/0, refresh_library_result/1, reserve_scan_gate_at/4, library_videos/0, library_videos_result/0, watched_videos_result/0, cleanup_protected_paths_result/0, managed_section_inventory_result/0, empty_section_trash_result/0, reserve_trash_gate_at/4, parse_managed_section/2, parse_section_paths/1, update_title/2, parse_videos/1, parse_watched_paths/1, parse_active_paths/1, parse_partial_watched_paths/1]).

-define(DEFAULT_BASE_URL, "http://127.0.0.1:32400").
-define(DEFAULT_SECTION_ID, "").

%% The Plex token is read only into memory and is never returned to Gleam or
%% included in an error/log message.
refresh_library() ->
    _ = refresh_library_result(300000),
    nil.

refresh_library_result(CooldownMs) ->
    with_token_result(fun(Token) ->
        Base = base_url(),
        Section = section_id(),
        case reserve_scan_gate(Base, Section, CooldownMs) of
            {ok, false} -> {ok, nil};
            {ok, true} ->
                Url = Base ++ "/library/sections/" ++ Section ++ "/refresh?X-Plex-Token=" ++ quote(Token),
                case request(get, Url) of
                    {ok, _} -> {ok, nil};
                    _ -> {error, <<"local Plex scan request failed">>}
                end;
            {error, _} = Error -> Error
        end
    end, {error, <<"Plex credentials or library section unavailable">>}).

%% Reserve the scan window before making the request. The persistent deadline
%% remains set after request failures, and the token is never part of the key.
reserve_scan_gate(Base, Section, CooldownMs) ->
    reserve_scan_gate_at(Base, Section, erlang:monotonic_time(millisecond), CooldownMs).

%% Exposed for deterministic native tests; production uses monotonic time above.
reserve_scan_gate_at(Base, Section, Now, CooldownMs) ->
    Key = {?MODULE, refresh_gate, Base, Section},
    Lock = {Key, self()},
    Cooldown = max(CooldownMs, 0),
    case global:trans(Lock, fun() ->
        case persistent_term:get(Key, undefined) of
            Until when is_integer(Until), Now < Until -> false;
            _ ->
                persistent_term:put(Key, Now + Cooldown),
                true
        end
    end) of
        true -> {ok, true};
        false -> {ok, false};
        aborted -> {error, <<"Plex scan gate unavailable">>}
    end.

library_videos() ->
    case library_videos_result() of
        {ok, Videos} -> Videos;
        _ -> []
    end.

library_videos_result() ->
    with_token(fun(Token) ->
        Url = base_url() ++ "/library/sections/" ++ section_id() ++ "/all?type=4&X-Plex-Container-Size=100000&X-Plex-Token=" ++ quote(Token),
        case request(get, Url) of
            {ok, Body} -> {ok, parse_videos(Body)};
            _ -> {error, <<"local Plex server unavailable">>}
        end
    end, {error, <<"Plex credentials or library section unavailable">>}).

%% Return every exact media path Plex marks as watched (viewCount > 0). A
%% configured-but-unavailable server is an error; missing credentials/section
%% is a disabled no-op so cleanup never mistakes a failed request for empty.
watched_videos_result() ->
    with_token_result(fun(Token) ->
        LibraryUrl = base_url() ++ "/library/sections/" ++ section_id() ++
            "/all?type=4&X-Plex-Container-Size=100000&X-Plex-Token=" ++ quote(Token),
        SessionsUrl = base_url() ++ "/status/sessions?X-Plex-Token=" ++ quote(Token),
        case {request(get, LibraryUrl), request(get, SessionsUrl)} of
            {{ok, LibraryBody}, {ok, SessionsBody}} ->
                case {parse_watched_paths_result(LibraryBody), parse_protected_active_paths_result(SessionsBody)} of
                    {{ok, WatchedPaths}, {ok, ActivePaths}} ->
                        {ok, {true, {WatchedPaths, ActivePaths}}};
                    _ -> {error, <<"local Plex response could not be parsed">>}
                end;
            _ -> {error, <<"local Plex server unavailable">>}
        end
    end, {ok, {false, {[], []}}}).

%% Fail closed unless the section response is complete and every candidate and
%% active session Part has a usable filepath. The token remains local to FFI.
cleanup_protected_paths_result() ->
    with_token_result(fun(Token) ->
        Base = base_url(),
        Section = section_id(),
        LibraryUrl = Base ++ "/library/sections/" ++ Section ++
            "/all?type=4&X-Plex-Container-Start=0&X-Plex-Container-Size=100000&X-Plex-Token=" ++ quote(Token),
        SessionsUrl = Base ++ "/status/sessions?X-Plex-Token=" ++ quote(Token),
        case {request(get, LibraryUrl), request(get, SessionsUrl)} of
            {{ok, LibraryBody}, {ok, SessionsBody}} ->
                case {parse_section_paths(LibraryBody),
                        parse_partial_watched_paths_result(LibraryBody),
                        parse_protected_active_paths_result(SessionsBody)} of
                    {{ok, _AllPaths}, {ok, PartialPaths}, {ok, ActivePaths}} ->
                        {ok, {true, lists:usort(PartialPaths ++ ActivePaths)}};
                    _ -> {error, <<"local Plex response could not be parsed">>}
                end;
            _ -> {error, <<"local Plex server unavailable">>}
        end
    end, {ok, {false, []}}).

%% Return strict inventory for the configured section. No path or scanner
%% defaults are inferred: malformed or incomplete responses fail closed.
managed_section_inventory_result() ->
    with_token_result(fun(Token) ->
        Section = section_id(),
        SectionsUrl = base_url() ++ "/library/sections?X-Plex-Token=" ++ quote(Token),
        ItemsUrl = base_url() ++ "/library/sections/" ++ Section ++
            "/all?type=4&X-Plex-Container-Start=0&X-Plex-Container-Size=100000&X-Plex-Token=" ++ quote(Token),
        case request(get, SectionsUrl) of
            {ok, SectionsBody} ->
                case parse_managed_section(SectionsBody, Section) of
                    {ok, {Type, Refreshing, Locations}} ->
                        case request(get, ItemsUrl) of
                            {ok, ItemsBody} ->
                                case parse_section_paths(ItemsBody) of
                                    {ok, Paths} -> {ok, {Type, Refreshing, Locations, Paths}};
                                    {error, _} -> {error, <<"Plex section items could not be parsed">>}
                                end;
                            _ -> {error, <<"Plex section items are unavailable">>}
                        end;
                    {error, _} = Error -> Error
                end;
            _ -> {error, <<"Plex sections are unavailable">>}
        end
    end, {error, <<"Plex credentials or library section unavailable">>}).

%% This endpoint is intentionally section-scoped. A separate persistent-term
%% gate prevents stale trash listings from causing repeated destructive PUTs.
empty_section_trash_result() ->
    with_token_result(fun(Token) ->
        Base = base_url(),
        Section = section_id(),
        case reserve_trash_gate(Base, Section, 300000) of
            {ok, false} -> {ok, false};
            {ok, true} ->
                Url = Base ++ "/library/sections/" ++ Section ++
                    "/emptyTrash?X-Plex-Token=" ++ quote(Token),
                case request(put, Url) of
                    {ok, _} -> {ok, true};
                    _ -> {error, <<"Plex section trash cleanup failed">>}
                end;
            {error, _} = Error -> Error
        end
    end, {error, <<"Plex credentials or library section unavailable">>}).

reserve_trash_gate(Base, Section, CooldownMs) ->
    reserve_trash_gate_at(Base, Section, erlang:monotonic_time(millisecond), CooldownMs).

reserve_trash_gate_at(Base, Section, Now, CooldownMs) ->
    Key = {?MODULE, trash_gate, Base, Section},
    Lock = {Key, self()},
    Cooldown = max(CooldownMs, 0),
    case global:trans(Lock, fun() ->
        case persistent_term:get(Key, undefined) of
            Until when is_integer(Until), Now < Until -> false;
            _ ->
                persistent_term:put(Key, Now + Cooldown),
                true
        end
    end) of
        true -> {ok, true};
        false -> {ok, false};
        aborted -> {error, <<"Plex trash cleanup gate unavailable">>}
    end.

parse_managed_section(Body, SectionId) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        case Xml of
            #xmlElement{name = 'MediaContainer'} ->
                Directories = xmerl_xpath:string("//Directory", Xml),
                SectionKey = section_key(SectionId),
                Matches = [Directory || #xmlElement{} = Directory <- Directories,
                    maps:get("key", attrs(Directory#xmlElement.attributes), "") =:= SectionKey],
                case Matches of
                    [Directory] ->
                        Attrs = attrs(Directory#xmlElement.attributes),
                        Type = maps:get("type", Attrs, ""),
                        Refreshing = parse_refreshing(maps:get("refreshing", Attrs, "")),
                        Locations = xmerl_xpath:string(".//Location", Directory),
                        Paths = [maps:get("path", attrs(Location#xmlElement.attributes), "") ||
                            #xmlElement{} = Location <- Locations],
                        case Type =/= [] andalso Refreshing =/= error andalso
                                length(Paths) =:= length(Locations) andalso
                                not lists:member([], Paths) of
                            true ->
                                {ok, {unicode:characters_to_binary(Type), Refreshing,
                                    [unicode:characters_to_binary(Path) || Path <- Paths]}};
                            false -> {error, <<"invalid Plex section metadata">>}
                        end;
                    _ -> {error, <<"configured Plex section was not found uniquely">>}
                end;
            _ -> {error, <<"invalid Plex sections XML">>}
        end
    catch
        _:_ -> {error, <<"invalid Plex sections XML">>}
    end.

parse_section_paths(Body) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        case Xml of
            #xmlElement{name = 'MediaContainer'} ->
                Videos = xmerl_xpath:string("//Video", Xml),
                case complete_container(Xml, Videos) of
                    true -> case [video_part_paths(Video) || #xmlElement{} = Video <- Videos] of
                        PathsByVideo ->
                            case lists:all(fun(Paths) -> Paths =/= error end, PathsByVideo) of
                                true ->
                                    Paths = lists:append(PathsByVideo),
                                    {ok, lists:usort([unicode:characters_to_binary(Path) || Path <- Paths])};
                                false -> {error, <<"invalid Plex section media paths">>}
                            end
                    end;
                    false -> {error, <<"incomplete Plex section media response">>}
                end;
            _ -> {error, <<"invalid Plex section media XML">>}
        end
    catch
        _:_ -> {error, <<"invalid Plex section media XML">>}
    end.

parse_refreshing("0") -> false;
parse_refreshing("false") -> false;
parse_refreshing("1") -> true;
parse_refreshing("true") -> true;
parse_refreshing(_) -> error.

complete_container(Xml, Videos) ->
    try
        Attrs = attrs(Xml#xmlElement.attributes),
        Size = list_to_integer(maps:get("size", Attrs)),
        TotalSize = list_to_integer(maps:get("totalSize", Attrs)),
        Offset = list_to_integer(maps:get("offset", Attrs)),
        Size =:= length(Videos) andalso TotalSize =:= Size andalso Offset =:= 0
    catch
        _:_ -> false
    end.

video_part_paths(Video) ->
    Parts = xmerl_xpath:string(".//Part", Video),
    case Parts =/= [] andalso lists:all(fun
        (#xmlElement{attributes = Attributes}) -> maps:get("file", attrs(Attributes), "") =/= [];
        (_) -> false
    end, Parts) of
        true -> [maps:get("file", attrs(Part#xmlElement.attributes)) || Part <- Parts];
        false -> error
    end.

section_key(SectionId) when is_binary(SectionId) -> binary_to_list(SectionId);
section_key(SectionId) -> SectionId.

update_title(RatingKey, Title) ->
    with_token(fun(Token) ->
        Url = base_url() ++ "/library/metadata/" ++ binary_to_list(RatingKey) ++
            "?title=" ++ quote(unicode:characters_to_list(Title)) ++
            "&title.locked=1&X-Plex-Token=" ++ quote(Token),
        _ = request(put, Url),
        ok
    end),
    nil.

with_token(Fun) ->
    with_token(Fun, nil).

with_token(Fun, Default) ->
    try
        Token = string:trim(os:cmd("defaults read com.plexapp.plexmediaserver PlexOnlineToken 2>/dev/null")),
        case {Token, section_id()} of
            {[], _} -> Default;
            {_, []} -> Default;
            _ -> Fun(Token)
        end
    catch
        _:_ -> Default
    end.

request(Method, Url) ->
    try
        _ = application:ensure_all_started(inets),
        Request = case Method of
            get -> {Url, []};
            put -> {Url, [], "application/x-www-form-urlencoded", ""}
        end,
        case httpc:request(Method, Request, [{timeout, 5000}, {connect_timeout, 3000}], [{body_format, binary}]) of
            {ok, {{_Version, Code, _Reason}, _Headers, Body}} when Code >= 200, Code < 300 -> {ok, Body};
            {ok, {{_Version, Code, _Reason}, _Headers, _Body}} -> {error, {http_status, Code}};
            Error -> Error
        end
    catch
        _:_ -> {error, unavailable}
    end.

parse_videos(Body) ->
    try
        %% xmerl_scan expects UTF-8 bytes and decodes them into Unicode
        %% character lists for attribute values.
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        Videos = [Video || #xmlElement{name = 'Video'} = Video <- xmerl_xpath:string("//Video", Xml)],
        to_video_rows(Videos)
    catch
        _:_ -> []
    end.

parse_watched_paths(Body) ->
    case parse_watched_paths_result(Body) of
        {ok, Paths} -> Paths;
        error -> []
    end.

parse_watched_paths_result(Body) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        Videos = [Video || #xmlElement{name = 'Video'} = Video <- xmerl_xpath:string("//Video", Xml)],
        {ok, lists:usort(lists:filtermap(fun(Video) ->
            Attrs = attrs(Video#xmlElement.attributes),
            case {watched_count(maps:get("viewCount", Attrs, "0")), first_part_file(Video)} of
                {true, Path} when Path =/= [] -> {true, unicode:characters_to_binary(Path)};
                _ -> false
            end
        end, Videos))}
    catch
        _:_ -> error
    end.

parse_active_paths(Body) ->
    case parse_protected_active_paths_result(Body) of
        {ok, Paths} -> {ok, Paths};
        error -> {error, <<"invalid Plex sessions XML">>}
    end.

parse_partial_watched_paths(Body) ->
    case parse_partial_watched_paths_result(Body) of
        {ok, Paths} -> {ok, Paths};
        error -> {error, <<"invalid or incomplete Plex section XML">>}
    end.

parse_partial_watched_paths_result(Body) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        case Xml of
            #xmlElement{name = 'MediaContainer'} ->
                Videos = xmerl_xpath:string("//Video", Xml),
                case complete_container(Xml, Videos) of
                    false -> error;
                    true ->
                        case lists:foldl(fun
                            (#xmlElement{} = Video, {ok, Paths}) ->
                                Attrs = attrs(Video#xmlElement.attributes),
                                %% Plex omits both fields for an unwatched item.
                                case {strict_integer(maps:get("viewCount", Attrs, "0")),
                                        strict_integer(maps:get("viewOffset", Attrs, "0")),
                                        video_part_paths(Video)} of
                                    {{ok, 0}, {ok, Offset}, PartPaths} when Offset > 0, PartPaths =/= error ->
                                        {ok, PartPaths ++ Paths};
                                    {{ok, Count}, {ok, Offset}, PartPaths}
                                            when Count >= 0, Offset >= 0, PartPaths =/= error ->
                                        {ok, Paths};
                                    _ -> error
                                end;
                            (_, error) -> error
                        end, {ok, []}, Videos) of
                            {ok, Paths} -> {ok, lists:usort([unicode:characters_to_binary(Path) || Path <- Paths])};
                            error -> error
                        end
                end;
            _ -> error
        end
    catch
        _:_ -> error
    end.

strict_integer(Value) when is_list(Value) ->
    case string:to_integer(Value) of
        {Integer, []} -> {ok, Integer};
        _ -> error
    end;
strict_integer(_) -> error.

parse_protected_active_paths_result(Body) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{quiet, true}, {encoding, "utf-8"}]),
        case Xml of
            #xmlElement{name = 'MediaContainer'} ->
                Parts = xmerl_xpath:string("//Part", Xml),
                Videos = xmerl_xpath:string("//Video", Xml),
                case lists:all(fun(Video) -> video_part_paths(Video) =/= error end, Videos)
                  andalso lists:all(fun
                    (#xmlElement{attributes = Attributes}) -> maps:get("file", attrs(Attributes), "") =/= [];
                    (_) -> false
                end, Parts) of
                    true ->
                        {ok, lists:usort([unicode:characters_to_binary(maps:get("file", attrs(Part#xmlElement.attributes))) ||
                            #xmlElement{} = Part <- Parts])};
                    false -> error
                end;
            _ -> error
        end
    catch
        _:_ -> error
    end.

watched_count(Value) ->
    case string:to_integer(Value) of
        {Count, []} when Count > 0 -> true;
        _ -> false
    end.

with_token_result(Fun, Default) ->
    try
        Token = string:trim(os:cmd("defaults read com.plexapp.plexmediaserver PlexOnlineToken 2>/dev/null")),
        case {Token, section_id()} of
            {[], _} -> Default;
            {_, []} -> Default;
            _ -> Fun(Token)
        end
    catch
        _:_ -> Default
    end.

to_video_rows(Videos) ->
    lists:filtermap(fun(Video) ->
        Attrs = attrs(Video#xmlElement.attributes),
        case {maps:get("ratingKey", Attrs, ""), first_part_file(Video), maps:get("title", Attrs, "")} of
            {[], _, _} -> false;
            {_, [], _} -> false;
            {_, _, []} -> false;
            {Key, Path, Title} ->
                {true, {
                    unicode:characters_to_binary(Key),
                    unicode:characters_to_binary(Path),
                    unicode:characters_to_binary(Title)
                }}
        end
    end, Videos).

attrs(Attributes) ->
    maps:from_list([{atom_to_list(A#xmlAttribute.name), A#xmlAttribute.value} || A <- Attributes]).

first_part_file(Video) ->
    case xmerl_xpath:string(".//Part", Video) of
        [#xmlElement{attributes = Attributes} | _] -> maps:get("file", attrs(Attributes), "");
        _ -> ""
    end.

quote(Value) ->
    uri_string:quote(Value, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~").

base_url() ->
    strip_trailing_slash(string:trim(os:getenv("PLEX_URL", ?DEFAULT_BASE_URL))).

section_id() ->
    string:trim(os:getenv("PLEX_SECTION_ID", ?DEFAULT_SECTION_ID)).

strip_trailing_slash([]) -> [];
strip_trailing_slash(Value) ->
    case lists:last(Value) of
        $/ -> strip_trailing_slash(lists:droplast(Value));
        _ -> Value
    end.
