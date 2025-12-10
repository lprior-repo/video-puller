-module(parser_ffi).
-export([parse_float/1, parse_video_metadata/1]).

parse_float(Str) ->
    ListStr = binary_to_list(Str),
    try
        Float = list_to_float(ListStr),
        {ok, Float}
    catch
        error:badarg ->
            % If not a float, try as integer and convert to float
            try
                Int = list_to_integer(ListStr),
                {ok, float(Int)}
            catch
                error:_ -> {error, nil}
            end
    end.

parse_video_metadata(JsonString) ->
    try
        % Use json:decode from OTP 27+
        Json = json:decode(JsonString),

        % Get required fields
        Title = maps:get(<<"title">>, Json),
        Thumbnail = maps:get(<<"thumbnail">>, Json),
        Duration = maps:get(<<"duration">>, Json),
        Uploader = maps:get(<<"uploader">>, Json),

        % Get optional view_count field
        % Gleam Option is represented as {some, Val} or none atom
        ViewCount = case maps:get(<<"view_count">>, Json, nil) of
            nil -> none;
            null -> none;
            Val when is_integer(Val) -> {some, Val};
            _ -> none
        end,

        {ok, {Title, Thumbnail, Duration, Uploader, ViewCount}}
    catch
        error:{badkey, _Key} ->
            {error, <<"Missing required field in JSON">>};
        error:badarg ->
            {error, <<"Invalid JSON format">>};
        error:undef ->
            {error, <<"JSON library not available">>};
        _:_ ->
            {error, <<"Failed to decode video metadata">>}
    end.
