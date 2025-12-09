-module(parser_ffi).
-export([parse_float/1]).

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
