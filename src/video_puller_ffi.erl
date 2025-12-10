-module(video_puller_ffi).
-export([priv_directory/1, getenv/1]).

priv_directory(AppName) ->
    BinAppName = binary_to_atom(AppName),
    case code:priv_dir(BinAppName) of
        {error, _} ->
            % Return empty string on error (will fall back to dev path)
            <<"">>;
        PrivDir when is_list(PrivDir) ->
            % Convert list to binary string
            list_to_binary(PrivDir)
    end.

getenv(Key) when is_binary(Key) ->
    case os:getenv(binary_to_list(Key)) of
        false ->
            {error, nil};
        Value when is_list(Value) ->
            {ok, list_to_binary(Value)}
    end.
