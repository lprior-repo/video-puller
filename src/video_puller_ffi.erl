-module(video_puller_ffi).
-export([priv_directory/1]).

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
