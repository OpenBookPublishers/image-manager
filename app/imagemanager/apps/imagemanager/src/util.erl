-module(util).

-export([upload_dir/0]).
-export([thumbnail_dir/0]).
-export([file_exists/1]).
-export([db_connection/0]).
-export([sql_result_to_map_list/1]).
-export([unique_list/1]).

upload_dir() ->
    case os:getenv("UPLOAD_PATH") of
        false ->
            {ok, Dir} = application:get_env(imagemanager, uploaded_dir),
            Dir;
        S ->
            S
    end.

thumbnail_dir() ->
    case os:getenv("THUMBNAIL_PATH") of
        false ->
            {ok, Dir} = application:get_env(imagemanager, thumbnail_dir),
            Dir;
        S ->
            S
    end.

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {error, enoent} -> true;
        _ -> false
    end.

db_connection() ->
    Hostname = os:getenv("PGHOST"),
    Username = os:getenv("PGUSER"),
    Database = os:getenv("PGNAME"),
    DB_Opts = #{ database => Database },
    {ok, C} = epgsql:connect(Hostname, Username, DB_Opts),
    C.

sql_result_to_map_list({ok, Cols, Rows}) ->
    ColNames = [ ColName || {column, ColName, _, _, _, _, _} <- Cols ],
    Tmp = [ lists:zip(ColNames, tuple_to_list(Row)) || Row <- Rows ],
    [ maps:from_list(R) || R <- Tmp ].

unique_list(LL) ->
    Intermediate = lists:foldl(fun(I, Acc) ->
                                       sets:add_element(I, Acc)
                               end, sets:new(), LL),
    sets:to_list(Intermediate).
