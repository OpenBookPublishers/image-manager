% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(img_mgr_serv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([receive_upload/5, set_image_details/6, remove_image/1, all_images/0,
         all_chapters/0, all_licences/0, fail_image/2, update_image_details/4,
         set_image_rank/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

receive_upload(Filename, Hash, Data, MimeType, Metadata) ->
    gen_server:call(?SERVER, {upload, Filename, Hash, Data, MimeType, Metadata}).

set_image_details(Hash, Format, Resolution, Res_Category, Filename, Metadata) ->
    gen_server:call(?SERVER, {set_details, Hash, Format, Resolution,
                              Res_Category, Filename, Metadata}).

update_image_details(Hash, Chapter_Uuid, Caption, Licence_Status) ->
    gen_server:call(?SERVER, {update_details, Hash, Chapter_Uuid, Caption, Licence_Status}).

fail_image(Hash, Reason) ->
    gen_server:call(?SERVER, {failed, Hash, Reason}).

remove_image(Hash) ->
    gen_server:call(?SERVER, {remove_image, Hash}).

all_images() ->
    gen_server:call(?SERVER, all_images).

all_chapters() ->
    gen_server:call(?SERVER, all_chapters).

all_licences() ->
    gen_server:call(?SERVER, all_licences).

set_image_rank(Hash, NewRank) ->
    gen_server:call(?SERVER, {set_image_rank, Hash, NewRank}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call({upload, Filename, Hash, Data, MimeType, Metadata}, _From, State) ->
    Dir = util:upload_dir(),
    Path = filename:join(Dir, Hash),
    case util:file_exists(Path) of
       true -> Reply = create_file(Hash, Path, Data, Filename, MimeType,
                                  Metadata),
               {reply, Reply, State};
       false -> dupe(Hash),
                {reply, {ok, exists}, State}
    end;

handle_call({set_details, Hash, Format, Resolution, Res_Category, Filename, Metadata},
            _From, State) ->
    save_image(Hash, Format, Resolution, Res_Category, Filename, Metadata),
    Reply = ok,
    {reply, Reply, State};

handle_call({update_details, Hash, Chapter_Uuid, Caption, Licence_Status},
            _From, State) ->
    do_update_image_details(Hash, Chapter_Uuid, Caption, Licence_Status),
    Reply = ok,
    {reply, Reply, State};

handle_call({failed, Hash, Reason}, _From, State) ->
    do_fail_image(Hash, Reason),
    Reply = {ok, Reason},
    {reply, Reply, State};

handle_call({remove_image, Hash}, _From, State) ->
    do_remove_image(Hash),
    Reply = ok,
    {reply, Reply, State};

handle_call(all_images, _From, State) ->
    Results = all_image_details(),
    Reply = {ok, Results},
    {reply, Reply, State};

handle_call(all_chapters, _From, State) ->
    Results = all_chapter_details(),
    Reply = {ok, Results},
    {reply, Reply, State};

handle_call(all_licences, _From, State) ->
    Results = all_licence_details(),
    Reply = {ok, Results},
    {reply, Reply, State};

handle_call({set_image_rank, Hash, NewRank}, _From, State) ->
    Results = do_set_image_rank(Hash, NewRank),
    Reply = {ok, Results},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = {error, unrecognised_message},
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% ideally we'd plumb the filename through to here
dupe(Hash) ->
    lager:info("Duplicate file: ~p", [Hash]),
    img_mgr_proto:notice("Duplicate file uploaded."),
    ok.

with_db_connection(F) ->
    C = util:db_connection(),
    try
        F(C)
    after
        epgsql:close(C)
    end
        %, flush()
        .

with_transaction(F) ->
    Opts = #{ reraise => true },
    C = util:db_connection(),
    try
        epgsql:with_transaction(C, fun(C0) -> F(C0) end, Opts)
    after
        epgsql:close(C)
    end.

% the metadata structure is actually defined in api_h.erl
save_image(Hash, Format, Resolution, Res_Category, Filename, Metadata) ->
    {upload_metadata, Chapter_Uuid} = Metadata,
    %% NB "rank" needs to be last
    Query =
        "INSERT INTO image " ++
        "(hash, image_name, format, resolution, res_category, chapter_uuid, " ++
        "licence_status, placeholder, rank) VALUES " ++ 
        "($1, $2, $3, $4, $5, $6, $7, $8, $9);",

    Caption = "Caption TBD for " ++ binary_to_list(Filename),
    Copyright = "To Be Determined",
    Placeholder = "f",
    Parameters = [Hash,
                  Caption,
                  Format,
                  Resolution,
                  Res_Category,
                  Chapter_Uuid,
                  Copyright,
                  Placeholder
                 ],
    try with_db_connection(
      fun(C) -> save_image_transaction(C, Query, Chapter_Uuid, Parameters) end
    ) of
        {Rank, {ok, _Count}} ->
            img_mgr_proto:update_image(Hash, Format, Resolution, Res_Category,
                                       Filename, Chapter_Uuid, Rank, Copyright),
            ok;
        {_, {error, {_, _, _Code, unique_violation, _Msg, _Details}}} ->
            dupe(Hash)
    catch
        _ -> lager:info("Caught SQL error"),
             failed
        % hopefully this will let other errors through
    end.


save_image_transaction(C, Query, Chapter_Uuid, Parameters) ->
    Sql = "SELECT MAX(rank) + 1 AS max_rank FROM image WHERE chapter_uuid = $1;",
    Rank = case epgsql:equery(C, Sql, [Chapter_Uuid]) of
               {ok, _Cols, [{null}]} -> 0;
               {ok, _Cols, [{R}]} -> R
           end,
    Parameters2 = lists:append(Parameters, [Rank]),
    {Rank, epgsql:equery(C, Query, Parameters2)}.


do_fail_image(Hash, Reason) ->
    lager:info("Failed image (~p): ~p", [Hash, Reason]),
    Notice = "Image upload failed: " ++ Reason,
    img_mgr_proto:notice(Notice),
    do_remove_image(Hash).


do_remove_image(Hash) ->
    img_mgr_proto:remove_image(Hash),
    [ file:delete(filename:join(Dir, Hash)) || Dir <- [util:upload_dir(),
                                                       util:thumbnail_dir()] ],
    remove_image_from_db(Hash).

% TODO: the ranks after this image should be consolidated
%
% get the chapter and rank of the image to be deleted
% get the max rank of the images in its chapter
% if the max rank > rank of image to be deleted,
%   then demote images (deleted + 1, max_rank)
remove_image_from_db(Hash) ->
    Stmt1 = "SELECT chapter_uuid, rank FROM image WHERE hash = $1;",
    Stmt2 = "SELECT MAX(rank) FROM image WHERE chapter_uuid = $1;",
    Parameters = [Hash],
    ok = with_db_connection(fun(C) ->
         case epgsql:equery(C, Stmt1, [Hash]) of
             {ok, _, []} -> ok; % image wasn't inserted in the first place
             {ok, _, [{Chapter_Uuid, Rank}]} ->
                 {ok, _, [{MR}]} = epgsql:equery(C, Stmt2, [Chapter_Uuid]),
                 epgsql:equery(C, "DELETE FROM image WHERE hash = $1;", Parameters),
                 case MR of
                     null -> ok;
                     Max_Rank when Max_Rank > Rank ->
                         do_promote_images(C, Chapter_Uuid, Max_Rank,
                                           Rank, Max_Rank + 1000),
                         propagate_rank_updates(Rank, Max_Rank, Chapter_Uuid),
                         ok;
                     Max_Rank when Max_Rank == Rank -> ok;
                     _ -> lager:info("Unmatched max_rank: ~p ~p", [MR, Rank]),
                          ok
                 end
         end
    end).

create_file(Hash, Path, Data, Filename, _MimeType, Metadata) ->
    ok = file:write_file(Path, Data),
    q_img_analyser:submit({Hash, Filename, Metadata}),
    img_mgr_proto:create_file(Hash),
    {ok, created}.

all_image_details() ->
    Results = with_db_connection(
                fun(C) -> epgsql:equery(C, "SELECT * FROM image_chapter order by rank;", []) end
    ),
    util:sql_result_to_map_list(Results).

all_chapter_details() ->
    Results = with_db_connection(
                fun(C) -> epgsql:equery(C, "SELECT * from chapter ORDER BY ordinal;", []) end
    ),
    util:sql_result_to_map_list(Results).

all_licence_details() ->
    Results = with_db_connection(
                fun(C) -> epgsql:equery(C, "SELECT * from copyright;", []) end
    ),
    util:sql_result_to_map_list(Results).

do_update_image_details(Hash, Chapter_Uuid, Caption, Licence_Status) ->
    Stmt1 = "UPDATE image SET chapter_uuid = $1, image_name = $2, licence_status = $3 WHERE hash = $4;",
    Parameters1 = [Chapter_Uuid, Caption, Licence_Status, Hash],
    Stmt2 = "SELECT * FROM image_chapter WHERE hash = $1;",
    Parameters2 = [Hash],
    Results = with_transaction(
                fun(C) ->
                        epgsql:equery(C, Stmt1, Parameters1),
                         epgsql:equery(C, Stmt2, Parameters2)
                end),
    Results2 = util:sql_result_to_map_list(Results),
    [Updates|[]] = Results2,
    img_mgr_proto:update_image(Updates),
    ok.
    %% TODO: need to notify the client of error

do_set_image_rank(Hash, New_Rank) ->
    Results = with_transaction(fun(C) -> do_set_image_rank(
                                           C, Hash, New_Rank)
                               end),
    case Results of
        ok -> ok;
        {renumbered, From_Rank, To_Rank, Chapter_Uuid} ->
            propagate_rank_updates(From_Rank, To_Rank, Chapter_Uuid)
    end.
%    img_mgr_proto:update_rank(Results),

do_set_image_rank(C, Hash, New_Rank) ->
    Offset = 1000,
    Stmt1 = "SELECT chapter_uuid, rank FROM image WHERE hash = $1;",
    {Chapter_Uuid, Old_Rank} = case epgsql:equery(C, Stmt1, [Hash]) of
                                   {ok, _Cols, [{CU, Rk}]} -> {CU, Rk}
                               end,
    Stmt2 = "SELECT MAX(rank) FROM image WHERE chapter_uuid = $1;",
    Max_Rank = case epgsql:equery(C, Stmt2, [Chapter_Uuid]) of
                   {ok, _, [{MR}]} -> MR
               end,
    Big_Rank = Max_Rank + Offset,

    %% block rank < 0 or > max
    if New_Rank == Old_Rank -> ok;
       New_Rank < 0 -> ok;
       New_Rank > Max_Rank -> ok;
       true -> do_change_images_rank(C, Hash, New_Rank, Old_Rank,
                                       Chapter_Uuid,
                                       Big_Rank)
    end.

do_change_images_rank(C, Hash, New_Rank, Old_Rank,
                                      Chapter_Uuid,
                                      Big_Rank) ->
    Tmp_Rank = -1,
    % move old image aside
    do_change_image_rank(C, Hash, Tmp_Rank),
    % update other images, up or down
    {From_Rank, To_Rank} =
        case New_Rank < Old_Rank of
            true -> do_demote_images(C, Chapter_Uuid, New_Rank,
                                     Old_Rank, Big_Rank),
                    {New_Rank, Old_Rank};
            false -> do_promote_images(C, Chapter_Uuid, New_Rank,
                                       Old_Rank, Big_Rank),
                     {Old_Rank, New_Rank}
        end,
    % move old image into new rank
    do_change_image_rank(C, Hash, New_Rank),
    {renumbered, From_Rank, To_Rank, Chapter_Uuid}.

do_change_image_rank(C, Hash, New_Rank) ->
    Stmt = "UPDATE image SET rank = $1 WHERE hash = $2;",
    epgsql:equery(C, Stmt, [New_Rank, Hash]).

do_demote_images(C, Chapter_Uuid, New_Rank, Old_Rank, Big_Rank) ->
    Stmt1 = "UPDATE image SET rank = rank + $1" ++
        " WHERE rank >= $2 AND rank <= $3 AND chapter_uuid = $4;",
    epgsql:equery(C, Stmt1, [Big_Rank, New_Rank, Old_Rank, Chapter_Uuid]),

    Stmt2 = "UPDATE image SET rank = rank - $1 + 1" ++
        " WHERE rank >= $1 AND chapter_uuid = $2;",
    epgsql:equery(C, Stmt2, [Big_Rank, Chapter_Uuid]).

do_promote_images(C, Chapter_Uuid, New_Rank, Old_Rank, Big_Rank) ->
    Stmt1 = "UPDATE image SET rank = rank + $1" ++
        " WHERE rank >= $2 + 1 AND rank <= $3 AND chapter_uuid = $4;",
    epgsql:equery(C, Stmt1, [Big_Rank, Old_Rank, New_Rank, Chapter_Uuid]),

    Stmt2 = "UPDATE image SET rank = rank - $1 - 1" ++
        " WHERE rank >= $2 AND chapter_uuid = $3;",
    epgsql:equery(C, Stmt2, [Big_Rank, Big_Rank, Chapter_Uuid]).

propagate_rank_updates(From_Rank, To_Rank, Chapter_Uuid) ->
    Stmt = "SELECT * FROM image_chapter " ++
        "WHERE chapter_uuid = $1 AND rank >= $2 AND rank <= $3 ORDER BY rank;",
    Parameters = [Chapter_Uuid, From_Rank, To_Rank],
    with_db_connection(
      fun(C) ->
              Res0 = epgsql:equery(C, Stmt, Parameters),
              Res1 = util:sql_result_to_map_list(Res0),
              [ img_mgr_proto:update_image(Res) || Res <- Res1 ]
      end
     ),
    ok.