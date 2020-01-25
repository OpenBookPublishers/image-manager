% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(q_img_thumbnail).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([submit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

submit(Args) ->
    ppool:async_queue(?MODULE, [Args]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Hash, Format, Resolution, Res_Category, Filename, Metadata}) ->
    process_flag(trap_exit, true),
    self() ! {run, Hash, Format, Resolution, Res_Category, Filename, Metadata},
    {ok, {Hash}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({run, Hash, Format, Resolution, Res_Category, Filename, Metadata}, State) ->
    try run(Hash, Format, Resolution, Res_Category, Filename, Metadata) of
        _ -> {stop, normal, State}
    catch
        _ -> {stop, normal, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run(Hash, Format, Resolution, Res_Category, Filename, Metadata)
  when is_binary(Hash), is_binary(Format), is_list(Resolution),
       is_binary(Filename) ->
    Timeout = 10 * 1000,
    Dir = util:upload_dir(),
    Path = binary_to_list(filename:join(Dir, Hash)),
    Output_Dir = util:thumbnail_dir(),
    Output_Path = binary_to_list(filename:join(Output_Dir, Hash)),
%    Thumb_Resolution = "128x128!",
    Thumb_Resolution = "64x64!",
    Command = "convert -thumbnail " ++ Thumb_Resolution ++ " " ++ Path ++ "[0] png:" ++ Output_Path,
    try shellcmd:run(Command, Timeout) of
        _Result -> %% FIXME
            process_thumbnail_details(Hash, Format, Resolution, Res_Category,
                                      Filename, Metadata)
    catch
        _ ->
            process_failed_thumbnail(Hash)
    end.

process_thumbnail_details(Hash, Format, Resolution, Res_Category, Filename, Metadata) ->
    img_mgr_serv:set_image_details(Hash, Format, Resolution,
                                   Res_Category, Filename, Metadata).

process_failed_thumbnail(Hash) ->
    img_mgr_serv:fail_image(Hash, "Could not generate thumbnail").
