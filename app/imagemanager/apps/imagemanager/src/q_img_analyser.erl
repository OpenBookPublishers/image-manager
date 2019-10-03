% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(q_img_analyser).

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

init({Hash, Filename, Metadata}) ->
    process_flag(trap_exit, true),
    self() ! {run, Hash, Filename, Metadata},
    {ok, {Hash}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({run, Hash, Filename, Metadata}, State) ->
    try run(Hash, Filename, Metadata) of
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

run(Hash, Filename, Metadata) ->
    Timeout = 10 * 1000,
    Dir = util:upload_dir(),
    Path = filename:join(Dir, Hash),
    Command = "identify " ++ Path,
    try shellcmd:run(Command, Timeout) of
        Result ->
            process_image_details(Hash, Result, Filename, Metadata)
    catch
        _ ->
            process_failed_image(Hash, Filename)
    end.

process_image_details(Hash, Result, Filename, Metadata) ->
    Tokens = string:tokens(Result, " "),
    Format = list_to_binary(lists:nth(2, Tokens)),
    Resolution = lists:nth(3, Tokens),
    case {format:allowed(Format), resolution:category(Resolution)} of
        {false, _} -> img_mgr_serv:fail_image(
                        Hash, "Image format not allowed: " ++ Format);
        {_, {rejected, Reason}} -> img_mgr_serv:fail_image(Hash, Reason);
        {_, {ok, Res_Category}} ->
            q_img_thumbnail:submit({Hash, Format, Resolution,
                                    Res_Category,
                                    Filename, Metadata})
    end.

process_failed_image(Hash, _Filename) ->
    img_mgr_serv:fail_image(Hash, "Could not identify(1) image").
