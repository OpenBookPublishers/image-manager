% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(imagemanager_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Secs = 15,
    ppool:start_pool(q_img_analyser, 2, {q_img_analyser, start_link, []}),
    ppool:start_pool(q_img_thumbnail, 1, {q_img_thumbnail, start_link, []}),
    ok = util:wait_for_db(Secs),
    ok = start_cowboy(),
    imagemanager_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", api_h, []},
            {"/api/[...]", api_h, []},
            {"/images/[...]", api_h, []},
            {"/thumbnails/[...]", api_h, []},
            {"/websocket", ws_h, []},
            {"/public/[...]", cowboy_static, {priv_dir, imagemanager, "public"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
         env => #{dispatch => Dispatch}
    }),
    ok.
