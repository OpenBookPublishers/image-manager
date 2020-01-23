% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([broadcast_json/2]).

-define(SERVER, ?MODULE).

broadcast_raw(Msg) ->
    gproc_ps:publish(l, ?SERVER, Msg).

broadcast_json(Type, Details) ->
    lager:info("TD: ~p ~p", [Type, Details]),
    broadcast_raw(encode_msg(Type, Details)).

encode_msg(Type, Details) ->
    Object = #{
      type => Type,
      details => Details
    },
    zj:encode(Object).

init(Req, Opts) ->
    Timeout_Secs = 300,
	{cowboy_websocket, Req, Opts, #{
        idle_timeout => Timeout_Secs * 1000}}.

websocket_init(State) ->
    % gproc register
    gproc_ps:subscribe(l, ?SERVER),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
    Reply = handle_text(Msg),
	{reply, {text, Reply}, State};
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({gproc_ps_event, ?SERVER, Echo}, State) ->
    {reply, {text, Echo}, State};
websocket_info(_Info, State) ->
	{ok, State}.

json_error() ->
    zj:encode(#{ type => "error",
                          details => "JSON not decoded" }).

handle_text(Msg) ->
    lager:info("Msg: ~p", [Msg]),
    try zj:binary_decode(Msg) of
        {ok, Term} -> handle_json(Term);
        _ -> json_error()
    catch 
        _ -> json_error()
    end.

handle_json(#{ <<"event">> := <<"get_all_images">> }) ->
    get_all_images();

handle_json(#{ <<"event">> := <<"get_all_chapters">> }) ->
    get_all_chapters();

handle_json(#{ <<"event">> := <<"get_all_licences">> }) ->
    get_all_licences();

handle_json(#{ <<"event">> := <<"delete_image">>,
               <<"details">> := #{
                 <<"hash">> := Hash
                }
             }) ->
    delete_image(binary_to_list(Hash));

handle_json(#{ <<"event">> := <<"update_image">>,
               <<"details">> := #{
                 <<"hash">> := Hash,
                 <<"image">> := Image
                }
             }) ->
    update_image(binary_to_list(Hash), Image);

handle_json(#{ <<"event">> := <<"set_rank">>,
               <<"details">> := #{
                 <<"hash">> := Hash,
                 <<"rank">> := New_Rank
                }
             }) ->
    set_rank(binary_to_list(Hash), New_Rank);

handle_json(Term) ->
    lager:info("Term: ~p", [Term]),
    encode_msg(message, "JSON decoded but not understood").


get_all_images() ->
    {ok, ImageData} = img_mgr_serv:all_images(),
    encode_msg(all_images, ImageData).

get_all_chapters() ->
    {ok, ChapterData} = img_mgr_serv:all_chapters(),
    encode_msg(all_chapters, ChapterData).

get_all_licences() ->
    {ok, LicenceData} = img_mgr_serv:all_licences(),
    encode_msg(all_licences, LicenceData).

delete_image(Hash) ->
    case img_mgr_serv:remove_image(Hash) of
        ok -> encode_msg(message, "Image deletion request submitted");
        _ -> encode_msg(message, "Error deleting image")
    end.

update_image(Hash, Image) ->
    #{
       <<"chapter_uuid">> := Chapter_Uuid,
       <<"id">> := Check_Hash,
       <<"text">> := Caption,
       <<"licence_status">> := Licence_Status
    } = Image,
    Hash = binary_to_list(Check_Hash),
    ok = img_mgr_serv:update_image_details(
           Hash,
           Chapter_Uuid,
           Caption,
           Licence_Status,
           Image),
    encode_msg(message, "Image updated").

set_rank(Hash, New_Rank) ->
    case img_mgr_serv:set_image_rank(Hash, New_Rank) of
        {ok, ok} -> encode_msg(message, "New rank requested");
        Fail -> lager:info("set rank failed: ~p", [Fail]),
                encode_msg(message, "New rank failed")
    end.

