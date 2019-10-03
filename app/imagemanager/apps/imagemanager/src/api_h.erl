% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(api_h).
-export([init/2]).

-define(PAYLOAD_FIELD, "thefile").
-define(CHAPTER_FIELD, "x-chapter-uuid").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    URI = cowboy_req:path(Req0),
    handle_req(Method, URI, Req0, State).


handle_req(<<"GET">>, <<"/api/images">>, Req0, State) ->
    {ok, ImageData} = img_mgr_serv:all_images(),
    Payload = zj:binary_encode(ImageData),
    Resp = cowboy_req:reply(200,
      #{
         <<"Content-type">> => <<"application/json">>
       }, Payload, Req0),
    {ok, Resp, State};

handle_req(<<"POST">>, <<"/api/upload">>, Req0, State) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req0),
	{ok, Data, Req3} = read_whole_file_upload(Req2),
	{file, <<?PAYLOAD_FIELD>>, Filename, ContentType}
		= cow_multipart:form_data(Headers),
    Chapter_Uuid = cowboy_req:header(<<?CHAPTER_FIELD>>, Req0),
    % this metadata structure is actually consumed in img_mgr_serv:save_image
    Metadata = {upload_metadata, Chapter_Uuid},
    Hash = bin_to_hex:bin_to_hex_string(crypto:hash(md5, Data)),
    {ok, _} = img_mgr_serv:receive_upload(Filename, Hash, Data,
                                          ContentType, Metadata),
	{ok, Req3, State};


handle_req(<<"GET">>, <<"/images/", Hash/binary>>, Req0, State) ->
    Path = filename:join(util:upload_dir(), Hash),
    C = util:db_connection(),
    Parameters = [Hash],
    {ok, _Cols, Rows} = epgsql:equery
                    (C, "SELECT format FROM image WHERE hash = $1;", Parameters),
    epgsql:close(C),
    Resp =
        case Rows of
        [] -> not_found(Req0);
        [{Format}|[]] ->
                {ok, MimeType} = format:mime_type(Format),
                {ok, Payload} = file:read_file(Path),
                cowboy_req:reply(
                  200,
                  #{
                    <<"Content-type">> => MimeType
                   }, Payload, Req0)
        end,
    {ok, Resp, State};

handle_req(<<"GET">>, <<"/thumbnails/", Hash/binary>>, Req0, State) ->
    Path = filename:join(util:thumbnail_dir(), Hash),
    Format = <<"PNG">>,
    Resp = case util:file_exists(Path) of
        true -> not_found(Req0);
        false ->
            {ok, MimeType} = format:mime_type(Format),
            {ok, Payload} = file:read_file(Path),
            ETag = <<"ETag: \"thumb-", Hash/binary, "\"">>,
            cowboy_req:reply(
              200,
              #{
                <<"Content-type">> => MimeType,
                <<"Cache-control">> => <<"max-age=86400">>,
                <<"ETag">> => ETag
               }, Payload, Req0)
    end,
    {ok, Resp, State};

handle_req(_Method, _URI, Req0, State) ->
    Req = not_found(Req0),
    Method = cowboy_req:method(Req0),
    URI = cowboy_req:path(Req0),
    lager:info(erlang:binary_to_list(<<"Method: ", Method/binary>>)),
    lager:info(erlang:binary_to_list(<<"URI: ", URI/binary>>)),
    {ok, Req, State}.

not_found(Req0) ->
    cowboy_req:reply(
      404,
      #{ <<"Content-type">> => "text/plain" },
      "Not found", Req0
    ).

read_whole_file_upload(Req2) ->
    read_whole_file_upload(Req2, <<"">>).

read_whole_file_upload(Req2, Acc) ->
    case cowboy_req:read_part_body(Req2) of
        {more, NewData, Req3} ->
            read_whole_file_upload(Req3, <<Acc/binary, NewData/binary>>);
        {ok, FinalData, Req3} ->
            {ok, <<Acc/binary, FinalData/binary>>, Req3};
        _ -> chunked_upload_failed
    end.
