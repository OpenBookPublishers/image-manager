% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(format).

-export([mime_type/1, allowed/1]).

mime_type(<<"JPEG">>) ->
    {ok, <<"image/jpeg">>};
mime_type(<<"PDF">>) ->
    {ok, <<"application/pdf">>};
mime_type(<<"PNG">>) ->
    {ok, <<"image/png">>};
mime_type(<<"TIFF">>) ->
    {ok, <<"image/tiff">>};
mime_type(_) ->
    format_unsupported.

allowed(Format) ->
    case mime_type(Format) of
        {ok, _} -> true;
        _ -> false
    end.

             



