% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(img_mgr_proto).

-export([update_image/8, remove_image/1, create_file/1]).
-export([update_image/1]).
-export([update_rank/1]).
-export([notice/1]).

update_rank(Details) ->
    broadcast(update_rank, Details).

update_image(Details) ->
    broadcast(update, Details).

update_image(Hash, Format, Resolution, Res_Category, Filename, Chapter_Uuid,
            Rank, Licence_Status) ->
    Details = #{
        hash => Hash,
        format => Format,
        resolution => Resolution,
        res_category => Res_Category,
        image_name => Filename,
        chapter_uuid => Chapter_Uuid,
        rank => Rank,
        licence_status => Licence_Status
     },
    broadcast(update, Details).

create_file(Hash) ->
    Details = #{
      hash => Hash
     },
    broadcast(create, Details).

remove_image(Hash) ->
    Details = #{
      hash => Hash
     },
    broadcast(delete, Details).

notice(Notice) ->
    Details = #{
      message => Notice
     },
    broadcast(notice, Details).

broadcast(Type, Details) ->
    ws_h:broadcast_json(Type, Details).
