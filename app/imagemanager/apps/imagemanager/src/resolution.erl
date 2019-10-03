% OBP Image Manager, an image-manager for book publishing, by Martin Keegan
%
% Copyright (C) 2019, Open Book Publishers CIC Ltd
%
% This programme is free software; you may redistribute and/or modify
% it under the terms of the Apache Licence v2.0.
-module(resolution).

-export([category/1]).

% we expect Resolution to be in the form "1920x768"
category(Resolution) ->
    [Width, Height |[]] = string:split(Resolution, "x"),
    W = list_to_integer(Width),
    H = list_to_integer(Height),
    sieve_category(H, W).

% when sieving through, we transpose the X and Y dimensions
sieve_category(Height, Width)
  when Height >= 3200, Width >= 2500 -> {ok, full_page};
sieve_category(Height, Width)
  when Height >= 2100, Width >= 1700 -> {ok, half_page};
sieve_category(Height, Width)
  when Height >= 800, Width >= 500 -> {ok, small};
sieve_category(_, _) -> {ok, low_res}.

% can also say {rejected, "Reason ..."}
