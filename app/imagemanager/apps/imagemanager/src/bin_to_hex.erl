% snarfed from:
% https://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex

-module(bin_to_hex).

-export([bin_to_hex/1, bin_to_hex_string/1]).

bin_to_hex(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

bin_to_hex_string(Bin) when is_binary(Bin) ->
    erlang:binary_to_list(bin_to_hex(Bin)).

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.
