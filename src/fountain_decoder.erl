-module(fountain_decoder).

-export([new/2, catch_drop/2, catch_drop/3]).

-type stats() :: { CntDroplets :: non_neg_integer(),
                   CntChunks :: non_neg_integer(),
                   Overhead :: float(),
                   UnknownChunks :: non_neg_integer() }.

-type catch_result() :: {missing, stats()} | {finished, binary(), stats()}.

-spec new(Len :: pos_integer(), BlockSize :: pos_integer()) -> {ok, reference()} | {error, any()}.
new(Len, BlockSize) ->
    erlang_fountain:new_decoder(Len, BlockSize).

-spec catch_drop(Decoder :: reference(),
                 Droplet :: erlang_fountain:droplet()) -> catch_result().
catch_drop(Decoder, Droplet) ->
    erlang_fountain:catch_drop(Decoder, Droplet).

-spec catch_drop(Decoder :: reference(),
                 Droplet :: erlang_fountain:droplet(),
                 DecType :: tc128 | tc256 | tc512) -> catch_result().
catch_drop(Decoder, Droplet, DecType) ->
    erlang_fountain:catch_drop_ldpc(Decoder, Droplet, DecType).
