-module(fountain_encoder).

-export([new/3, new/4, next/1]).

-spec new(Data :: binary(),
          Chunk :: pos_integer(),
          Type :: random | systematic) -> reference().
new(Data, Chunk, Type) ->
    erlang_fountain:new_encoder(Data, Chunk, Type).

-spec new(Data :: binary(),
          Chunk :: pos_integer(),
          Type :: random_ldpc | systematic_ldpc,
          EncType :: tc128 | tc256 | tc512) -> reference().
new(Data, Chunk, Type, EncType) ->
    erlang_fountain:new_ldpc_encoder(Data, Chunk, Type, EncType).


-spec next(Encoder :: reference()) -> erlang_fountain:droplet().
next(Encoder) ->
    erlang_fountain:next(Encoder).
