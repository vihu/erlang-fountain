-module(fountain_encoder).

-export([new/3, next/1]).

-spec new(Data :: binary(),
          Chunk :: pos_integer(),
          Type :: random | systematic) -> reference().
new(Data, Chunk, Type) ->
    erlang_fountain:new_encoder(Data, Chunk, Type).

-spec next(Encoder :: reference()) -> erlang_fountain:droplet().
next(Encoder) ->
    erlang_fountain:next(Encoder).
