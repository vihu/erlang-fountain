-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_decode_systematic_test() ->
    NumBytes = 1024,
    Chunk = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoder} = erlang_fountain:encode_systematic(Data, Chunk),

    ?assert(is_reference(Encoder)),

    {ok, Decoded} = erlang_fountain:decode(Encoder, NumBytes, Chunk),
    ?assert(Data == Decoded).

encode_decode_random_test() ->
    NumBytes = 1024,
    Chunk = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoder} = erlang_fountain:encode_random(Data, Chunk),

    ?assert(is_reference(Encoder)),

    {ok, Decoded} = erlang_fountain:decode(Encoder, NumBytes, Chunk),
    ?assert(Data == Decoded).
