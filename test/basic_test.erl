-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_decode_systematic_test() ->
    NumBytes = 1024,
    Chunk = 64,
    Data = crypto:strong_rand_bytes(NumBytes),
    io:format("Data: ~p~n", [Data]),
    Encoder = fountain_encoder:new(Data, Chunk, systematic),
    Decoder = fountain_decoder:new(NumBytes, Chunk),

    {done, Output} = lists:foldl(
                       fun(_, {done, Res}) ->
                               {done, Res};
                          (_, {not_done, Res}) ->
                               CatchResult = fountain_decoder:catch_drop(Decoder, fountain_encoder:next(Encoder)),
                               io:format("CatchResult: ~p~n", [CatchResult]),
                               case CatchResult of
                                   {missing, _} ->
                                       {not_done, Res};
                                   {finished, Ret, _Stats} ->
                                       {done, Ret}
                               end
                       end, {not_done, <<>>}, lists:seq(1, 5000)),

    io:format("Output: ~p~n", [Output]),
    ?assert(Output == Data).

encode_decode_random_test() ->
    NumBytes = 1024,
    Chunk = 64,
    Data = crypto:strong_rand_bytes(NumBytes),
    %% io:format("Data: ~p~n", [Data]),
    Encoder = fountain_encoder:new(Data, Chunk, random),
    Decoder = fountain_decoder:new(NumBytes, Chunk),

    {done, Output} = lists:foldl(
                       fun(_, {done, Res}) ->
                               {done, Res};
                          (_, {not_done, Res}) ->
                               CatchResult = fountain_decoder:catch_drop(Decoder, fountain_encoder:next(Encoder)),
                               %% io:format("Foo: ~p~n", [Foo]),
                               case CatchResult of
                                   {missing, _} ->
                                       {not_done, Res};
                                   {finished, Ret, _Stats} ->
                                       {done, Ret}
                               end
                       end, {not_done, <<>>}, lists:seq(1, 5000)),

    ?assert(Output == Data).
