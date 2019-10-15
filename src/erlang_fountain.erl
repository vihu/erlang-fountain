-module(erlang_fountain).

%% API
-export([encode_systematic/2, encode_random/2, decode/3]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-spec encode_systematic(Data :: binary(), Chunk :: pos_integer()) -> {ok, reference()} | {error, any()}.
encode_systematic(Data, Chunk) ->
    %% Do systematic encode unless specified
    encode_native_systematic(binary:bin_to_list(Data), Chunk).

-spec encode_random(Data :: binary(), Chunk :: pos_integer()) -> {ok, reference()} | {error, any()}.
encode_random(Data, Chunk) ->
    encode_native_random(binary:bin_to_list(Data), Chunk).

-spec decode(Encoder :: reference(), Length :: pos_integer(), Chunk :: pos_integer()) -> {ok, binary()} | {error, any()}.
decode(EncoderRef, Length, Chunk) ->
    case decode_native(EncoderRef, Length, Chunk) of
        {ok, L} ->
            {ok, binary:list_to_bin(L)};
        {error, _}=Error ->
            Error
    end.

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.

encode_native_systematic(_, _) ->
    not_loaded(?LINE).

encode_native_random(_, _) ->
    not_loaded(?LINE).

decode_native(_, _, _) ->
    not_loaded(?LINE).
