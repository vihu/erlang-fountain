-module(erlang_fountain).

%% API
-export([
         %% Encoding
         new_encoder/3,
         new_ldpc_encoder/4,
         next/1,
         %% Decoding
         new_decoder/2,
         catch_drop/2,
         catch_drop_ldpc/3
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-type drop_type() :: {seeded, Seed :: pos_integer(), Degree :: pos_integer()} | {edges, Cnt :: [pos_integer()]}.
-type droplet() :: {DropType :: drop_type(), Data :: binary()}.

-spec new_encoder(Data :: binary(), Chunk :: pos_integer(), Type :: random | systematic) -> reference().
new_encoder(_Data, _Chunk, _Type) ->
    not_loaded(?LINE).

-spec new_ldpc_encoder(Data :: binary(), Chunk :: pos_integer(), Type :: systematic | random, EncType :: tc128 | tc256 | tc512) -> reference().
new_ldpc_encoder(_Data, _Chunk, _Type, _EncType) ->
    not_loaded(?LINE).

-spec next(Encoder :: reference()) -> droplet().
next(_Encoder) ->
    not_loaded(?LINE).

-spec new_decoder(Len :: pos_integer(), BlockSize :: pos_integer()) -> reference().
new_decoder(_Len, _BlockSize) ->
    not_loaded(?LINE).

-spec catch_drop(Decoder :: reference(), Droplet :: droplet()) -> reference().
catch_drop(_Decoder, _Droplet) ->
    not_loaded(?LINE).

-spec catch_drop_ldpc(Decoder :: reference(), Droplet :: droplet(), DecType :: tc128 | tc256 | tc512) -> reference().
catch_drop_ldpc(_Decoder, _Droplet, _DecType) ->
    not_loaded(?LINE).


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
