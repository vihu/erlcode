-module(erlcode).

%% API
-export(
   [
    golay_extended_encode/1,
    golay_extended_decode/1,
    golay_standard_encode/1,
    golay_standard_decode/1,
    golay_shortened_encode/1,
    golay_shortened_decode/1
   ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-include_lib("erlcode.hrl").

%% ==================================================================
%% API
%% ==================================================================

-spec golay_extended_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_extended_encode(Data) when Data < ?MAX_12_BIT_INT ->
    not_loaded(?LINE).
-spec golay_extended_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} | {error, unrecoverable}.
golay_extended_decode(Data) when Data < ?MAX_24_BIT_INT ->
    not_loaded(?LINE).

-spec golay_standard_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_standard_encode(Data) when Data < ?MAX_12_BIT_INT ->
    not_loaded(?LINE).
-spec golay_standard_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} | {error, unrecoverable}.
golay_standard_decode(Data) when Data < ?MAX_23_BIT_INT ->
    not_loaded(?LINE).

-spec golay_shortened_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_shortened_encode(Data) when Data < ?MAX_6_BIT_INT ->
    not_loaded(?LINE).
-spec golay_shortened_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} | {error, unrecoverable}.
golay_shortened_decode(Data) when Data < ?MAX_18_BIT_INT ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

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
