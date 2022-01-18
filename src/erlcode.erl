%% @doc API for various coding schemes with Rust NIF as the backend.
%%
%% Exposes:
%%
%% `golay_extended_encode'
%%
%% `golay_extended_decode'
%%
%% `golay_standard_encode'
%%
%% `golay_standard_decode'
%%
%% `golay_shortened_encode'
%%
%% `golay_shortened_decode'
%%
%% `bch_encode'
%%
%% `bch_decode'
%%
%% `hamming_standard_encode'
%%
%% `hamming_standard_decode'
%%
%% `hamming_shortened_encode'
%%
%% `hamming_shortened_decode'

-module(erlcode).

%% API
-export(
   [
    golay_extended_encode/1, golay_extended_decode/1,
    golay_standard_encode/1, golay_standard_decode/1,
    golay_shortened_encode/1, golay_shortened_decode/1,
    bch_encode/1, bch_decode/1,
    hamming_standard_encode/1, hamming_standard_decode/1,
    hamming_shortened_encode/1, hamming_shortened_decode/1
   ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-include_lib("erlcode.hrl").

%% ==================================================================
%% API
%% ==================================================================

%% @doc Encode the given 12 data bits into a 24-bit codeword
-spec golay_extended_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_extended_encode(Data) when Data < ?MAX_12_BIT_INT ->
    not_loaded(?LINE).


%% @doc Try to decode the given 24-bit word to the nearest codeword, correcting up to 3 errors and detecting 4 errors
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 12
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec golay_extended_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                                      {error, {unrecoverable, non_neg_integer()}}.
golay_extended_decode(Data) when Data < ?MAX_24_BIT_INT ->
    not_loaded(?LINE).

%% @doc Encode the given 12 data bits into a 23-bit codeword
-spec golay_standard_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_standard_encode(Data) when Data < ?MAX_12_BIT_INT ->
    not_loaded(?LINE).

%% @doc Try to decode the given 23-bit word to the nearest codeword, correcting up to 3 errors and detecting 4 errors
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 12
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec golay_standard_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                                      {error, {unrecoverable, non_neg_integer()}}.
golay_standard_decode(Data) when Data < ?MAX_23_BIT_INT ->
    not_loaded(?LINE).

%% @doc Encode the given 6 data bits into a 18-bit codeword
-spec golay_shortened_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
golay_shortened_encode(Data) when Data < ?MAX_6_BIT_INT ->
    not_loaded(?LINE).

%% @doc Try to decode the given 18-bit word to the nearest codeword, correcting up to 3 errors
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 6
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec golay_shortened_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                                       {error, {unrecoverable, non_neg_integer()}}.
golay_shortened_decode(Data) when Data < ?MAX_18_BIT_INT ->
    not_loaded(?LINE).

%% @doc Encode the given 16 data bits into a 64-bit codeword
-spec bch_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
bch_encode(Data) when Data < ?MAX_16_BIT_INT ->
    not_loaded(?LINE).

%% @doc Try to decode the given 64-bit word to the nearest codeword, correcting up to 11 bit errors
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 16
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec bch_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                           {error, {unrecoverable, non_neg_integer()}}.
bch_decode(Data) when Data < ?MAX_64_BIT_INT ->
    not_loaded(?LINE).

%% @doc Encode the given 11 data bits into a 15-bit codeword
-spec hamming_standard_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
hamming_standard_encode(Data) when Data < ?MAX_11_BIT_INT ->
    not_loaded(?LINE).

%% @doc Try to decode the given 15-bit word to the nearest codeword, correcting up to 1 bit error
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 11
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec hamming_standard_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                                        {error, {unrecoverable, non_neg_integer()}}.
hamming_standard_decode(Data) when Data < ?MAX_15_BIT_INT ->
    not_loaded(?LINE).

%% @doc Encode the given 6 data bits into a 10-bit codeword
-spec hamming_shortened_encode(Data :: pos_integer()) -> {ok, Encoded :: pos_integer()}.
hamming_shortened_encode(Data) when Data < ?MAX_6_BIT_INT ->
    not_loaded(?LINE).

%% @doc Try to decode the given 10-bit word to the nearest codeword, correcting up to 1 bit error
%%
%% If decoding was successful, return `{ok, (data, err)}', where `data' is the 6
%% data bits and `err' is the number of corrected bits.
%% Otherwise, return `{error, {unrecoverable, data}}' to
%% indicate an unrecoverable error.
-spec hamming_shortened_decode(Data :: pos_integer()) -> {ok, {Decoded :: pos_integer(), Corrupted :: non_neg_integer()}} |
                                                         {error, {unrecoverable, non_neg_integer()}}.
hamming_shortened_decode(Data) when Data < ?MAX_10_BIT_INT ->
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
