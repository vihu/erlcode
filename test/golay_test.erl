-module(golay_test).

-include("erlcode.hrl").
-include_lib("eunit/include/eunit.hrl").

encode_extended_identity_test() ->
    Data = lists:seq(1, ?MAX_12_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:golay_extended_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_24_BIT_INT),
              {ok, {Dec, 0}} = erlcode:golay_extended_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_12_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.

encode_standard_identity_test() ->
    Data = lists:seq(1, ?MAX_12_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:golay_standard_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_23_BIT_INT),
              {ok, {Dec, 0}} = erlcode:golay_standard_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_12_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.

encode_shortened_identity_test() ->
    Data = lists:seq(1, ?MAX_6_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:golay_shortened_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_18_BIT_INT),
              {ok, {Dec, 0}} = erlcode:golay_shortened_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_6_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.
