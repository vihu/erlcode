-module(hamming_test).

-include("erlcode.hrl").
-include_lib("eunit/include/eunit.hrl").

hamming_standard_test() ->
    Data = lists:seq(1, ?MAX_11_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:hamming_standard_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_15_BIT_INT),
              {ok, {Dec, 0}} = erlcode:hamming_standard_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_11_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.

hamming_shortened_test() ->
    Data = lists:seq(1, ?MAX_6_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:hamming_shortened_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_10_BIT_INT),
              {ok, {Dec, 0}} = erlcode:hamming_shortened_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_6_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.
