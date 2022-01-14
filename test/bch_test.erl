-module(bch_test).

-include("erlcode.hrl").
-include_lib("eunit/include/eunit.hrl").

bch_identity_test() ->
    Data = lists:seq(1, ?MAX_16_BIT_INT - 1),

    lists:foreach(
      fun(I) ->
              {ok, Enc} = erlcode:bch_encode(I),
              io:format("I: ~p, Enc: ~p~n", [I, Enc]),
              ?assert(Enc =< ?MAX_64_BIT_INT),
              {ok, {Dec, 0}} = erlcode:bch_decode(Enc),
              io:format("I: ~p, Dec: ~p~n", [I, Dec]),
              ?assert(Dec =< ?MAX_16_BIT_INT),
              ?assertEqual(I, Dec)
      end, Data),

    ok.
