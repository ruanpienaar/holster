-module(holster_sm_tests).
-include_lib("eunit/include/eunit.hrl").

holster_sm_unit_test_() ->
    {setup,
     % Setup Fixture
     fun() ->
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
       {"holster_sm:open/0 when already connected",
            ?_assert(unit_testing:try_test_fun(fun open_when_connected/0))}
     ]
    }.

open_when_connected() ->
    %% TODO: use proper Data map
    ?assertEqual(
        keep_state_and_data,
        holster_sm:open(enter, _PrevState = connected, #{})
    ).
