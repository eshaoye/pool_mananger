-module(pool_manager_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

case1_test() ->
    catch pool_manager:stop(pool_1),
    {ok, _} = pool_manager:start(pool_1, 10, db_worker),
    Pid = pool_manager:checkout(pool_1),
    pool_manager:checkin(pool_1, Pid),
    sys:get_state(pool_1).


case2_test() ->
    catch pool_manager:stop(pool_1),
    {ok, _} = pool_manager:start(pool_1, 10, db_worker),
    Pid = pool_manager:checkout(pool_1),
    pool_manager:checkin(pool_1, Pid),
    State = sys:get_state(pool_1),
    io:format("before kill the child, state is ~p~n", [State]),
    PidList = element(6, State),
    exit(lists:nth(1, PidList), kill),
    sys:get_state(pool_1).


case3_test() ->
    catch pool_manager:stop(pool_1),
    {ok, _} = pool_manager:start(pool_1, 2, db_worker, 2),
    Pid1 = pool_manager:checkout(pool_1),
    Pid2 = pool_manager:checkout(pool_1),
    Pid3 = pool_manager:checkout(pool_1),
    Pid4 = pool_manager:checkout(pool_1),
    {error, no_workers} = pool_manager:checkout(pool_1),
    sys:get_state(pool_1).