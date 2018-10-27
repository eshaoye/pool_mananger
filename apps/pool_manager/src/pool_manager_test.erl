-module(pool_manager_test).

-compile(export_all).

test1() ->
    pool_manager:start(pool_1, 10, db_test),
    Pid = pool_manager:checkout(pool_1),
    pool_manager:checkin(pool_1, Pid),
    sys:get_state(pool_1).


test2() ->
    pool_manager:start(pool_1, 10, db_test),
    Pid = pool_manager:checkout(pool_1),
    pool_manager:checkin(pool_1, Pid),
    State = sys:get_state(pool_1),
    io:format("before kill the child, state is ~p~n", [State]),
    PidList = element(5, State),
    exit(lists:nth(1, PidList), kill),
    sys:get_state(pool_1).