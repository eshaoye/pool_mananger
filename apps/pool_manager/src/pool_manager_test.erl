-module(pool_manager_test).

-compile(export_all).

test1() ->
    pool_manager:start(pool_1, 10, db_test),
    Pid = pool_manager:checkout(pool_1),
    pool_manager:checkin(pool_1, Pid),
    sys:get_state(pool_1).
