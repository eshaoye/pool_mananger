-module(pool_manager).

-behaviour(gen_server).

%%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([start/3,
        stop/1,
        checkout/1,
        checkin/2]).

-record(state, {
    workers_num = 0,
    worker_mod = undefined,
    used = [],
    workers = []
}).

start(PoolName, WorkersNum, WorkerMod) ->
    gen_server:start({local, PoolName}, ?MODULE, [WorkersNum, WorkerMod], []).

stop(PoolName) ->
    ok.

checkout(PoolName) ->
    gen_server:call(PoolName, checkout).

checkin(PoolName, Pid) ->
    gen_server:cast(PoolName, {checkin, Pid}).

init([WorkersNum, WorkerMod]) ->
    process_flag(trap_exit, true),
    Workers = [begin {ok, P} = WorkerMod:start_link(), P end || _ <- lists:seq(1, WorkersNum)],
    {ok, #state{workers_num = WorkersNum,
                worker_mod = WorkerMod,
                workers = Workers}}.

handle_call(checkout, _From, #state{workers = []} = State) ->
    {reply, {error, no_workers}, State};
handle_call(checkout, _From, #state{workers = [P | T],
                                    used = U} = State) ->
    {reply, P, State#state{workers = T,
                            used = [P | U]}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% -----------------------------------------------------------------
%%% handle_cast/2
%%% -----------------------------------------------------------------
handle_cast({checkin, Pid}, #state{workers = W,
                                    used = U} = State) ->
    {noreply, State#state{workers = [Pid | W],
                          used = U -- [Pid]}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% handle_info/2
%%% -----------------------------------------------------------------
handle_info({'EXIT', From, Reason}, #state{workers = W,
                                            used = U,
                                            worker_mod = Mod} = State) ->
    case lists:member(From, W) of
        true ->
            {ok, P} = Mod:start_link(),
            {noreply, State#state{workers = [P | W] -- [From],
                                  used = U -- [From]}};
        false ->
            {ok, P} = Mod:start_link(),
            {noreply, State#state{workers = [P | W],
                                  used = U -- [From]}}
    end;
handle_info(_, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% terminate/2
%%% -----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%% -----------------------------------------------------------------
%%% code_change/3
%%% -----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.        