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
        start/4,
        stop/1,
        checkout/1,
        checkin/2]).

-record(state, {
    workers_num = 0,
    worker_mod = undefined,
    overflow = 0,
    used = [],
    workers = []
}).

start(PoolName, WorkersNum, WorkerMod) ->
    gen_server:start({local, PoolName}, ?MODULE, [WorkersNum, WorkerMod], []).

start(PoolName, WorkersNum, WorkerMod, Overflow) ->
    gen_server:start({local, PoolName}, ?MODULE, [WorkersNum, WorkerMod, Overflow], []).

stop(PoolName) ->
    ok.

checkout(PoolName) ->
    gen_server:call(PoolName, checkout).

checkin(PoolName, Pid) ->
    gen_server:cast(PoolName, {checkin, Pid}).

init([WorkersNum, WorkerMod]) ->
    init([WorkersNum, WorkerMod, 0]);
init([WorkersNum, WorkerMod, Overflow]) ->
    process_flag(trap_exit, true),
    Workers = [begin {ok, P} = WorkerMod:start_link(), P end || _ <- lists:seq(1, WorkersNum)],
    {ok, #state{workers_num = WorkersNum,
                worker_mod = WorkerMod,
                workers = Workers,
                overflow = Overflow}}.

handle_call(checkout, _From, #state{workers = [],
                                    workers_num = InitNum,
                                    used = U,
                                    overflow = Overflow,
                                    worker_mod = Mod} = State) ->
    case erlang:length(U) >= InitNum + Overflow of
        true ->
            {reply, {error, no_workers}, State};
        false ->
            {ok, P} = Mod:start_link(),
            {reply, P, State#state{used = [P | U]}}
    end;
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
                                   workers_num = InitNum,
                                   used  = U,
                                   worker_mod = Mod} = State) ->   
    case erlang:length(U) > InitNum of
        true ->
            unlink(Pid),
            Mod:stop(Pid),
            {noreply, State#state{used = U -- [Pid]}};
        false ->
            {noreply, State#state{workers = [Pid | W],
                          used = U -- [Pid]}}     
    end;
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