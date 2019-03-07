%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2018, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2018 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(abel_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_worker/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [N]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------

%%% there a set of tree servers, creating simple_one_for_one for testing purpose right now
%%% this can be changed by specifiying a list of child specifications and use one_for_one instead

init([N]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    PoolTable = ets:new(tree, [public, set]),
    true = ets:insert(PoolTable, {c, 0}),
    PoolTableCounter = ets:new(treecounter, [named_table,public, set]),
    true = ets:insert(PoolTableCounter, {c, 0}),
    CreateSpec = fun(X) ->
			 child_specs(X, {'abel_inf', start_link, [X]}, PoolTable)
		 end,
    Children = lists:map(CreateSpec, lists:seq(1,N)),

    Registration = #{ id => N+1,
		      start => {'abel_reg', start_link, [N, PoolTable]},
		      restart => permanent,
		      shutdown => 5000,
		      type => worker,
		      modules => ['abel_reg']},

    {ok, {SupFlags, [Registration | Children]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child_specs(Id, {M,F,A}, PoolTable) ->
    {Id, {?MODULE, start_worker, [Id,{M,F,A},PoolTable]}, permanent, 2000, worker, [M]}.

start_worker(Id, {M,F,A},PoolTable) ->
    {ok, Pid} = erlang:apply(M,F,A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.
