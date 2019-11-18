%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2018, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2018 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(abel_coord).

-behaviour(gen_statem).

%% API
-export([new_component/3,start_component/1,start_component/2]).

-export([prefix/2,choice/2,par/2]).


%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-record(data, {
	  queue,
	  agent,
	  procs,
	  interface,
	  status,   % storing the status of each process, send or receive at the moment
	  init_beh, % initial behaviour
	  mid,
	  counter,
	  resend, % before entering retrying sending (mid = counter), re calculating the set
	  module
	 }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
new_component(M, Env, I) ->
    gen_statem:start_link(?MODULE, [M, Env, I], []).

start_component(Pid) ->
    gen_statem:cast(Pid, {start_beh, init_beh}).

start_component(Pid, Init) ->
    gen_statem:cast(Pid, {start_beh, Init}).

%% to be called by processes
prefix(Pid, {Type, Act, Con}) ->
    gen_statem:call(Pid, {Type, Act, Con}).

choice(Pid, {Type, BehList}) ->
    gen_statem:call(Pid, {choice, Type, BehList}).

par(Pid, BehList) ->
    gen_statem:cast(Pid, {parallel, BehList}).

%% call(Pid, F) ->
%%     gen_statem:cast(Pid, {call, F}).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
		  gen_statem:init_result(term()).
init([M,Env,I]) ->
    process_flag(trap_exit, true),
    {ok, S} = abel_reg:reg(), %% register to the inf
%    InitBeh = erlang:apply(M,init_beh,[self(),[]]),
    {ok, {Env, false}, #data{queue = gb_trees:empty(),
			     status = #{}, agent = S, procs = 0, module = M,
			     interface = tuple_to_list(I), resend = 0, mid = -1, counter = 0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for every event a gen_statem receives.
%% @end
%%--------------------------------------------------------------------
-spec handle_event('enter',
		   OldState :: term(),
		   State :: term(),
		   Data :: term()) ->
			  gen_statem:state_enter_result(term());
		  (gen_statem:event_type(),
		   Msg :: term(),
		   State :: term(),
		   Data :: term()) ->
			  gen_statem:event_handler_result(term()).
%% starting point of the execution
handle_event(cast, {start_beh, F}, _State, Data = #data{module = M}) ->
    erlang:apply(M,F,[self(),[]]),
    %Pid = spawn_link(F),
    {keep_state, Data};
%, Data#data{procs = NumProcs + length(Pids)}};

%% handle request of parallel process creation
handle_event(cast, {parallel, FunList}, _State, Data  = #data{procs = NumProcs}) ->
    Pids = [spawn_link(F) || F <- FunList],
    %io:format("create ~p processes ~n",[length(Pids)]),
    {keep_state, Data#data{procs = NumProcs + length(Pids)}};

% a process terminated
handle_event(info, {'EXIT', _, _}, {Env,Flip},
	     Data = #data{status = Status, procs = NProcs, counter = Counter, mid = Mid, interface = I }) when Mid == Counter ->
    %% must send situation
    NumProcs = NProcs - 1,
    Resend = maps:size(maps:filter(fun(_,V) -> V == sending end, Status)),
%    io:format("~p exit 1 proc where n = ~p, m = ~p and sends stuck ~p ~n ",[maps:with(I,Env), NumProcs, maps:size(Status), Resend]),
    case NumProcs == maps:size(Status) of %% all processes commit
	true ->
	    {next_state, {Env, not Flip}, Data#data{status = Status, procs = NumProcs, resend = Resend}};
	false ->
	    {keep_state, Data#data{status = Status, procs = NumProcs}}
    end;
handle_event(info, {'EXIT', _, _}, {Env, Flip}, Data = #data{status = Status, procs = NProcs, queue = Queue}) ->
    %% There are two cases arising from this event, either the component goes to receive, or send
    NumProcs = NProcs - 1,
    %% case maps:get(assigned,Env) of
    %% 	1 ->
    %% 	    maps:get(report,Env) ! {done, maps:get(id,Env),maps:get(colour,Env),maps:get(round,Env)},
    %% 	    {stop,normal,Data};
    %% 	0 ->
    %% 	    ok
    %% end,

    case NumProcs == maps:size(Status) andalso not gb_trees:is_empty(Queue) of %% all processes commit
	true ->
	    {keep_state, Data#data{status = Status, procs = NumProcs},
	     [{next_event, internal, receiving}]};
	false ->
	    {keep_state, Data#data{status = Status, procs = NumProcs}}
    end;

%% handle a sending operation of any process, guarded by G, update U followed
handle_event({call, From},
	     {send, {G, Msg, _, _}, _},
	     {Env, _},
	     Data = #data{agent = Agent, status = Status, procs = NumProcs, interface = I, queue = Queue, mid = Mid, counter = Counter}) when Mid =/= Counter ->

    NewMid = case G(Env) andalso Mid == -1 of
		 true ->
		     NewMsg = partial_eval(Msg, Env),
		     %io:format("===~p request id when counter = ~p for ~p ~n",[maps:with(I,Env), Counter, NewMsg]),
		     abel_inf:request_id(Agent, self()),
		     -2;
		 false -> Mid
	     end,

    NewStatus = maps:update_with(From, fun(_) -> sending end, sending, Status),
    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of %% can receive?
	true ->
	    {keep_state, Data#data{status = NewStatus, mid = NewMid},[postpone,{next_event, internal, receiving}]};
	false ->
	    {keep_state, Data#data{status = NewStatus, mid = NewMid},[postpone]}
    end;
handle_event({call, From}, {send, {G, Msg, Pred, U}, Con}, {Env, Sending},
	     Data = #data{agent = Agent, status = Status, procs = NumProcs, interface = I, mid = Mid, counter = Counter, resend = Resend}) ->
    %% register this sending action in the status as discarding any message
    case G(Env) of
	true ->
	    PublicEnv = maps:with(I,Env),
	    NewPred = partial_eval(Pred, PublicEnv),
	    NewMsg = partial_eval(Msg, Env),
	    abel_inf:send(Agent, {Mid, {NewPred, NewMsg, PublicEnv}, self()}),
	    NewEnv = update_output(Env,U),
	    gen_statem:reply(From, {Con,[]}),
	    %attout(I,Env,NewMsg),

	    Status2 = maps:remove(From, Status),
	    %% if sucessfully send, then relies on environment change to retry deffered actions if any
	    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1}};

	false ->
	    NewMsg = partial_eval(Msg, Env),
	    NewStatus = maps:update_with(From, fun(_) -> sending end, sending, Status),
	    %io:format("resend of ~p = ~p mid = ~p bc of ~p with ~p, where n = ~p, m = ~p ~n",[maps:with(I,Env), Resend, Mid,U, NewMsg, NumProcs, maps:size(NewStatus)]),
	    case Resend == 0 of
		true -> %% last send can trigger receiving a pending message
		    {keep_state, Data#data{status = NewStatus}, [postpone]};
		false ->
		    case Resend == 1 andalso NumProcs == maps:size(NewStatus) of
			true ->
			    io:format("resend of ~p = ~p mid = ~p bc of ~p with ~p ~n",[maps:with(I,Env), Resend, Mid,U, NewMsg]),
			    abel_inf:send(Agent, {Mid, {fun(_,_) -> false end, {}, #{}}, self()}),
			    attout(I,Env,#{}),
			    {keep_state, Data#data{status = NewStatus, resend = Resend - 1, counter = Counter + 1, mid = -1}, [postpone]};
		       false ->
			    %io:format("false branch resend of ~p = ~p mid = ~p bc of ~p with ~p, where n = ~p, m = ~p ~n",[maps:with(I,Env), Resend, Mid,U, NewMsg, NumProcs, maps:size(NewStatus)]),
			    {keep_state, Data#data{status = NewStatus, resend = Resend - 1}, [postpone]}
		    end
	    end
    end;

%% no mixed choice of sending and receiving
%% BehList = [{Description},...]
%% SendDescription = {{Guard, Msg, Pred, U},Con}
handle_event({call,From}, {choice, send, BehList}, {Env, _},
	     Data = #data{procs = NumProcs, status = Status, agent = Agent, queue = Queue, interface = I, mid = Mid, counter = Counter}) when Mid =/= Counter ->
    % pick any send
    NewMid = case lists:dropwhile(fun({Act,_}) ->
				 G = element(1,Act), % the first element of X is aware predicate
				 not (G(Env) andalso Mid == -1)
			 end, BehList) of
		 [] ->  Mid;
		 _ -> abel_inf:request_id(Agent, self()),
		     %io:format("===~p request id when counter = ~p ~n",[maps:with(I,Env), Counter]),
		      -2
	     end,
    NewStatus = maps:update_with(From, fun(_) -> sending end, sending, Status), % I am not receiving
    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of %% can receive?
	true ->
	    {keep_state, Data#data{status = NewStatus, mid = NewMid},[postpone,{next_event, internal, receiving}]};
	false ->
	    {keep_state, Data#data{status = NewStatus, mid = NewMid},[postpone]}
    end;
handle_event({call,From}, {choice, send, BehList}, {Env, Sending},
	     Data = #data{procs = NumProcs, status = Status, agent = Agent, interface = I, mid = Mid, resend = Resend, counter = Counter})  ->
    case lists:dropwhile(fun({X,_}) ->
				 G = element(1,X), % the first element of X is aware predicate
				 not (G(Env))
			 end,
			 BehList) of
	[] ->
	    NewStatus = maps:update_with(From, fun(_) -> sending end, sending, Status),
	    case Resend == 0 of
		true -> %% last send can trigger receiving a pending message
		    {keep_state, Data#data{status = NewStatus}, [postpone]};
		false ->
		    case Resend == 1 andalso NumProcs == maps:size(NewStatus) of
			true ->
			    io:format("resend of ~p = ~p, mid = ~p ~n",[maps:with(I,Env), Resend,Mid]),
			    abel_inf:send(Agent, {Mid, {fun(_,_) -> false end, {}, #{}}, self()}),
			 %   attout(I,Env,#{}),
			    {keep_state, Data#data{status = NewStatus, resend = Resend - 1, counter = Counter + 1, mid = -1}, [postpone]};
			false ->
			    {keep_state, Data#data{status = NewStatus, resend = Resend - 1}, [postpone]}
		    end
	    end;
	[{{_, Msg, Pred, Update}, Con} | _] ->   %% pick the first sending action, followed by an update
	    PublicEnv = maps:with(I,Env),
	    NewPred = partial_eval(Pred, PublicEnv),
	    NewMsg = partial_eval(Msg, Env),
	    abel_inf:send(Agent, {Mid, {NewPred, NewMsg, PublicEnv}, self()}),
	    NewEnv = update_output(Env, Update),
	    gen_statem:reply(From, {Con,[]}),
	    Status2 = maps:remove(From,Status),
	    %% fix the semantics of sending
	    %attout(I,Env,NewMsg),
	    %% if sucessfully send, then relies on environment change to retry deffered actions if any
	    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1}}
    end;

%% Choice Receive
%% RecvDescription = {{Guard, Pred, X, U},Con}
handle_event({call, From}, {choice, recv, BehList}, _State,
	     Data = #data{procs = NumProcs, status = Status, queue = Queue}) ->
	    % this is a choice with receiving, after register it for receiving, handling similar to ordinary receive
    NewStatus = maps:update_with(From, fun(_) -> BehList end, BehList, Status),
    case not gb_trees:is_empty(Queue) andalso NumProcs == maps:size(NewStatus) of
	true ->
	    {keep_state, Data#data{status = NewStatus},
	     [{next_event, internal, receiving}]};
	false ->
	    {keep_state, Data#data{status = NewStatus}}
    end;

handle_event({call, From}, {recv, Act, Con}, _State,
	     Data = #data{status = Status, queue = Queue, procs = NumProcs}) ->
    NewStatus = maps:update_with(From, fun(_) -> {Act, Con} end, {Act, Con}, Status),
    case not gb_trees:is_empty(Queue) andalso NumProcs == maps:size(NewStatus) of
	true ->
	    %io:format("~p vs ~p~n",[NumProcs, maps:size(NewStatus)]),
	    {keep_state, Data#data{status = NewStatus}, [{next_event, internal, receiving}]};
	false ->
	    %io:format("~p vs ~p~n",[NumProcs, maps:size(NewStatus)]),
	    {keep_state, Data#data{status = NewStatus}}
    end;

handle_event(cast, {data, {Id, Msg}}, _State,
	     Data = #data{queue = Queue, status = Status, procs = NumProcs, counter = Counter}) ->
    NewQueue = gb_trees:insert(Id, Msg, Queue),  % does it really need to add to the queue?
    case NumProcs == maps:size(Status) andalso NumProcs > 0 of
	true ->
	    {keep_state, Data#data{queue = NewQueue},
	     [{next_event, internal, receiving}]};
	false ->
	    {keep_state, Data#data{queue = NewQueue}}
    end;

handle_event(cast, {timestamp, Id}, {Env,Sending}, Data = #data{counter = Counter, status = Status, procs = NumProcs, mid = Mid, interface = I}) ->
%   io:format("===~p received id ~p when procs = ~p, mapsize ~p ~n",[maps:with(I,Env),Id,NumProcs, maps:size(Status)]),
    Resend = maps:size(maps:filter(fun(_,S) -> S =:= sending end, Status)),
    case Id == Counter of
	true ->
%	    io:format("msg forwarded, hanlde ~p ~n",[NumProcs]),
	    NewSending = not Sending,
	    {next_state, {Env, NewSending}, Data#data{resend = Resend, mid = Id}};
	false ->
%	    io:format("msg forwarded, no ~p ~n",[NumProcs]),
	    {keep_state, Data#data{mid = Id}}
    end;


%% handle_event(internal, receiving, {Env, Sending},
%% 	     Data = #data{status = Status, mid = Mid, counter = Counter}) when Mid == Counter ->
%%     %% Resend = maps:size(maps:filter(fun(_,S) -> S =:= sending end, Status)),
%%     %% {keep_state, Data#data{resend = Resend}};
%%    % keep_state_and_data;
%%     {next_state, {Env, Sending}, Data};
handle_event(internal, receiving, {Env, Sending}, Data = #data{queue = Queue, procs = NumProcs, status = Status, mid = Mid, interface = I,  resend = Resend, counter = Counter}) ->
    %% check if there any procs can receive this message
    case gb_trees:is_empty(Queue) orelse element(1,gb_trees:smallest(Queue)) /= Counter orelse NumProcs /= maps:size(Status) of
	true ->
	    %io:format("trap ~p ~n",[maps:with(I,Env)]),
	    {next_state, {Env, Sending}, Data};
	    %keep_state_and_data;
	false ->
	    {_, Msg, Rest} = gb_trees:take_smallest(Queue),
	    Result = dispatch_internal(Msg, maps:to_list(Status), Env),
	    case Result of
		[] ->
		    %MContent = element(2,Msg),
		    %attd(I,Env,MContent),
		    NewSending = if (Counter + 1 == Mid) -> not Sending; true -> Sending end, %state change
		    NewResend = if (Counter + 1 == Mid) ->  maps:size(maps:filter(fun(_,S) -> S =:= sending end, Status)); true -> Resend end,
		    handle_event(internal, receiving, {Env, NewSending}, Data#data{queue = Rest, resend = NewResend, counter = Counter + 1});
		[{From, U, Con, X}] ->
		    %%discard_message_and_reply
		    MContent = element(2,Msg),
		    NewEnv = update_input(Env,U,MContent),
		    %attin(I,NewEnv,MContent),
		    gen_statem:reply(From, {Con,zip(X,MContent)}),
		    NewStatus = maps:remove(From, Status),
		    NewSending = if (Counter + 1 == Mid) -> not Sending; true -> Sending end, %state change
		    NewResend = if (Counter + 1 == Mid) ->  maps:size(maps:filter(fun(_,S) -> S =:= sending end, Status)); true -> Resend end,
		    {next_state, {NewEnv, NewSending}, Data#data{queue = Rest, status = NewStatus, resend = NewResend, counter = Counter + 1}}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
		       any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
	OldVsn :: term() | {down,term()},
	State :: term(), Data :: term(), Extra :: term()) ->
			 {ok, NewState :: term(), NewData :: term()} |
			 (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dispatch_internal(_, [], _) ->
    []; % no process can receive the message
dispatch_internal(Msg, [{_,sending} | T], REnv) -> % not receiving to sending process
    dispatch_internal(Msg, T, REnv);
dispatch_internal({SPred, MsgContent, SEnv} = Msg, [{From, List} | T], REnv) when is_list(List) -> % a set of receive actions registered by a choice process
    case lists:dropwhile(fun({{G,RPred, X, _},_}) ->
				 try not (tuple_size(MsgContent) == tuple_size(X) andalso G(REnv) andalso RPred(REnv,MsgContent,SEnv) andalso SPred(REnv)) of
				 Check -> Check
				 catch
				     _:_ -> true %% type error
				 end
			 end,
			 List) of
	[] ->
	    dispatch_internal(Msg, T, REnv);
	[{{_,_,X,U},Con} |_] ->
	   % io:format("GOES HERE ~n"),
	    [{From, U, Con, X}]
    end;
% ordinary receive
dispatch_internal({SPred, MsgContent, SEnv} = Msg, [{From, {{G, RPred, X, U}, Con}}|T], REnv) ->
    try tuple_size(X) == tuple_size(MsgContent) andalso G(REnv) andalso RPred(REnv, MsgContent, SEnv) andalso SPred(REnv) of
	true ->
	    [{From, U, Con, X}];
	false ->
	    dispatch_internal(Msg,T, REnv)
    catch
	_:_ -> dispatch_internal(Msg,T, REnv)
    end;
dispatch_internal(A,B,C) ->
    io:format("other clause that shouldnt ~p ~p ~p ~n", [A,B,C]).


partial_eval(Pred, Env) when is_function(Pred) ->
    F = fun(E1) ->
		fun(E2) ->
			Pred(E1,E2)

		end
	end,
    F(Env);
partial_eval(Msg, Env) when is_tuple(Msg) ->
    F = fun(X) ->
		if is_function(X) ->
			X(Env);
		   true ->
			X
		end
	end,
    list_to_tuple(lists:map(F, tuple_to_list(Msg))).

update_output(Env,U) ->
    lists:foldl(fun({X,V},Sum) ->
			EvalV =
			    case is_function(V) of
				true -> V(Env);
				false -> V
			    end,
			maps:update(X, EvalV, Sum)
		end, Env, U).
update_input(Env,U,M) ->
    lists:foldl(fun({X,V},Sum) ->
			EvalV =
			    case is_function(V) of
				true -> V(Env,M);
				false -> V
			    end,
			maps:update(X, EvalV, Sum)
		end, Env, U).


attout(I,E,M) ->
    C = maps:with(I,E),
    io:format("Comp ~p sent ~p ~n",[C,M]).

attin(I,E,M) ->
    C = maps:with(I,E),
    io:format("Comp ~p accepted ~p ~n",[C,M]).

attd(I,E,M) ->
    C = maps:with(I,E),
    io:format("Comp ~p discarded ~p ~n",[C,M]).

zip(V,M) ->
    lists:zip(tuple_to_list(V),tuple_to_list(M)).
