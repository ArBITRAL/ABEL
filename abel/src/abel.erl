%%% interface module
-module(abel).

-export([start/1,start/2]).

-export([new_component/3,start_component/1]).

-export([send/2,recv/2,choice/2,parallel/2]).

-export([this/1,att/2]).

%% Infrastructure
start(T) ->
    start(T,binary).

start(T,Mode) ->
    abel_sup:start_link(T), % start the infrastructure
    ok = abel_reg:create_tree(Mode).  % shape a binary tree of T nodes

%% Components
new_component(M, Env, I) ->
    {ok, C} = abel_coord:start_component(M, Env, I),
    C. %separate this two for handling massive initializations

start_component(C) ->
    abel_coord:start_beh(C).

%% Processes
%% send & receive interface
send(C, Act) ->
    OutAct = pretty_format(Act),
    Ack = abel_coord:action(C, OutAct),
    Ack.

recv(C, Act) ->
    InAct = pretty_format(Act),
    Ack = abel_coord:action(C, InAct),
    Ack.

%% chocice
choice(C, BehList) ->
    BList2 = [{pretty_format(Act),Con} || {Act,Con} <- BehList],
    {Ret, F} = abel_coord:choice(C,BList2),
    if not is_atom(F) ->
	    case erlang:fun_info(F,arity) of
		{_, 0} -> F();
		_ -> F(Ret)
	    end;
       true -> ok
    end.
%% interleaving
parallel(C, L) when is_list(L) ->
    abel_coord:par(C, L).

%% attributes helpfers
this(A) when is_atom(A) ->
    fun(E) ->
	    maps:get(A,E)
    end.

att(A,E) ->
    maps:get(A,E).



%% internal formatting functions
pretty_format({Msg,Pred}) when is_tuple(Msg) andalso is_function(Pred) ->
    {send, fun(_) -> true end, Msg, Pred, []};
pretty_format({G,Msg,Pred}) when is_function(G) andalso is_function(Pred) andalso is_tuple(Msg) ->
    {send, G, Msg, Pred, []};
pretty_format({Msg,Pred,U}) when is_tuple(Msg) andalso is_function(Pred) andalso is_list(U) ->
    {send, fun(_) -> true end, Msg, Pred, U};
pretty_format({G,Msg,Pred,U}) when is_function(G) andalso is_tuple(Msg) andalso is_function(Pred) andalso is_list(U) ->
    {send,G,Msg,Pred,U};
pretty_format(Pred) when is_function(Pred) ->
    {recv, fun(_) -> true end, Pred, []};
pretty_format({G,Pred}) when is_function(G) andalso is_function(Pred) ->
    {recv, G, Pred, []};
pretty_format({Pred,U}) when is_function(Pred) andalso is_list(U) ->
    {recv, fun(_) -> true end, Pred, U};
pretty_format({G,Pred,U}) when is_function(G) andalso is_function(Pred) andalso is_list(U)  ->
    {recv,G,Pred,U}.
