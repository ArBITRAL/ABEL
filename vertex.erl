-module(vertex).
-import(abel,[send/2,recv/2,choice/2,parallel/2,att/2,this/1]).

%% to be called by coordinator
-export([init_beh/1]).

init_beh(C) ->
    F = fun() -> f(C) end,
    T = fun() -> t(C) end,
    D = fun() -> d(C) end,
    A = fun() -> a(C) end,
    [F,T,D,A].

%% Process F
f(C) ->
    %% define messages, predicates, updates
    G = fun(E) -> att(send,E) andalso not att(assigned, E) end,
    M = {'try', fun(E) -> min_colour(att(used,E)) end, this(round)},
    SP = fun(E1, E2) -> lists:member(att(id,E1), att(nbr, E2)) end,
    U = [{send, false}, {colour, fun(E) -> min_colour(att(used,E)) end}],
    Act = {G, M, SP, U},

    send(C, Act),
    f(C).

%% Process T
t(C) ->
    RP1 = fun(E1, E2, M) -> size(M) == 3 andalso
				   element(1,M) == 'try' andalso
				   att(id,E1) > att(id,E2) andalso
				   att(round,E1) == element(3,M)
		       end,
    U1 = [{counter, fun(E,_) -> att(counter,E) + 1 end}],

    RP2 = fun(E1, E2, M) ->
			  size(M) == 3 andalso
			      element(1,M) == 'try' andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) == element(3,M)
		  end,
    U2  = [{counter, fun(E,_) -> att(counter, E) + 1 end},
	   {constraints, fun(E,M) -> sets:add_element(element(2,M),att(constraints,E)) end}],

    RP3 = fun(E1, E2, M) ->
		  size(M) == 3 andalso
		      element(1,M) == 'try' andalso
		      att(id,E1) > att(id,E2) andalso
		      att(round,E1) < element(3,M)
		  end,
    U3 = [{round, fun(_,M) -> element(3,M) end}, {send, true}, {counter, 1},
	  {constraints, sets:new()}],
    RP4 = fun(E1, E2, M) ->
			  size(M) == 3 andalso
			      element(1,M) == 'try' andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) < element(3,M)
		  end,
    U4 = [{round, fun(_,M) -> element(3,M) end}, {send, true}, {counter, 1},
	  {constraints, fun(_,M) -> sets:add_element(element(2,M),sets:new()) end}],

    Act1 = {RP1,U1},
    Act2 = {RP2,U2},
    Act3 = {RP3,U3},
    Act4 = {RP4,U4},

    TRef = fun() -> t(C) end,

    choice(C, [{Act1, TRef},{Act2, TRef},{Act3, TRef},{Act4, TRef}]).

%% Process D
d(C) ->
    RP1 = fun(E1,_,M) ->
		    size(M) == 3 andalso
			element(1,M) == 'done' andalso
			att(round, E1) >= element(3,M)
		  end,
    U1 = [{done, fun(E,_) -> att(done,E) + 1 end},
	  {used, fun(E,M) -> sets:add_element(element(2,M),att(used,E)) end}],
    RP2 = fun(E1,_,M) ->
		  size(M) == 3 andalso
		      element(1,M) == 'done' andalso
		      att(round,E1) < element(3,M)
	  end,
    U2 = [{round, fun(_,M) -> element(3,M) end},
	     {done, fun(E,_) -> att(done,E) + 1 end},
	     {constraints, sets:new()},
	     {send, true},
	     {counter, 0},
	     {used, fun(E,M) -> sets:add_element(element(2,M),att(used,E)) end}],

    Act1 = {RP1,U1},
    Act2 = {RP2,U2},
    DRef = fun() -> d(C) end,

    choice(C, [{Act1, DRef}, {Act2, DRef}]).

%% Process A
a(C) ->
    G = fun(E) -> att(colour,E) > 0 andalso
		     att(counter, E) == length(att(nbr, E)) - att(done,E)
		     andalso not
		     sets:is_element(att(colour,E), sets:union(att(constraints,E),att(used,E)))
	 end,
    M = {'done', this(colour), fun(E) -> att(round,E) + 1 end},
    SP = fun(E1,E2) ->
		 lists:member(att(id,E1), att(nbr,E2))
	 end,
    U = [{assigned, true}],

    Act = {G,M,SP,U},
    send(C,Act).




%% helper functions
min_colour(L) ->
    min_colour(1,lists:usort(sets:to_list(L))).

min_colour(V, []) -> V;
min_colour(V, [H | _]) when V < H ->
    V;
min_colour(V, [H | T]) when V == H ->
    min_colour(V + 1, T).
