-module(vertex).
-import(abel,[send/2,recv/2,choice/2,parallel/2,att/2,this/1]).
-export([d/1,f/1,a/1,t/1]).

%% Process F
f(C) ->
    %% define messages, predicates, updates
    AP = fun(E) -> att(send,E) andalso not att(assigned, E) end,
    Msg = {'try', fun(E) -> min_colour(att(used,E)) end, this(round)},
    SP = fun(E1, E2) -> lists:member(att(id,E1), att(nlist, E2)) end,
    U = [{send, false}, {colour, fun(E) -> min_colour(att(used,E)) end}],
    Act = {AP, Msg, SP, U},

    send(C, Act),
    f(C).

%% Process T
t(C) ->
    RP1 = fun(E1, E2, Msg) -> size(Msg) == 3 andalso
				   element(1,Msg) == 'try' andalso
				   att(id,E1) > att(id,E2) andalso
				   att(round,E1) == element(3,Msg)
		       end,
    U1 = [{counter, fun(E,_) -> att(counter,E) + 1 end}],

    RP2 = fun(E1, E2, Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == 'try' andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) == element(3,Msg)
		  end,
    U2  = [{counter, fun(E,_) -> att(counter, E) + 1 end},
	   {constraints, fun(E,Msg) -> sets:add_element(element(2,Msg),att(constraints,E)) end}],

    RP3 = fun(E1, E2, Msg) ->
		  size(Msg) == 3 andalso
		      element(1,Msg) == 'try' andalso
		      att(id,E1) > att(id,E2) andalso
		      att(round,E1) < element(3,Msg)
		  end,
    U3 = [{round, fun(_,Msg) -> element(3,Msg) end}, {send, true}, {counter, 1},
	  {constraints, sets:new()}],
    RP4 = fun(E1, E2, Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == 'try' andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) < element(3,Msg)
		  end,
    U4 = [{round, fun(_,Msg) -> element(3,Msg) end}, {send, true}, {counter, 1},
	  {constraints, fun(_,Msg) -> sets:add_element(element(2,Msg),sets:new()) end}],

    Act1 = {RP1,U1},
    Act2 = {RP2,U2},
    Act3 = {RP3,U3},
    Act4 = {RP4,U4},

    TRef = fun() -> t(C) end,

    choice(C, [{Act1, TRef},{Act2, TRef},{Act3, TRef},{Act4, TRef}]).

%% Process D
d(C) ->
    RP1 = fun(E1,E2,Msg) ->
		    size(Msg) == 3 andalso
			element(1,Msg) == 'done' andalso
			att(round, E1) >= element(3,Msg)
		  end,
    U1 = [{done, fun(E,_) -> att(done,E) + 1 end},
	  {used, fun(E,Msg) -> sets:add_element(element(2,Msg),att(used,E)) end}],
    RP2 = fun(E1,E2,Msg) ->
		  size(Msg) == 3 andalso
		      element(1,Msg) == 'done' andalso
		      att(round,E1) < element(3,Msg)
	  end,
    U2 = [{round, fun(_,Msg) -> element(3,Msg) end},
	     {done, fun(E,_) -> att(done,E) + 1 end},
	     {constraints, sets:new()},
	     {send, true},
	     {counter, 0},
	     {used, fun(E,Msg) -> sets:add_element(element(2,Msg),att(used,E)) end}],

    Act1 = {RP1,U1},
    Act2 = {RP2,U2},
    DRef = fun() -> d(C) end,
    choice(C, [{Act1, DRef}, {Act2, DRef}]).

%% Process A
a(C) ->
    AP = fun(E) -> att(colour,E) > 0 andalso
		     att(counter, E) == length(att(nlist, E)) - att(done,E)
		     andalso not
		     sets:is_element(att(colour,E), sets:union(att(constraints,E),att(used,E)))
	 end,
    Msg = {'done', this(colour), fun(E) -> att(round,E) + 1 end},
    SP = fun(E1,E2) ->
		 lists:member(att(id,E1), att(nlist,E2))
	 end,
    U = [{assigned, true}],
    Act = {AP,Msg,SP,U},
    send(C,Act).




%% helper functions
min_colour(L) ->
    min_colour(1,lists:usort(sets:to_list(L))).

min_colour(V, []) -> V;
min_colour(V, [H | _]) when V < H ->
    V;
min_colour(V, [H | T]) when V == H ->
    min_colour(V + 1, T).
