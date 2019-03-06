-module(vertex2).
-import(abel,[send/2,recv/2,choice/2,parallel/2,att/2,this/1]).

%% to be called by coordinator
-export([init_beh/1]).

init_beh(C) ->
    F = fun() -> f(C) end,
    T = fun() -> t(C) end,
    D = fun() -> d(C) end,
    A = fun() -> a(C) end,
    [F,T,D,A].

%%%%% Main Code
f(C) ->
    M = {'try', fun(E) -> min_colour(att(used, E)) end, this(round)},
    SP = fun(S, R) -> lists:member(att(id,S), att(nbr, R)) end,
    U = [{counter, fun(E) -> att(counter, E) + 1 end}, {colour, fun(E) -> min_colour(att(used,E)) end}],

    Act = {M,SP,U},
    send(C, Act).

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
    U2 =  [{counter, fun(E,_) -> att(counter, E) + 1 end},
	   {constraints, fun(E,M) -> sets:add_element(element(2,M),att(constraints,E)) end}],

    RP3 = fun(E1, E2, M) ->
			  size(M) == 3 andalso
			      element(1,M) == 'try' andalso
			      att(id,E1) > att(id,E2) andalso
			      att(round,E1) < element(3,M)
		  end,
    U3 =[{counter1, fun(E,_) -> att(counter1,E) + 1 end}],

    RP4 = fun(E1, E2, M) ->
			  size(M) == 3 andalso
			      element(1,M) == 'try' andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) < element(3,M)
		  end,
    U4 = [{counter1, fun(E,_) -> att(counter1,E) + 1 end},
	  {constraints1, fun(E,M) -> sets:add_element(element(2,M),att(constraints1,E)) end}],

    Act1 = {RP1,U1},
    Act2 = {RP2,U2},
    Act3 = {RP3,U3},
    Act4 = {RP4,U4},

    TRef = fun() -> t(C) end,
    choice(C, [{Act1, TRef},{Act2, TRef},{Act3, TRef},{Act4, TRef}]).

d(C) ->
    RP1 = fun(E,_,M) ->
		    size(M) == 3 andalso
			element(1,M) == 'done' andalso
			att(round, E) == element(3,M)
		  end,
    U1 = [{done, fun(E,_) -> att(done,E) + 1 end},
	  {used, fun(E,M) -> sets:add_element(element(2,M),att(used,E)) end}],

    RP2 = fun(E,_,M) ->
			  size(M) == 3 andalso
			      element(1,M) == 'done' andalso
			      att(round,E) > element(3,M)
		  end,
    U2 = [{done, fun(E,_) -> att(done,E) + 1 end},
	     {counter, fun(E,_) -> att(counter,E) + 1 end},
	     {used, fun(E,M) -> sets:add_element(element(2,M),att(used,E)) end}],

    Act1 = {RP1, U1},
    Act2 = {RP2, U2},
    DRef = fun() -> d(C) end,

    choice(C, [{Act1,DRef}, {Act2, DRef}]).

a(C) ->
    G1 =  fun(E) ->
			att(assigned,E) == false andalso
			    att(counter, E) == length(att(nbr, E)) + 1
			    andalso not
			    sets:is_element(att(colour,E), sets:union(att(constraints,E),att(used,E)))
	      end,
    M1 = {'done', this(colour), this(round)},
    SP1 = fun(E1,E2) -> lists:member(att(id,E1), att(nbr,E2)) end,
    U1 = [{assigned, true}],

    G2 = fun(E) -> att(assigned,E) == false andalso
			    att(counter, E) == length(att(nbr, E)) + 1
			    andalso
			    sets:is_element(att(colour,E), sets:union(att(constraints,E),att(used,E)))
	      end,
    M2 = {'try', fun(E) ->  min_colour(sets:union(att(constraints,E),att(used,E))) end, fun(E) -> att(round,E) + 1 end},
    SP2 =  fun(E1, E2) -> lists:member(att(id,E1), att(nbr, E2)) end,

    U2 = [{round, fun(E) -> att(round,E) + 1 end},
	  {colour, fun(E) ->  min_colour(sets:union(att(constraints,E),att(used,E)))  end},
	  {constraints, fun(E) -> att(constraints1,E) end},
	  {counter, fun(E) -> att(counter1,E) + att(done,E) + 1 end},
	  {constraints1, sets:new()},
	  {counter1, 0}],

    Act1 = {G1,M1,SP1,U1},
    Act2 = {G2,M2,SP2,U2},

    ARef = fun() -> a(C) end,
    choice(C,[{Act1,nil},{Act2,ARef}]).



%% helper functions
min_colour(L) ->
    min_colour(1,lists:usort(sets:to_list(L))).

min_colour(V, []) -> V;
min_colour(V, [H | _]) when V < H ->
    V;
min_colour(V, [H | T]) when V == H ->
    min_colour(V + 1, T).
