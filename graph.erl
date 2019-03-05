-module(graph).
-import(abel,[send/2,recv/2,choice/2,parallel/2,att/2,this/1]).
-export([d/1,f/1,a/1,t/1]).


f(C) ->
    %% define messages, predicates, updates
    AP = fun(E) -> att(send,E) andalso not att(assigned, E) end,
    Msg = {tryc, fun(E) -> min_color(att(Used,E)) end, this(round)},
    SP = fun(E1, E2) -> lists:member(att(id,E1), att(nlist, E2)) end,
    U = [{send, false}, {color, fun(E) -> min_color(att(used,E)) end}],
    Act = {AP, Msg, SP, U),

    send(C, Act),
    f(C).

t(C) ->
    Act1 = {fun(E1, E2, Msg) -> size(Msg) == 3 andalso
				   element(1,Msg) == tryc andalso
				   att(id,E1) > att(id,E2) andalso
				   att(round,E1) == element(3,Msg)
		       end,
	    [{counter, fun(E,_) -> att(counter,E) + 1 end}]
	   },
    Act2 = {fun(E1, E2, Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == tryc andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) == element(3,Msg)
		  end,
	    [{counter, fun(E,_) -> att(counter, E) + 1 end},
	     {constraints, fun(E,Msg) -> sets:add_element(element(2,Msg),att(constraints,E)) end}]
	   },
    Act3 = {fun(E1, E2, Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == tryc andalso
			      att(id,E1) > att(id,E2) andalso
			      att(round,E1) < element(3,Msg)
		  end,
	    [{round, fun(_,Msg) -> element(3,Msg) end}, {send, true}, {counter, 1},
	     {constraints, sets:new()}]
	   },
    Act4 = {fun(E1, E2, Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == tryc andalso
			      att(id,E1) < att(id,E2) andalso
			      att(round,E1) < element(3,Msg)
		  end,
	    [{round, fun(_,Msg) -> element(3,Msg) end}, {send, true}, {counter, 1},
	     {constraints, fun(_,Msg) -> sets:add_element(element(2,Msg),sets:new()) end}]
	   },
    choice(C, [{Act1, fun() -> t(C) end},
	       {Act2, fun() -> t(C) end},
	       {Act3, fun() -> t(C) end},
	       {Act4, fun() -> t(C) end}
	      ]).
d(C) ->
    Act1 = {fun(E1,E2,Msg) ->
		    size(Msg) == 3 andalso
			element(1,Msg) == done andalso
			att(round, E1) >= element(3,Msg)
		  end,
	    [{done, fun(E,_) -> att(done,E) + 1 end},
	     {used, fun(E,Msg) -> sets:add_element(element(2,Msg),att(used,E)) end}]
	   },
    Act2 = {fun(E1,E2,Msg) ->
			  size(Msg) == 3 andalso
			      element(1,Msg) == done andalso
			      this(round,E1) < element(3,Msg)
		  end,
	    [{round, fun(_,Msg) -> element(3,Msg) end},
	     {done, fun(E,_) -> this(done,E) + 1 end},
	     {constraints, sets:new()},
	     {send, true},
	     {counter, 0},
	     {used, fun(E,Msg) -> sets:add_element(element(2,Msg),att(used,E)) end}]
	   },
    choice(C, [{Act1, fun() -> d(C) end}, {Act2, fun() -> d(C) end}]).

a(C) ->
    Act = {fun(E) ->
			att(color,E) > 0 andalso
			    att(counter, E) == length(att(nlist, E)) - att(done,E)
			    andalso not
			    sets:is_element(this(color,E), sets:union(att(constraints,E),att(used,E)))
	      end,
	      {done, this(color), fun(E) -> att(round,E) + 1 end},
	      fun(E1,E2) ->
		      lists:member(att(id,E1), att(nlist,E2))
	      end,
	      [{assigned, true}]},
    send(C,Act).


%% helper functions
min_color(L) ->
    min_color(1,lists:usort(sets:to_list(L))).

min_color(V, []) -> V;
min_color(V, [H | _]) when V < H ->
    V;
min_color(V, [H | T]) when V == H ->
    min_color(V + 1, T).
