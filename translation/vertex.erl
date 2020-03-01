-module(vertex).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(C,V) -> 
    parallel(C,V,[
	fun(_V) -> f(C,_V) end,
	fun(_V) -> t(C,_V) end,
	fun(_V) -> d(C,_V) end,
	fun(_V) -> a(C,_V) end
    ]).

f(C,V) -> 
    prefix(C,V,{{fun(_L) -> true end,
	{fun(_L) -> 'try' end,fun(_L) -> att(color,_L) end,fun(_L) -> att(round,_L) end},
	fun(_L, _R) -> sets:is_element(att(id,_R),att(nbr,_L)) end,
	[{counter, fun(_L) -> att(counter,_L) + 1 end}]},
	nil}
	).

t(C,V) -> 
    choice(C,V,[
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == 'try' andalso att(id,_L) > att(id,_R) andalso att(round,_L) == msg(3,_M) end,
	{x,y,z},
	[{counter, fun(_L,_M) -> att(counter,_L) + 1 end}]},
	fun(_V) -> t(C,[]) end},
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == 'try' andalso att(id,_L) < att(id,_R) andalso att(round,_L) == msg(3,_M) end,
	{x,y,z},
	[{counter, fun(_L,_M) -> att(counter,_L) + 1 end},
	{constraints, fun(_L,_M) -> sets:union(att(constraints,_L),[msg(2,_M)]) end}]},
	fun(_V) -> t(C,[]) end},
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == 'try' andalso att(id,_L) > att(id,_R) andalso att(round,_L) < msg(3,_M) end,
	{x,y,z},
	[{counter1, fun(_L,_M) -> att(counter1,_L) + 1 end}]},
	fun(_V) -> t(C,[]) end},
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == 'try' andalso att(id,_L) < att(id,_R) andalso att(round,_L) < msg(3,_M) end,
	{x,y,z},
	[{counter1, fun(_L,_M) -> att(counter1,_L) + 1 end},
	{constraints1, fun(_L,_M) -> sets:union(att(constraints1,_L),[msg(2,_M)]) end}]},
	fun(_V) -> t(C,[]) end}
    ]).

d(C,V) -> 
    choice(C,V,[
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == done andalso att(round,_L) == msg(3,_M) end,
	{x,y,z},
	[{done, fun(_L,_M) -> att(done,_L) + 1 end},
	{used, fun(_L,_M) -> sets:union(att(used,_L),[msg(2,_M)]) end}]},
	fun(_V) -> d(C,[]) end},
	{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) == done andalso att(round,_L) > msg(3,_M) end,
	{x,y,z},
	[{done, fun(_L,_M) -> att(done,_L) + 1 end},
	{counter, fun(_L,_M) -> att(counter,_L) + 1 end},
	{used, fun(_L,_M) -> sets:union(att(used,_L),[msg(2,_M)]) end}]},
	fun(_V) -> d(C,[]) end}
    ]).

a(C,V) -> 
    choice(C,V,[
	{{fun(_L) -> (sets:size(att(nbr,_L)) + 1 == att(counter,_L)) andalso (not sets:is_element(att(color,_L),sets:union(att(constraints,_L),att(used,_L)))) end,
	{fun(_L) -> done end,fun(_L) -> att(color,_L) end,fun(_L) -> att(round,_L) end},
	fun(_L, _R) -> sets:is_element(att(id,_R),att(nbr,_L)) end,
	[{assigned, fun(_L) -> true end}]},
	nil},
	{{fun(_L) -> (sets:size(att(nbr,_L)) + 1 == att(counter,_L)) andalso (sets:is_element(att(color,_L),sets:union(att(constraints,_L),att(used,_L)))) end,
	{fun(_L) -> 'try' end,fun(_L) -> att(color,_L) end,fun(_L) -> att(round,_L) + 1 end},
	fun(_L, _R) -> sets:is_element(att(id,_R),att(nbr,_L)) end,
	[{round, fun(_L) -> att(round,_L) + 1 end},
	{counter, fun(_L) -> att(done,_L) + att(counter1,_L) + 1 end},
	{constraints, fun(_L) -> att(constraints1,_L) end},
	{counter1, fun(_L) -> 0 end},
	{constraints1, fun(_L) -> [] end}]},
	fun(_V) -> a(C,[]) end}
    ]).

