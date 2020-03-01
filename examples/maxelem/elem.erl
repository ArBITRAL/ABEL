-module(elem).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(C,V) -> 
    call(C,V,fun(_V) -> p(C,_V) end).

a(C,V) -> 
    prefix(C,V,{{fun(_L) -> att(s,_L) == true end,
	{fun(_L) -> att(n,_L) end},
	fun(_L, _R) -> true end,
	[]},
	nil}
	).

b(C,V) -> 
    prefix(C,V,{{fun(_L) -> true end,
	fun(_L, _M, _R) -> msg(1,_M) >= att(n,_L) end,
	{x},
	[{s, fun(_L,_M) -> false end}]},
	nil}
	).

p(C,V) -> 
    parallel(C,V,[
	fun(_V) -> a(C,_V) end,
	fun(_V) -> b(C,_V) end
    ]).

