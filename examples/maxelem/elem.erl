-module(elem).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(_C,V) ->
    parallel(_C,V,[
	fun(_V) -> a(_C,_V) end,
	fun(_V) -> b(_C,_V) end
    ]).

a(_C,V) ->
    prefix(_C,V,{{fun(_LclE) -> att(s,_LclE) == 1 end,
	{fun(_LclE) -> att(n,_LclE) end},
	fun(_LclE, _RmtE) -> true end,
	[]},
	nil}
	).

b(_C,V) ->
    prefix(_C,V,{{fun(_LclE) -> true end,
		  fun(_LclE, _M, _RmtE) -> msg(1,_M) >= att(n,_LclE) end,
	{x},
	[{s, fun(_LclE,_M) -> 0 end}]},
	nil}
	).
