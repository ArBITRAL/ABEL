-module(maxelem).

-import(abel,[new_component/3,start_component/1]).

-export([start/1]).

start(N) ->
    ok = abel:start(N),
    Common = #{s => true},
    I = {n,s},
    CompAddresses = lists:foldr(fun(X, Acc) ->
					Env = maps:merge(#{n => X},Common),
					Comp = new_component(elem, Env, I),
					[Comp | Acc]
				end, [], [3,2,1,4,5,6]),
    %% Start execution
    [start_component(C) || C <- CompAddresses],
    ok.
