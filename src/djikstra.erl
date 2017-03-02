%% @author eavnvya
%% @doc @todo Add description to djikstra.


-module(djikstra).

%% ====================================================================
%% API functions
%% ====================================================================
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

entry(Node, Sorted) ->
	case lists:keysearch(Node, 1, Sorted) of
		{value,{_,Len,_}} ->
			Len;
		false->
			0
	end.

replace(Node, N, Gateway, Sorted) ->
	lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node,N,Gateway})).	


update(Node, N, Gateway, Sorted) ->
	Existing = entry(Node, Sorted),
	if 
		N < Existing ->
			replace(Node, N, Gateway, Sorted);
		true ->
			Sorted
	end.

iterate([], _Map, Table) ->
	Table;
iterate([{_,inf,_}|_], _Map, Table) ->
	Table;
iterate([{Dest,Len,Nhop}|Tail], _Map, Table) ->
	Connected = map:reachable(Dest, _Map),
	Updated = lists:foldl(fun(_Elem, Acc) ->
						update(_Elem,Len+1,Nhop,Acc) end, Tail, Connected),
	iterate(Updated, _Map, [{Dest,Nhop}|Table]).


table(Gws, Map) ->
	Nodes = map:all_nodes(Map),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gws) end, Nodes),
    Direct = lists:map(fun (Nd) -> {Nd,0,Nd} end, Gws),
    Indirect = lists:map(fun (Nd) -> {Nd,inf,unknown} end, Rest),
    Sorted = lists:append(Direct, Indirect),
    iterate(Sorted, Map, []).

route(Node, Table) ->
	case lists:keysearch(Node, 1, Table) of
	{value, {_, unknown}} ->
	    notfound;
	{value, {_, Gw}} ->
	    {ok, Gw};
	false ->
	    notfound
    end.

	
%% ====================================================================
%% Internal functions
%% ====================================================================


