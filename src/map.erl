%% @author eavnvya
%% @doc @todo Add description to map.


-module(map).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
	[].

update(Node, Links, Map) ->
	case lists:keysearch(Node, 1, Map) of
		{value, {_,_}} ->
			[{Node, Links}|lists:keydelete(Node, 1, Map)];
		false ->
			[{Node, Links}|Map]
	end.		
		
reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		false ->
			[];
		{H,T} ->
			T
	end.
	
	

all_nodes(Map) ->
	Acc0 = [],	
	lists:foldl(fun(Elem, Acc) -> parseElement(Elem, Acc) end, Acc0, Map).


			
parseElement({Node, Links}, Acc) ->
	lists:foldl(fun(Elem, NodeList) ->
						case lists:member(Elem, NodeList) of
							false ->
								[Elem|NodeList];
							true ->
								NodeList
						end end, Acc, [Node|Links]).


	
	
	
				

%% ====================================================================
%% Internal functions
%% ====================================================================


