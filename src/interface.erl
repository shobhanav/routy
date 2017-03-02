%% @author eavnvya
%% @doc @todo Add description to interface.


-module(interface).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new()->
	[].

add(Name, Ref, Pid, Intf) ->
	[{Name,Ref,Pid}|Intf].

remove(Name, Intf)  ->
	lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
	case lists:keysearch(Name, 1, Intf) of
		{value, {_,_,_Pid}} ->
			{ok,_Pid};
		false ->
			notfound
	end.

ref(Name, Intf) ->
	case lists:keysearch(Name, 1, Intf) of
		{value, {_,_Ref,_}} ->
			{ok,_Ref};
		false ->
			notfound
	end.

name(Ref, Intf) ->
	case lists:keysearch(Ref, 2, Intf) of
		{value, {_Name,_,_}} ->
			{ok,_Name};
		false ->
			notfound
	end.

list(Intf) ->
	lists:map(fun({Name,_,_}) -> Name end, Intf).

broadcast(Message, Intf) ->
	lists:map(fun({_,_,Pid}) -> Pid ! Message end, Intf).
	

%% ====================================================================
%% Internal functions
%% ====================================================================


