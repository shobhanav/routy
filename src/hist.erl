%% @author eavnvya
%% @doc @todo Add description to hist.


-module(hist).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, update/3]).

new(Name) ->
	[{Name, inf}].

update(Name, X, Msg)->
    case lists:keysearch(Name, 1, Msg) of
	{value, {Name, Y}} ->
	    if 
		X > Y -> 
		    {new, [{Name, X}|lists:keydelete(Name, 1, Msg)]};
		true -> 
		    old
	    end;
	false ->
	    {new, [{Name, X}|lists:keydelete(Name, 1, Msg)]}
    end.

			

%% ====================================================================
%% Internal functions
%% ====================================================================


