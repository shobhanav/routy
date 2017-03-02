% @author eavnvya
%% @doc @todo Add description to routy.


-module(routy).
-export([start/2, stop/1, init/1, router/6, displayStatus/1]).

start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)),
	io:format("router process started ~w~n",Reg).


stop(Node) ->
	Node ! stop.	


init(Name) ->
	Intf = interface:new(),
	Map = map:new(),
	Table = djikstra:table(Intf, Map),
	Hist = hist:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
	receive		
		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = interface:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		
		{remove, Node} ->
			{ok, Ref} = interface:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = interface:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		
		{DOWN, Ref, process,_,_} ->
			{ok, Down} = interface:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = interface:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		
		{status, From} ->			
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);
		
		{links, Node, R, Links} ->
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					interface:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					router(Name, N, Hist1, Intf, Table, Map1);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;
		
		update ->
			Table1 = djikstra:table(interface:list(Intf), Map),
			router(Name, N, Hist, Intf, Table1, Map);
		
		broadcast ->
			Message = {links, Name, N, interface:list(Intf)},
			interface:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);
		
		{route, Name, From, Message} ->
			io:format("~w: received message ~w~n", [Name, Message]),
			router(Name, N, Hist, Intf, Table, Map);
		
		{route, To, From, Message} ->
			io:format("~w: routing message (~w)", [Name, Message]),
			case djikstra:route(To, Table) of
				{ok, Gw} ->
					case interface:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! {route, To, From, Message};
						notfound ->
							ok
					end;
				notfound ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);
		
		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map);
					
		stop ->
			ok
	end.

displayStatus(Router) ->
    Router ! {status, self()},	
    receive
		{status, {Name, N, Hist, Intf, Table, Map}} ->
			io:format("Status -------------~n"),
			io:format(" name: ~w~n", [Name]),
			io:format("    n: ~w~n", [N]),
			io:format(" msgs: ~w~n", [Hist]),
			io:format(" intf: ~w~n", [Intf]),
			io:format("table: ~w~n", [Table]),
			io:format("  map: ~w~n", [Map]),
			ok;
		true ->
			io:format("  got something: ~n")
	
	end.
    
    
	
			





