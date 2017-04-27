-module(philosopher).
-export([init/1, philosopher_init/0, philosopher_loop/5]).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + (X rem Y);
mod(0,Y) -> 0.

init(Philo_count) ->
	{_, [{IP_tuple, _, _} | _]} = inet:getif(),
	MyIP = list_to_atom(inet_parse:ntoa(IP_tuple)),
	MyFullIdentifier = node(),%list_to_atom(atom_to_list(jba_philo@) ++ atom_to_list(MyIP)),
	AllNodes = [MyFullIdentifier] ++ nodes(),
	AllPids = startPhilosophers(Philo_count,AllNodes,[]),
	sendPids(length(AllPids),AllPids),
	AllPids.

startPhilosophers(0,_,Pids) ->
	Pids;
startPhilosophers(N,Nodes,Pids) ->
	Length = length(Nodes),	
	Ip1Id = (N rem Length) + 1,
	Ip1 = lists:nth(Ip1Id,Nodes),
	startPhilosophers(N - 1,Nodes,[spawn(Ip1,fun() ->  philosopher:philosopher_init() end)|Pids]).

sendPids(0,Pids) ->
	done;
sendPids(N,Pids) ->
	N_minus_1 = N - 1,
	Length = length(Pids),
	Right = mod(N_minus_1 + 1  , Length) + 1,
	Left = mod((N_minus_1 - 1), Length) + 1,
	lists:nth(N,Pids) ! {set_neighbors, lists:nth(Left,Pids),lists:nth(Right,Pids)},
	sendPids(N - 1,Pids).

philosopher_init() ->
	receive
		{set_neighbors, LeftPid, RightPid} -> 
		%	erlang:display("HELLO"),
			erlang:display({init_ed,self(),LeftPid,RightPid}),
			philosopher_loop(LeftPid, RightPid, no, no, thinking)
	end.

philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State) ->
	receive
		time -> 
			erlang:display({self(),timed_at_state,State}),
			advance(LeftPid, RightPid, HasLeft, HasRight, State);
		left_check -> 
			erlang:display({self(),checked_my_left_fork,HasLeft}),
			LeftPid ! {left_checked,HasLeft},
			philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State);
		right_check -> 
			erlang:display({self(),checked_my_right_fork,HasRight}),
			RightPid ! {right_checked,HasRight},
			philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State)
	end.

advance(LeftPid, RightPid, HasLeft, HasRight, thinking) ->
	erlang:display({self(),became_hungry}),
	philosopher_loop(LeftPid, RightPid, no, no, hungry);
advance(LeftPid, RightPid, HasLeft, HasRight, hungry) ->
	LeftPid ! right_check,
	receive
		{right_checked,yes} -> 
			erlang:display({self(),failed_to_get_left_fork}),
			philosopher_loop(LeftPid,RightPid,no,no,hungry);
		{right_checked,no} -> 
			erlang:display({self(),got_left_fork}),
			philosopher_loop(LeftPid,RightPid,yes,no,hungry_left);
		left_check -> 
			erlang:display({self(),checked_left,HasLeft}),
			LeftPid ! {left_checked,HasLeft},
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry);
		right_check -> 
			erlang:display({self(),checked_right,HasRight}),
			RightPid ! {right_checked,HasRight},
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry)
	end;
advance(LeftPid, RightPid, HasLeft, HasRight, hungry_left) ->
	RightPid ! left_check,
	receive
		{left_checked,yes} -> 
			erlang:display({self(),failed_to_get_right_fork}),
			philosopher_loop(LeftPid,RightPid,no,no,hungry);
		{left_checked,no} -> 
			erlang:display({self(),got_right_fork}),
			philosopher_loop(LeftPid,RightPid,yes,yes,eating);
		left_check -> 
			erlang:display({self(),checked_left,HasLeft}),
			LeftPid ! {left_checked,HasLeft},
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry_left);
		right_check -> 
			erlang:display({self(),checked_right,HasRight}),
			RightPid ! {right_checked,HasRight},
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry_left)
	end;
advance(LeftPid, RightPid, HasLeft, HasRight, eating) ->
	erlang:display({self(),finished_eating}),
	philosopher_loop(LeftPid, RightPid, no, no, thinking).