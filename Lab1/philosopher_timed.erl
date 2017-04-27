-module(philosopher_timed).
-export([init/1, philosopher_init/0, philosopher_loop/5, killPhilosophers/1]).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + (X rem Y);
mod(0,_) -> 0.

init(Philo_count) ->
	%{_, [{IP_tuple, _, _} | _]} = inet:getif(),
	%MyIP = list_to_atom(inet_parse:ntoa(IP_tuple)),
	MyFullIdentifier = node(),%list_to_atom(atom_to_list(jba_philo@) ++ atom_to_list(MyIP)),
	AllNodes = [MyFullIdentifier] ++ nodes(),
	AllPids = startPhilosophers(Philo_count,AllNodes,[]),
	sendPids(length(AllPids),AllPids),
	AllPids.

%philosopher_timed:killPhilosophers().
killPhilosophers(Pids) ->
	lists:foreach(fun(Pid) -> Pid ! kill end, Pids).

startPhilosophers(0,_,Pids) ->
	Pids;
startPhilosophers(N,Nodes,Pids) ->
	Length = length(Nodes),	
	Ip1Id = (N rem Length) + 1,
	Ip1 = lists:nth(Ip1Id,Nodes),
	startPhilosophers(N - 1,Nodes,[spawn(Ip1,fun() ->  philosopher_timed:philosopher_init() end)|Pids]).

sendPids(0,_) ->
	done;
sendPids(N,Pids) ->
	N_minus_1 = N - 1,
	Length = length(Pids),
	Right = mod(N_minus_1 + 1  , Length) + 1,
	Left = mod((N_minus_1 - 1), Length) + 1,
	lists:nth(N,Pids) ! {set_neighbors, lists:nth(Left,Pids),lists:nth(Right,Pids)},
	%erlang:send_after(), lists:nth(N,Pids), time),
	sendPids(N - 1,Pids).

philosopher_init() ->
	receive
		{set_neighbors, LeftPid, RightPid} -> 
		%	erlang:display("HELLO"),
			erlang:send_after(round(random:uniform() * 5000), self(), time),
			erlang:display({init_ed,self(),LeftPid,RightPid}),
			philosopher_loop(LeftPid, RightPid, no, no, thinking)
	end.

%philosophy(Pid) ->
%	Pid ! time,
%	apply_after(round(random:uniform() * 5000, philosophy, Function, Arguments)

philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State) ->
	receive
		time -> 
			erlang:send_after(round(random:uniform() * 5000), self(), time),
			erlang:display({self(),timed_at_state,State}),
			advance(LeftPid, RightPid, HasLeft, HasRight, State);
		left_check -> 
			erlang:display({self(),checked_left,HasLeft}),
			LeftPid ! {left_checked,HasLeft},
			philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State);
		right_check -> 
			erlang:display({self(),checked_right,HasRight}),
			RightPid ! {right_checked,HasRight},
			philosopher_loop(LeftPid, RightPid, HasRight, HasLeft, State);
		kill ->
			erlang:display({self(),killed})
	end.

advance(LeftPid, RightPid, _, _, thinking) ->
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
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry);
		time -> 
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
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry_left);
		time -> 
			advance(LeftPid, RightPid, HasRight, HasLeft, hungry_left)
	end;
advance(LeftPid, RightPid, _, _, eating) ->
	erlang:display({self(),finished_eating}),
	philosopher_loop(LeftPid, RightPid, no, no, thinking).
