-module(cups_ui).
-export([init/1, philosopher_init/0, killPhilosophers/1]).

-on_load(startup/0).


startup() ->
	compile:file(ph_com),
	init(3),
	ok.


mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + (X rem Y);
mod(0,_) -> 0.


sendTick() ->
	TickTime = 2000,
	MinTime = 1000,
	erlang:send_after(round(random:uniform() * TickTime + MinTime), self(), tick),
	sentTick.
sendTock() ->
	TockTime = 5000,
	MinTime = 5000,
	%erlang:send_after(round(random:uniform() * TockTime + MinTime), self(), tock),
	sentTock.
sendBong() ->
	BongTime = 5000,
	MinTime = 2000,
	erlang:send_after(round(random:uniform() * BongTime + MinTime), self(), bong),
	sentBong.

init(Philo_count) ->
	MyFullIdentifier = node(),%list_to_atom(atom_to_list(jba_philo@) ++ atom_to_list(MyIP)),
	AllNodes = [MyFullIdentifier] ++ nodes(),
	AllPids = startPhilosophers(Philo_count,AllNodes,[]),
	sendPids(length(AllPids),AllPids),
	[FirstDude|_] = AllPids,
	FirstDude ! force_cup,
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
	startPhilosophers(N - 1,Nodes,[spawn(Ip1,fun() ->  cups_ui:philosopher_init() end)|Pids]).


sendPids(0,_) ->
	done;
sendPids(N,Pids) ->
	N_minus_1 = N - 1,
	Length = length(Pids),
	Right = mod(N_minus_1 + 1  , Length) + 1,
	Left = mod((N_minus_1 - 1), Length) + 1,
	lists:nth(N,Pids) ! {set_neighbors, lists:nth(Left,Pids),lists:nth(Right,Pids)},
	sendPids(N - 1,Pids).


signal(Pid, Message, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State) ->
	%MyPid = self(),
	%spawn(fun() -> ph_com:send_philosopher(MyPid,LeftPid,RightPid,HasLeft,HasRight,HasCup) end),
	Pid ! Message,
	receive
		tick ->
			sendTick(),
			step(failure, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		tock ->
			philosopher_sleep(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		bong ->
			sendBong(),
			signal(Pid, Message, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		success_f ->
			step(success, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		success_c ->
			step(success, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		failure ->
			step(failure, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		left_check ->
			if
				HasLeft == no ->
					Msg = success;
				true ->
					Msg = failure
			end,
			erlang:display({self(),checked_left,Msg}),
			RightPid ! Msg,
			signal(Pid, Message, LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		kill ->
			erlang:display({self(),killed})
	end.


philosopher_init() ->
	receive
		{set_neighbors, LeftPid, RightPid} ->
			Seed = crypto:bytes_to_integer(crypto:strong_rand_bytes(12)),
			random:seed(Seed,Seed,Seed),
			sendTick(),
			sendTock(),
			sendBong(),
			erlang:display({init_ed,self(),LeftPid,RightPid}),
			philosopher_loop(LeftPid, RightPid, no, no, no, no, thinking)
	end.


philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State) ->
	MyPid = self(),
	spawn(fun() -> ph_com:send_philosopher(MyPid,LeftPid,RightPid,HasLeft,HasRight,HasCup) end) ! stuff,
	receive
		force_cup ->
			erlang:display({self(),forced_cup}),
			philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, yes, IsThirsty, State);
		tick ->
			sendTick(),
			erlang:display({self(),timed_at_state, IsThirsty, State}),
			advance(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		tock ->
			philosopher_sleep(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		bong ->
			sendBong(),
			erlang:display({self(),bong}),
			case IsThirsty of
					yes -> erlang:display({self(),still_thirsty});
					no -> erlang:display({self(),became_thirsty})
			end,
			philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, yes, State);
		left_check ->
			if
				HasLeft == no ->
					Msg = success_f;
				true ->
					Msg = failure
			end,
			erlang:display({self(),checked_left,Msg}),
			RightPid ! Msg,
			philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		right_check ->
			if
				HasRight == no ->
					Msg = success_f;
				true ->
					Msg = failure
			end,
			erlang:display({self(),checked_right,Msg}),
			LeftPid ! Msg,
			philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State);
		cup ->
			if
				(HasCup == yes) and (IsThirsty == no) ->
					erlang:display({self(), gave_away_cup}),
					RightPid ! success_c,
					philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, no, no, State);
				true ->
					RightPid ! failure,
					philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State)
			end;
		kill ->
			erlang:display({self(),killed})
	end.


philosopher_sleep(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State) ->
	MyPid = self(),
	spawn(fun() -> ph_com:send_philosopher(MyPid,LeftPid,RightPid,HasLeft,HasRight,HasCup) end),
	erlang:display({self(),fell_asleep}),
	sendTock(),
	receive
		tock ->
			sendTick(),
			sendTock(),
			erlang:display({self(),woke_up}),
			philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, HasCup, IsThirsty, State)
	end.


advance(LeftPid, RightPid, HasLeft, HasRight, no, yes, State) ->
	% erlang:display({self(),became_thirsty}),
	erlang:display({self(),asking_for_cup}),
	signal(LeftPid,cup,LeftPid,RightPid,HasLeft,HasRight,no,yes,State);
advance(LeftPid, RightPid, HasLeft, HasRight, yes, yes, State) ->
	erlang:display({self(),done_drinking}),
	philosopher_loop(LeftPid,RightPid,HasLeft,HasRight,yes,no,State);
advance(LeftPid, RightPid, _, _, HasCup, no, thinking) ->
	erlang:display({self(),became_hungry}),
	philosopher_loop(LeftPid, RightPid, no, no, HasCup, no, hungry);
advance(LeftPid, RightPid, HasLeft, HasRight, HasCup, no, hungry) ->
	signal(LeftPid, left_check, LeftPid, RightPid, HasLeft, HasRight, HasCup, no, hungry);
advance(LeftPid, RightPid, HasLeft, HasRight, HasCup, no, hungry_left) ->
	signal(RightPid, right_check, LeftPid, RightPid, HasLeft, HasRight, HasCup, no, hungry_left);
advance(LeftPid, RightPid, _, _, HasCup, no, eating) ->
	erlang:display({self(),finished_eating}),
	philosopher_loop(LeftPid, RightPid, no, no, HasCup, no, thinking).


step(failure, LeftPid, RightPid, HasLeft, HasRight, _, yes, State) ->
	erlang:display({self(),failed_to_get_cup}),
	philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, no, yes, State);
step(success, LeftPid, RightPid, HasLeft, HasRight, _, yes, State) ->
	erlang:display({self(),got_cup}),
	philosopher_loop(LeftPid, RightPid, HasLeft, HasRight, yes, yes, State);
step(failure, LeftPid, RightPid, _, _, HasCup, no, hungry) ->
	erlang:display({self(),failed_to_get_left_fork}),
	philosopher_loop(LeftPid, RightPid, no, no, HasCup, no, hungry);
step(success, LeftPid, RightPid, _, _, HasCup, no, hungry) ->
	erlang:display({self(),got_left_fork}),
	philosopher_loop(LeftPid, RightPid, yes, no, HasCup, no, hungry_left);
step(failure, LeftPid, RightPid, _, _, HasCup, no, hungry_left) ->
	erlang:display({self(),failed_to_get_right_fork}),
	philosopher_loop(LeftPid, RightPid, no, no, HasCup, no, hungry);
step(success, LeftPid, RightPid, _, _, HasCup, no, hungry_left) ->
	erlang:display({self(),got_right_fork}),
	philosopher_loop(LeftPid, RightPid, yes, yes, HasCup, no, eating).