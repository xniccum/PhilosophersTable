-module(ph_com).
-export([send_philosopher/1,send_philosopher/6,testPhilosopher/0]).

bool(yes) ->
	true;
bool(true) ->
	true;
bool(_) ->
	false.

format_philosopher(Pid,LeftNeighbor,RightNeighbor,HasLeft,HasRight,HasCup) ->
	Pst = "\"pid\":\"" ++ erlang:pid_to_list(Pid) ++ "\",",
	Lnst = "\"leftNeighbor\":\"" ++ erlang:pid_to_list(LeftNeighbor) ++ "\",",
	Rnst = "\"rightNeighbor\":\"" ++ erlang:pid_to_list(RightNeighbor) ++ "\",",
	Hlst = "\"hasLeftFork\":" ++ erlang:atom_to_list(bool(HasLeft)) ++ ",",
	Hrst = "\"hasRightFork\":" ++ erlang:atom_to_list(bool(HasRight)) ++ ",",
	Hcst = "\"hasCup\":" ++ erlang:atom_to_list(bool(HasCup)) ++ "",
	"{" ++ Pst ++ Lnst ++ Rnst ++ Hlst ++ Hrst ++ Hcst ++ "}".

testPhilosopher() ->
	format_philosopher(erlang:list_to_pid("<0.12.0>"),
		erlang:list_to_pid("<0.13.0>"),
		erlang:list_to_pid("<0.14.0>"),
		yes,no,yes).

send_philosopher(Philo) ->
	inets:start(),
	Url = "http://127.0.0.1:3000/api/philosophers",
	Headers = [],
	ContentType = "application/json",
	Body = Philo,
	Request = {Url,Headers,ContentType,Body},
	httpc:request(post, Request, [], [{sync, false}]).

send_philosopher(Pid,LeftNeighbor,RightNeighbor,HasLeft,HasRight,HasCup) ->
	send_philosopher(format_philosopher(Pid,LeftNeighbor,RightNeighbor,HasLeft,HasRight,HasCup)).