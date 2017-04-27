-module(tic_tac_toe).
-export([test/0]).

-on_load(startup/0).

-record(gamestate,{
	board=[[empty,empty,empty],[empty,empty,empty],[empty,empty,empty]],
	turn=left,
	turn_number=0,
	lpid,
	rpid
	}).

startup() ->
	compile:file(ph_com),
	ok.

test() ->
	#gamestate{}.

left_victory() ->
	