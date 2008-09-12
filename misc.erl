%%%--------------------------------------------------------------------- 
%%% module misc
%%%--------------------------------------------------------------------- 
%%% Small examples of Erlang code used for Erlang 101 portion of talk  
%%%--------------------------------------------------------------------- 

-module(misc).
-compile(export_all).

%% Countdown from N to 0 and print each number to the console
countdown(0) ->
	io:format("0~n");
countdown(N) ->
	io:format("~p~n",[N]),
	countdown(N-1).
	