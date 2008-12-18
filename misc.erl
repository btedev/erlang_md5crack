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
	
loop() ->
	receive
		{double,N} ->
			io:format("Double ~p if ~p~n",[N,N*2]),
			loop();
		{triple,N} ->
			io:format("Triple ~p if ~p~n",[N,N*3]),
			loop();
		close ->
			io:format("closing...~n",[]);
		Other ->
			io:format("Don't know how to ~p~n",[Other]),
			loop()
	end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
