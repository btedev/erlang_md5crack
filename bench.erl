%%%--------------------------------------------------------------------- 
%%% module bench
%%%--------------------------------------------------------------------- 
%%% Benchmarks pwd:decrypt function for random strings using varying 
%%% numbers of processes.  Expects input file for strings to be tested.
%%% File is output of ruby/bench.rb.
%%%--------------------------------------------------------------------- 

-module(bench).
-compile(export_all).

start(Processes) ->
	file:delete("results.txt"),
	{ok, FileIn} = file:open("ruby/results.txt", read),
	{ok, FileOut} = file:open("results.txt", write),
	test(FileIn, FileOut, Processes).
	
test(FileIn, FileOut, Processes) ->
	case io:get_line(FileIn,'') of
		eof ->
			file:close(FileIn),
			file:close(FileOut);
		Line ->
			Parts = string:tokens(Line,","),
			[TestString|Rest] = Parts,
			TestHash = pwd:encrypt(TestString),
			{found,Pwd,Time} = pwd:decrypt(TestHash, length(TestString), Processes),
			io:format(FileOut, "~p,~p,~p,~p~n",[Pwd,length(TestString),Processes,Time]),
			test(FileIn, FileOut, Processes)
	end.
