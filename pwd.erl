%%%--------------------------------------------------------------------- 
%%% Description module pwd
%%%--------------------------------------------------------------------- 
%%% Decrypts md5-hashed strings of known length.  This is demonstration
%%% code for an intro talk on Erlang concurrency.  
%%%--------------------------------------------------------------------- 

-module(pwd).
-compile(export_all).

%%----------------------------------------------------------------------
%% Function: encrypt/1
%% Purpose: Hash a plaintext string using erlang:md5 function
%% Args:   Plaintext string, must be lowercase with no numbers or special characters
%% Returns: Binary md5 hash
%%----------------------------------------------------------------------
encrypt(Plain) ->
	erlang:md5(Plain).
	
%%----------------------------------------------------------------------
%% Function: decrypt/3
%% Purpose: Given a md5 hash of known length, return the plaintext used to create the hash
%% (crack the password)
%% Args:   md5 hash, length of original string, number of processes to spawn
%% Returns: plaintext
%%----------------------------------------------------------------------
decrypt(Crypted, Len, Processes) ->
	CharArrays = partition_alphabet(Processes, Len),
	ServerPid = spawn(fun() -> loop() end),		
	lists:foreach(fun({Min,Max}) -> spawn(fun() -> analyze(ServerPid, Crypted, Min, Max, Len) end) end, CharArrays).
	
loop() ->
	io:format("server loop running~n"),
 	receive
		{found, Password} ->
			io:format("password decrypted: ~p~n",[Password]);
		_X ->
			io:format("received message ~p~n",[_X]),
			loop()			
	 end.

%%----------------------------------------------------------------------
%% Function: partition_alphabet/2
%% Purpose: partition lowercase alphabet according to number of process to spawn
%% Args:   Number of processes, length of plaintext
%% Returns: array of 2-tuples of form {Starting character array, Ending character array}
%%----------------------------------------------------------------------
partition_alphabet(Processes, Len) ->
	%get average chars per process (may not divide evenly, note using integer division)
	Incr = 26 div Processes,  
	partition_alphabet([], Processes, Len, Incr, $a).
	
partition_alphabet(L, 0, Len, Incr, CurChar) ->
	L;
partition_alphabet(L, 1, Len, Incr, CurChar) ->
	%final partiton is a special case.  In circumstances where alphabet / processes has a remainder,
	%the final partition will receive all remaining characters.  For instance, where processes = 4,
	%final partition will only receive 5 chars (v-z) instead of 7 like the others.
	Arr = {chr_array(CurChar, Len, min) , chr_array($z, Len, max)},
	partition_alphabet([Arr|L], 0, Len, Incr, 0);
partition_alphabet(L, Partitions, Len, Incr, CurChar) ->
	Arr = {chr_array(CurChar, Len, min) , chr_array(CurChar+Incr, Len, max)},
	partition_alphabet([Arr|L], Partitions-1, Len, Incr, CurChar+Incr+1).

%%----------------------------------------------------------------------
%% Function: analyze/5
%% Purpose: analyze each array of characters between Min and Max and compare its md5 hash against Crypted.
%% Notify server on success or failure
%% Args:   Server Pid, encrypted hash, starting character array, ending character array, length of plaintext
%% Returns: notifies server of success or failure
%%----------------------------------------------------------------------
analyze(Server, Crypted, Max, Max, Len) ->	
	Test = erlang:md5(Max),
	case Test =:= Crypted of 
		true ->
			Server ! {found, Max};		
		false -> 
			Server ! {notfound}
	end;
analyze(Server, Crypted, Cur, Max, Len) ->		
	Test = erlang:md5(Cur),
	case Test =:= Crypted of
		true ->
			Server ! {found, Cur};		
		false -> 
			analyze(Server, Crypted, next(Cur), Max, Len)
	end.

%%----------------------------------------------------------------------
%% Function: chr_array/3
%% Purpose: create an array of characters of length N that will represent either a minimum (starting)
%% or maxiumum (ending) character array for analysis.  
%% minimum starts with character C followed by N-1 "a" characters. For C=100, N=3, returns "daa"
%% maximum starts with character C followed by N-1 "z" characters. For C=121, N=3, returns "yzz"
%% Args:   first character in array, length of character array, min/max atom specifying type of array
%% Returns: character array of length N
%%----------------------------------------------------------------------
chr_array(C, N, MinMax) ->
	chr_array([], C, N, MinMax).
chr_array(L, C, 1, MinMax) ->
	[C|L];			%add character C as final item since it is appended to head of list
chr_array(L, C, N, min) ->
	chr_array([$a|L], C, N-1, min);
chr_array(L, C, N, max) ->
	chr_array([$z|L], C, N-1, max).
	
%%----------------------------------------------------------------------
%% Function: next/1
%% Purpose: increment char array L by one character. e.g., "aaa" becomes "aab" and "bzz" becomes "caa"
%% Args:   character array to increment
%% Returns: character array
%%----------------------------------------------------------------------
next(L) ->
	next(lists:reverse(L), [], true).

next([], L, _Incr) ->
	L;
next([$z|T], L, true) ->
	next(T, [$a|L], true);  %roll over "z" to "a" and pass along the increment
next([H|T], L, true) ->
	next(T, [H+1|L], false);
next([H|T], L, false) ->
	next(T, [H|L], false).
