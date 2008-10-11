%%%--------------------------------------------------------------------- 
%%% module pwd
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
	CharPartitions = partition_alphabet(Len, Processes),
	Server = self(),	
	StartTime = now(),
	lists:foreach(fun({Min,Max}) -> 
		spawn(fun() -> 
			analyze(Server, Crypted, Min, Max) end) 
		end, CharPartitions),
	loop(Processes, StartTime, notfound).

%%----------------------------------------------------------------------
%% Function: loop/3
%% Purpose: Server loop that listens for results of clients processes which perform password analysis.
%% Captures time required for successful decryption but doesn't return until all processes have reported
%% either success or failure.
%% Args:  Number of process, start time, return message 
%% Returns: Return value of either notfound or {found,password,elapsed_time}
%%----------------------------------------------------------------------
loop(0, _Start, Ret) ->
	Ret;
loop(Processes, Start, Ret) ->
 	receive		
		{found, Password} ->			
			Elapsed = timer:now_diff(now(), Start) / 1000 / 1000,  %seconds
			loop(Processes-1, Start, {found,Password,Elapsed});
		notfound ->
			io:format("Processes remaining: ~p~n",[Processes]),
			loop(Processes-1, Start, Ret)			
	 end.

%%----------------------------------------------------------------------
%% Function: analyze/5
%% Purpose: analyze each array of characters between Min and Max and compare its md5 hash against Crypted.
%% Notify server on success or failure
%% Args:   Server Pid, encrypted hash, starting character array, ending character array, length of plaintext
%% Returns: notifies server of success or failure
%%----------------------------------------------------------------------
analyze(Server, Crypted, Max, Max) ->
   case erlang:md5(Max) of
       Crypted ->
           Server ! {found, Max};
       _ ->
           Server ! notfound
   end;
analyze(Server, Crypted, Cur, Max) ->
   case erlang:md5(Cur) of
       Crypted ->
           Server ! {found, Cur};
       _ ->
           analyze(Server, Crypted, next(Cur), Max)
   end.
	
%%----------------------------------------------------------------------
%% Function: partition_alphabet/2
%% Purpose: partition lowercase alphabet according to number of process to spawn.
%% The max number of strings to analyze will be 26 ^ Len.
%% When calculating the first item of the bounds, all possible combinations
%% less than that length will be added, for instance for length 4, 
%% First = (26^3) + (26^2) + 26 = 18278. 
%% Args:   Length of plaintext, Number of processes
%% Returns: array of 2-tuples of form {min,max} where each are char arrays, e.g., {"aa","mm"}
%%----------------------------------------------------------------------
partition_alphabet(Len, Processes) ->
	TotalStrings = round(math:pow(26, Len)),
	First = first_int(Len),	
	Last = First + TotalStrings,
	StringsPerProc = round(TotalStrings / Processes), 
	partition_alphabet(Processes, First - 1, Last, StringsPerProc, []).

partition_alphabet(1, Cur, Last, _StringsPerProc, L) ->
	%final partition always receives all remaining arrays because
	%TotalStrings / Processes in partition_alphabet/2 above may have had a remainder
	Min = Cur + 1,
	MinMax = {chr_array(Min), chr_array(Last - 1)},
	[MinMax|L];	
partition_alphabet(ProcsLeft, Cur, Last, StringsPerProc, L) ->
	Min = Cur + 1,
	Max = Min + StringsPerProc - 1,
	MinMax = {chr_array(Min), chr_array(Max)},
	partition_alphabet(ProcsLeft - 1, Max, Last, StringsPerProc, [MinMax|L]).		

%%----------------------------------------------------------------------
%% Function: first_int/1
%% Purpose: given a string of length Len, return the base-10 number that represents the first string
%% having that length. For instance, Len 1 returns 0, Len 2 returns 26, etc...
%% Args:  string length
%% Returns:  integer
first_int(Len) when Len > 0 ->
	first_int(Len-1, 0).

first_int(0, Acc) ->
	Acc;	
first_int(Len, Acc) ->
	first_int(Len - 1, Acc + round(math:pow(26, Len))).

%%----------------------------------------------------------------------
%% Function: chr_array/1
%% Purpose: given a base-10 number, use base-26 math to return the corresponding character array
%% Based on Java example of hexavigesimal math from http://en.wikipedia.org/wiki/Hexavigesimal
%% $a represents ASCII 97.  Thus 0 returns [97] or "a".
%% Args:  base-10 number
%% Returns:  ASCII character array representing conversion from base-10 to base-26
chr_array(I) ->
	chr_array(I, []).
	
chr_array(I, L) when I > 25 ->
	R = I rem 26,
	D = I div 26,
	chr_array(D - 1, [R+$a|L]);	
chr_array(I, L) ->
	[I + $a|L].
	
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

