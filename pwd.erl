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
	S = self(),	
	lists:foreach(fun({Min,Max}) -> spawn(fun() -> analyze(S, Crypted, Min, Max, Len) end) end, CharPartitions),
	loop().
	
loop() ->
 	receive
		{found, Password} ->
			io:format("password decrypted: ~p~n",[Password]),
			Password;
		_X ->
			io:format("received message ~p~n",[_X]),
			loop()			
	 end.

%%----------------------------------------------------------------------
%% Function: analyze/5
%% Purpose: analyze each array of characters between Min and Max and compare its md5 hash against Crypted.
%% Notify server on success or failure
%% Args:   Server Pid, encrypted hash, starting character array, ending character array, length of plaintext
%% Returns: notifies server of success or failure
%%----------------------------------------------------------------------
analyze(Server, Crypted, Max, Max, Len) ->
   case erlang:md5(Max) of
       Crypted ->
           Server ! {found, Max};
       _ ->
           Server ! notfound
   end;
analyze(Server, Crypted, Cur, Max, Len) ->
   case erlang:md5(Cur) of
       Crypted ->
           Server ! {found, Cur};
       _ ->
			io:format("analyzing ~p~n",[Cur]),
           analyze(Server, Crypted, next(Cur), Max, Len)
   end.
	
%%----------------------------------------------------------------------
%% Function: partition_alphabet/2
%% Purpose: partition lowercase alphabet according to number of process to spawn
%% Args:   Length of plaintext, Number of processes
%% Returns: array of 2-tuples of form {min,max} where each of min and max are char arrays
%%----------------------------------------------------------------------
partition_alphabet(Len, Processes) ->
	TotalStrings = round(math:pow(26, Len)),
	StringsPerProc = round(TotalStrings / Processes), %average
	partition_alphabet(Processes, 0, StringsPerProc, TotalStrings, []).

partition_alphabet(1, Last, StringsPerProcess, TotalStrings, L) ->
	%final partition always receives Last to TotalStrings because
	%TotalStrings / Processes in partition_alphabet/2 above may have had a remainder
	Min = Last + 1,
	MinMax = {chr_array(Min), chr_array(TotalStrings)},
	[MinMax|L];	
partition_alphabet(ProcsLeft, Last, StringsPerProcess, TotalStrings, L) ->
	Min = Last + 1,
	Max = Min + StringsPerProcess - 1,
	MinMax = {chr_array(Min), chr_array(Max)},
	partition_alphabet(ProcsLeft - 1, Max, StringsPerProcess, TotalStrings, [MinMax|L]).	

%%----------------------------------------------------------------------
%% Function: chr_array/1
%% Purpose: given a base-10 number, use base-26 math to return the corresponding character array
%% Port of Java example of hexavigesimal math from http://en.wikipedia.org/wiki/Hexavigesimal
%% Counting starts at 1, not 0.  96 is magic number added to get ASCII value.  Thus 1 returns [97] or "a"
%% Args:  base-10 number
%% Returns:  ASCII character array representing conversion from base-10 to base-26
chr_array(I) ->
	chr_array(I,[]).
	
chr_array(I,L) when I > 26 ->
	R = I rem 26,
	chr_array(I div 26, [R+96|L]);
chr_array(I,L) ->
	[I+96|L].
	
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

