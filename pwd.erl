-module(pwd).
-compile(export_all).

encrypt(Plain) ->
	erlang:md5(Plain).
	
decrypt(Crypted, Len, Processes) ->
	CharArrays = partition_alphabet(Processes, Len),
	ServerPid = spawn(fun() -> loop() end),		
	lists:foreach(fun([Min|Max]) -> spawn(fun() -> analyze(ServerPid, Crypted, Min, Max, Len) end) end, CharArrays).
	
loop() ->
	io:format("server loop running~n"),
 	receive
		{found, Password} ->
			io:format("password decrypted: ~p~n",[Password]);
		_X ->
			io:format("received message ~p~n",[_X]),
			loop()			
	 end.

%partition lowercase alphabet according to number of process to spawn
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
	Arr = [chr_array(CurChar, Len, min) , chr_array($z, Len, max)],
	partition_alphabet([Arr|L], 0, Len, Incr, 0);
partition_alphabet(L, Partitions, Len, Incr, CurChar) ->
	Arr = [chr_array(CurChar, Len, min) , chr_array(CurChar+Incr, Len, max)],
	partition_alphabet([Arr|L], Partitions-1, Len, Incr, CurChar+Incr+1).

%analyze each array of chars between Min and Max and compare its md5 hash against Crypted.
%notify server on success or failure
analyze(Server, Crypted, Max, Max, Len) ->	
	Test = erlang:md5(Max),
	if 
		Test =:= Crypted -> 
			Server ! {found, Max};		
		true -> 
			Server ! {notfound}
	end;
analyze(Server, Crypted, Cur, Max, Len) ->		
	Test = erlang:md5(Cur),
	if 
		Test =:= Crypted -> 
			Server ! {found, Cur};		
		true -> 
			analyze(Server, Crypted, next(Cur), Max, Len)
	end.

%creates a minimum or maximum boundary char array 
%minimum starts with character C followed by N-1 "a" characters. For C=100, N=3, returns "daa"
%maximum starts with character C followed by N-1 "z" characters. For C=121, N=3, returns "yzz"
chr_array(C, N, MinMax) ->
	chr_array([], C, N, MinMax).
chr_array(L, C, 1, MinMax) ->
	[C|L];			%add character C as final item since it is appended to head of list
chr_array(L, C, N, min) ->
	chr_array([$a|L], C, N-1, min);
chr_array(L, C, N, max) ->
	chr_array([$z|L], C, N-1, max).
	
%increment char array L by one character. e.g., "aaa" becomes "aab" and "bzz" becomes "caa"
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
