%%%--------------------------------------------------------------------- 
%%% module dist 
%%%--------------------------------------------------------------------- 
%%% Adds distributed functions to pwd.erl 
%%%--------------------------------------------------------------------- 

-module(dist).
-compile(export_all).

%%----------------------------------------------------------------------
%% Function: rpc/1
%% Purpose: Standardizes calls to dist server_loop
%% Args: message to pass
%% Returns: reply from server_loop
%%----------------------------------------------------------------------
rpc(Q) ->
	dist ! {self(), Q},
	receive
		{dist, Reply} ->
			Reply
	end.

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: register server_loop as "dist"
%%----------------------------------------------------------------------
start() -> register(dist, spawn(fun() -> server_loop(dict:new(),none) end)).

%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose: stops all client processes and unregisters dist
%%----------------------------------------------------------------------
stop() -> rpc({stop_all}).

% note: rpc:call below is a function on the rpc lib and has nothing to do with the homegrown rpc() above
% rpc:call/2 allows a remote shell using the same password as the local shell to make rpc calls on local functions
register_me(Server, Nick) ->
	rpc:call(Server, dist, enlist, [Nick]).

%%----------------------------------------------------------------------
%% Function: enlist/1 
%% Purpose: enlists client for password-cracking
%% Args: nickname of client
%%----------------------------------------------------------------------
enlist(Nick) ->
	Reply = rpc({enlist, Nick}),
		case Reply of
			ok ->
				io:format("OK, I'm running as ~p on pid ~p...~n",[Nick, self()]),
				client_loop();
			Other ->
				io:format("problem registering bot: ~p~n",[Other])
		end.

%%----------------------------------------------------------------------
%% Function: decrypt/1
%% Purpose: called by server to begin decyption 
%% Args: plaintext word that will be encrypted, then sent to clients for decryption
%%----------------------------------------------------------------------
decrypt(Word) ->
	Len = string:len(Word),
	Crypted = pwd:encrypt(Word),
	rpc({decrypt, Crypted, Len}).

%%----------------------------------------------------------------------
%% Function: parrot/1
%% Purpose: make clients parrot any random thing
%% Args: text for all clients to parrot
%%----------------------------------------------------------------------
parrot(Text) ->
	rpc({parrot, Text}).

%%----------------------------------------------------------------------
%% Function: server_loop/1
%% Purpose: main server loop
%% Args: dictionary of bots (clients)
%%----------------------------------------------------------------------
server_loop(Bots,StartTime) ->
	receive
		{From, {enlist, Nick}} ->
			io:format("Welcome ~p~n", [Nick]),
			Bots2 = Bots:append(From, Nick),
			From ! {dist, ok},
			server_loop(Bots2,StartTime);
		{From, {parrot, Text}} ->
			dict:map(fun(Pid,Nick) -> Pid ! {parrot, Text, Nick} end, Bots),
			From ! {dist, ok},
			server_loop(Bots,StartTime);
		{From, {decrypt, Crypted, Len}} ->
			Pairs = pwd:partition_alphabet(Len, dict:size(Bots)),
			distribute_work(dict:to_list(Bots), Pairs, Crypted, Len),
			From ! {dist, ok},
			server_loop(Bots,now());
		{From, {found, Plain}} ->
			Elapsed = timer:now_diff(now(), StartTime) / 1000 / 1000,  %seconds
			io:format("~p decrypted password ~p in ~p seconds~n",[botnick(Bots,From),Plain,Elapsed]),
			From ! {dist, you_rock},
			server_loop(Bots,StartTime);
		{From, {notfound}} ->
			io:format("~p failed to find the password: ~n",[botnick(Bots,From)]),
			From ! {dist, thanks_for_playing},
			server_loop(Bots,StartTime);
		{From, {print_botlist}} ->
			io:format("Current bots:~n",[]),
			print_bots(dict:to_list(Bots)),
			From ! {dist, ok},
			server_loop(Bots,StartTime);
		{From, {stop_all}} ->
			dict:map(fun(Pid,_Nick) -> Pid ! {bye} end, Bots),
			From ! {dist, ok};
		{From, Other} ->
			io:format("cannot handle: ~p~n",[Other]),
			From ! {dist, ok},
			server_loop(Bots,StartTime)
	end.

%%----------------------------------------------------------------------
%% Function: distribute_work/4
%% Purpose: distribute password encryption to all clients
%% Args: list of bots, list of tuples of form {Min,Max}, hashed password, length of plaintext word
%%----------------------------------------------------------------------
distribute_work([],[],_,_) -> ok;
distribute_work(BotList, Pairs, Crypted, Len) ->
	[CurPair|RestPairs] = Pairs,
	[{Pid,_Nick}|RestBots] = BotList,
	Pid ! {decrypt, CurPair, Crypted, Len},
	distribute_work(RestBots,RestPairs,Crypted,Len).

%%----------------------------------------------------------------------
%% Function: botnick/2
%% Purpose: return the nickname of a bot by its pid
%% Args: list of bots, pid whose nickname will be returned
%%----------------------------------------------------------------------
botnick(Bots,Pid) ->
	{ok, Nick} = dict:find(Pid,Bots),
	Nick.

%%----------------------------------------------------------------------
%% Function: print_bots/1
%% Purpose: prints the list of bots (clients) currently enlisted
%% Args: list of bots
%%----------------------------------------------------------------------
print_bots([]) -> ok;
print_bots(Bots) ->
	[{_Pid,Nick}|Rest] = Bots,
	io:format("\t~p~n",[Nick]),
	print_bots(Rest).

%%----------------------------------------------------------------------
%% Function: client_loop/0
%% Purpose: main client functions
%%----------------------------------------------------------------------
client_loop() ->
	receive
		{decrypt, {Min, Max}, Crypted, Len} ->
			io:format("Trying to match password with strings between ~p and ~p of length ~p...~n",[Min,Max,Len]),
			analyze(Crypted, Min, Max),
			client_loop();
		{parrot, Text, Nick} ->
			io:format("My name is ~p and I go ~p~n", [Nick,Text]),
			client_loop();
		{bye} ->
			io:format("Going away...~n",[]);
		Other ->
			io:format("Client cannot handle ~p~n",[Other]),
			client_loop()
	end.

%%----------------------------------------------------------------------
%% Function: analyze/3
%% Purpose: modified pwd:analyze/3 used by clients to find password matches
%% Args: hashed password, current and max char arrays
%% Returns: sends results to server as message
%%----------------------------------------------------------------------
analyze(Crypted, Max, Max) ->
   case erlang:md5(Max) of
       Crypted ->
	   	   io:format("I did it!  The answer is ~p~n",[Max]),
           rpc({found, Max});
       _ ->
	   	   io:format("Someone else must have the answer...~n",[]),
           rpc({notfound})
   end;
analyze(Crypted, Cur, Max) ->
   case erlang:md5(Cur) of
       Crypted ->
	   	   io:format("I did it!  The answer is ~p~n",[Cur]),
           rpc({found, Cur});
       _ ->
           analyze(Crypted, pwd:next(Cur), Max)
   end.
