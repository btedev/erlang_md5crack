-module(dist).
-compile(export_all).

rpc(Q) ->
	dist ! {self(), Q},
	receive
		{dist, Reply} ->
			Reply
	end.

start() -> register(dist, spawn(fun() -> server_loop(dict:new()) end)).

stop() ->
	rpc({stop_all}).

% note: rpc:call below is a function on the rpc lib and has nothing to do with the homegrown rpc() above
% rpc:call/2 allows a remote shell using the same password as the local shell, to make rpc calls on local functions
register_me(Server, Nick) ->
	rpc:call(Server, dist, enslave, [Nick]).

enslave(Nick) ->
	Reply = rpc({enslave, Nick}),
		case Reply of
			ok ->
				io:format("OK, I'm running as ~p on pid ~p...~n",[Nick, self()]),
				client_loop();
			Other ->
				io:format("problem registering bot: ~p~n",[Other])
		end.

decrypt(Word) ->
	Len = string:len(Word),
	Crypted = pwd:encrypt(Word),
	rpc({decrypt, Crypted, Len}).

parrot(Text) ->
	rpc({parrot, Text}).

server_loop(Bots) ->
	receive
		{From, {enslave, Nick}} ->
			Bots2 = Bots:append(From, Nick),
			From ! {dist, ok},
			server_loop(Bots2);
		{From, {parrot, Text}} ->
			dict:map(fun(Pid,Nick) -> Pid ! {parrot, Text, Nick} end, Bots),
			From ! {dist, ok},
			server_loop(Bots);
		{From, {decrypt, Crypted, Len}} ->
			Pairs = pwd:partition_alphabet(Len, dict:size(Bots)),
			distribute_work(dict:to_list(Bots), Pairs, Len),
			From ! {dist, ok},
			server_loop(Bots);
		{From, {stop_all}} ->
			dict:map(fun(Pid,Nick) -> Pid ! {bye} end, Bots),
			From ! {dist, ok};
		{From, Other} ->
			io:format("cannot handle: ~p~n",[Other]),
			server_loop(Bots)
	end.

distribute_work([],[],_) -> ok;
distribute_work(BotList, Pairs, Len) ->
	[CurPair|RestPairs] = Pairs,
	[{Pid,Nick}|RestBots] = BotList,
	Pid ! {decrypt, CurPair, Len},
	distribute_work(RestBots,RestPairs,Len).

client_loop() ->
	receive
		{decrypt, {First, Last}, Len} ->
			io:format("Working on decrypting string of length ~p...~n",[Len]),

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
