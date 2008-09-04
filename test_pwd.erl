-module(test_pwd).
-include_lib("eunit/include/eunit.hrl").

encrypt_test() ->
	?assertEqual(<<73,246,138,92,132,147,236,44,11,244,137,130,28,33,252,59>>, pwd:encrypt("hi")).

%need to refactor to allow decryption tests without messaging to server
%decrypt_test() ->
%	?assertEqual("hi", pwd:decrypt(<<73,246,138,92,132,147,236,44,11,244,137,130,28,33,252,59>>, 2, 97, 122)).
	
next_test() ->
	?assertEqual("b", pwd:next("a")),
	?assertEqual("ba", pwd:next("az")),
	?assertEqual("zzz", pwd:next("zzy")),
	?assertEqual("aba", pwd:next("aaz")),
	?assertEqual("zba", pwd:next("zaz")),
	?assertEqual("baa", pwd:next("azz")).
	
partition_test() ->
	?assertEqual([{"aaaa","zzzz"}], pwd:partition_alphabet(1,4)),
	?assertEqual([{"saaa","zzzz"},{"jaaa","rzzz"},{"aaaa","izzz"}], pwd:partition_alphabet(3,4)).
	
char_array_test() ->
	?assertEqual("caa",pwd:chr_array($c,3,min)),
	?assertEqual("czz",pwd:chr_array($c,3,max)).