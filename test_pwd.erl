-module(test_pwd).
-include_lib("eunit/include/eunit.hrl").

next_test() ->
	?assertEqual("b", pwd:next("a")),
	?assertEqual("ba", pwd:next("az")),
	?assertEqual("zzz", pwd:next("zzy")),
	?assertEqual("aba", pwd:next("aaz")),
	?assertEqual("zba", pwd:next("zaz")),
	?assertEqual("baa", pwd:next("azz")).
	
chr_array_test() ->
	?assertEqual("a",pwd:chr_array(0)),
	?assertEqual("z",pwd:chr_array(25)),
	?assertEqual("aa",pwd:chr_array(26)),
	?assertEqual("zz",pwd:chr_array(701)),
	?assertEqual("aaa",pwd:chr_array(702)).
	
compare_algorithm_test() ->
	%next/1 and chr_array/1 should return the same results when 
	%incremented.  This will first test "ab" through "zz"
	Int = 27,
	Chr = "aa", 
	MaxInt = 701,
	compare(Int, Chr, MaxInt),
	%now test "aab" through "zzz"
	Int3 = 703,
	Chr3 = "aaa",
	MaxInt3 = 18278,
	compare(Int3, Chr3, MaxInt3).
	
compare(MaxInt, Chr, MaxInt) ->
	ok;
compare(Int, Chr, MaxInt) ->
	io:format("On int: ~p~n",[Int]),
	?assertEqual(pwd:next(Chr), pwd:chr_array(Int)),
	compare(Int+1, pwd:next(Chr), MaxInt).
	
first_int_test() ->
	?assertEqual(0, pwd:first_int(1)),
	?assertEqual(26, pwd:first_int(2)),
	?assertEqual(702, pwd:first_int(3)),
	?assertEqual(18278, pwd:first_int(4)).
	
partition_test() ->
	?assertEqual([{"mn","zz"},{"aa","mm"}], pwd:partition_alphabet(2,2)),
 	?assertEqual([{"aaaa","zzzz"}], pwd:partition_alphabet(4,1)),
 	?assertEqual([{"qqqs","zzzz"},{"iiij","qqqr"},{"aaaa","iiii"}], pwd:partition_alphabet(4,3)).
  		
encrypt_test() ->
	?assertEqual(<<73,246,138,92,132,147,236,44,11,244,137,130,28,33,252,59>>, pwd:encrypt("hi")).

decrypt_test() ->
	Bin = <<73,246,138,92,132,147,236,44,11,244,137,130,28,33,252,59>>,
	{found,Pass,_} = pwd:decrypt(Bin,2,2), 
	?assertEqual("hi",Pass),
	BinBad = <<74,246,138,92,132,147,236,44,11,244,137,130,28,33,252,59>>,
	?assertEqual(notfound, pwd:decrypt(BinBad,2,2)).
	