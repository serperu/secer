-module(test_etorrent).
-compile(export_all).

% GENERAL
	
file(o1) -> 
	'etorrent_dht_net.erl';
file(o2) -> 
	'etorrent_bcoding.erl';
file(n1) -> 
	'etorrent_dht_net_new.erl';
file(n2) -> 
	'etorrent_bcoding_new.erl'.

% POIs

poio1() ->
	{file(o1), 622, call, 1}.
poio2() ->
	{file(o1), 624, call, 1}.
poio3() ->
	{file(o1), 625, 'case', 1}.

poin1() ->
	{file(n1), 622, call, 1}.
poin2() ->
	{file(n1), 624, call, 1}.
poin3() ->
	{file(n1), 625, 'case', 1}.


rel1() ->
	[{poio1(), poin1()}, 
	 {poio2(), poin2()}, 
	 {poio3(), poin3()}].

funs() ->
	"[query_ping_0_test/0]".
