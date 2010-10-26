-module(test15).
-export([start/0]).

lookup(Table, Key) ->
	ets:lookup(Table, Key).

insert(Table, Key, Item) ->
	ets:insert(Table, {Key, Item}).

manage_table() ->
	receive
		{Key, Item, ID} ->
			io:fwrite("table_manager insert1 ~w ~w~n", [Key, Item]),
			%insert(Table, Key, Item),
			ets:insert(myTable, {Key, Item}),
			io:fwrite("table_manager insert2 ~n"),
			ID ! {insert_ret, ok};
		{Key, ID} ->
			%Value = lookup(Table, Key),
			io:fwrite("table_manager lookup1 ~w~n", [key]),
			Value = ets:lookup(myTable, Key),
			io:fwrite("table_manager lookup2 ~w~n", [Value]),
			ID ! {lookup_ret, Value};
		_ ->
			io:fwrite("manage_table not supported~n")
	end,
	manage_table().

accept() ->
	receive
		{lookup_ret, Value} ->
			io:fwrite("lookup: ~w~n", [Value]);
		{insert_ret, Value} ->
			io:fwrite("insert: ~w~n", [Value]);
		{insert, Key, Item} ->
			io:fwrite("accept_manager insert ~w ~w~n", [Key, Item]),
			table_manager ! {Key, Item, self()};
		{lookup, Key} ->
			io:fwrite("accept_manager lookup ~w~n", [Key]),
			table_manager ! {Key, self()};
		_  ->
			io:fwrite("accept not supported~n")
	end,
	accept().

start() ->
	ets:new(myTable, [public, named_table]),
	Pid = spawn(fun() -> manage_table() end),
	register(table_manager, Pid),
	%spawn(test15, accept, []).
	Pid2 = spawn(fun() -> accept() end),
	register(accept_manager, Pid2),
	spawn(fun() -> test() end).

test() ->
	accept_manager ! {insert, 123, 123},
	io:fwrite("issued insert ~n"),
	accept_manager ! {lookup, 123},
	io:fwrite("issued lookup ~n").
	
