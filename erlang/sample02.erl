-module(test16).
-export([start/1]).

-define(TCP_OPTIONS_1,[binary, {packet, line}, {active, false}, {reuseaddr, true}]).

-define(TCP_OPTIONS_2,[binary, {packet, raw}, {active, false}, {reuseaddr, true}]).

-record(client, {name=none, socket, mode}).
-record(queue, {name=none, list, mode}).

start(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS_1),
	Pid = spawn(fun() -> manage_clients([]) end),
	register(client_manager, Pid),
	Pid2 = spawn(fun() -> manage_queues([]) end),
	register(queue_manager, Pid2),
	do_accept(Socket).

do_accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			io:fwrite("Accept new client~n"),
			spawn(fun() -> handle_client(Socket) end),
			client_manager ! {connect, Socket};
		{error, Reason} ->
			io:fwrite("Socket accepst error: ~s~n", [Reason])
	end,
	do_accept(LSocket).

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			io:fwrite("Recv Data: ~s~n", [Data]),
			client_manager ! {data, Socket, Data},
			handle_client(Socket);
		{error, closed} ->
			client_manager ! {disconnect, Socket}
	end.

manage_clients(Clients) ->
	receive
		{connect, Socket} ->
			Client = #client{socket=Socket, mode=connect},
			NewClients = [Client | Clients],
			io:fwrite("Set new client~n");
		{disconnect, Socket} ->
			Client = find_client(Socket, Clients),
			NewClients = lists:delete(Client, Clients);
		{quit, Client} ->
			gen_tcp:close(Client#client.socket),
			NewClients = lists:delete(Client, Clients);
		{data, Socket, Data} ->
			Client = find_client(Socket, Clients),
			queue_manager ! {data, Client, Data},
			NewClients = Clients;
		_ ->
			io:fwrite("manage_clients: unknown message~n"),
			NewClients = Clients
	end,
	manage_clients(NewClients).

find_client(Socket, Clients) ->
	{value, Client} = lists:keysearch(Socket, #client.socket, Clients),
	Client.

manage_queues(Queues) ->
	receive
		{data, Client, Data} ->
			io:fwrite("Data: ~s~n", [Data]),
			Tokens = parse_data(binary_to_list(Data)),
			io:fwirte("tokens: ~s~n", [Tokens]),
			case Tokens of
				["create", Other] ->
					io:fwrite("Create: ~s~n", [Other]);
				["qget", Other] ->
					io:fwrite("Qget: ~s~n", [Other]);
				["qset", Other] ->
					io:fwrite("Qset: ~s~n", [Other]);
				["quit"] ->
					io:fwrite("Quit: ~n"),
					client_manager ! {quit, Client};
				_ ->
					io:fwrite("parse: unknown command~n")
			end,
			send_data(Client);
		_ ->
			io:fwrite("manage_queues: unknown message~n")
	end,
	manage_queues(Queues).

parse_data(Data) ->
	io:fwrite("parse_data~n"),
	Tokens = bogostrip(bogostrip(Data), " "),
	io:fwirte("tokens: ~s~n", [Tokens]).

bogostrip(String) ->
	io:fwrite("bogostrip1: ~s~n", [String]),
	bogostrip(String, "\r\n\t").

bogostrip(String, Chars) when Chars == " " ->
	io:fwrite("bogostrip3: ~s~n", [String]),
	string:tokens(String, Chars);
bogostrip(String, Chars) ->
	io:fwrite("bogostrip2: ~s~n", [String]),
	[Stripped|_Rest] = string:tokens(String, Chars),
	io:fwrite("Stripped: ~s~n", [Stripped]),
	Stripped.

send_data(Client) ->
	gen_tcp:send(Client#client.socket, "HELLO:~n").
