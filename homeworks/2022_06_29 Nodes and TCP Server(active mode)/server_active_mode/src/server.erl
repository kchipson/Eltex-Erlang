-module(server).

-export([start/0]).

% client(Client) ->
%     {ok, _Message} = gen_tcp:recv(Client, 0),
%     gen_tcp:send(Client,
%                  "HTTP/1.1 200 OK\n"
%                  "Content-type: text/html\n\n"
%                  "<h1>Hello world!</h1>"),
%     gen_tcp:close(Client).

% listen(Server) ->
%     {ok, ClientSocket} = gen_tcp:accept(Server),
%     io:format("### Сlient has connected. (~p)~n", [ClientSocket]),
%     spawn(?MODULE, client, [ClientSocket]),
%     listen(Server).
listen(Server) ->
    {ok, Socket} = gen_tcp:accept(Server),
    io:format("### New socket (~p) has been opened.~n", [Socket]),
    receive
        {tcp, Socket, _Msg} ->
        io:format("Socket (~p) got message~n", [Socket]),
        gen_tcp:send(Socket,
                "HTTP/1.1 200 OK\n"
                "Content-type: text/html\n\n"
                "<h1>Hello world!</h1>");
        {tcp_closed, Socket} ->
            io:format("Socket (~p), session closed ~n", [Socket])
    end,
    gen_tcp:close(Socket),
    listen(Server).

start() ->
    {ok, ServerSocket} = gen_tcp:listen(8080, [binary, {active, true}, {reuseaddr, true}]),
    io:format("Server has been started. (~p)~n", [ServerSocket]),
    listen(ServerSocket),
    gen_tcp:close(ServerSocket).
