-module(server).
-export([start/1]).
-export([test/0]).

response(Socket, Status, Headers, Body) ->
    gen_tcp:send(Socket, [Status, "\r\n", Headers, "\r\n\r\n", Body]).


build_json_rec([], [], Res) ->
    Res;

build_json_rec([HC | TC], [HR | TR], Res) ->
    build_json_rec(TC, TR,  Res#{ HC => HR}).

build_json_rec(Columns, Row) -> 
    build_json_rec(Columns, Row, maps:new()).


build_json(_, [], Res) ->
    jiffy:encode(#{<<"Status">> => <<"ok">>, <<"Result">> => Res});

build_json(Columns, [HR | TR], Res) ->
    build_json(Columns, TR, Res ++ [build_json_rec(Columns, HR)]).

build_json(Columns, Rows) ->
    build_json(Columns, Rows, []).

get_users() -> 
    PID = database:connection(),
    {ok, ColumnNames, Rows} = database:get_users(PID),
    database:close_connection(PID),
    build_json(ColumnNames, Rows).

get_user(Id) ->
    if
        Id == "" -> jiffy:encode(#{<<"Status">> => <<"error">>, <<"Error">> => <<"User ID must be specified. For example, users.get_user/1">>});
        true -> 
            PID = database:connection(),
            {ok, ColumnNames, Rows} = database:get_user_by_ID(PID, Id),
            database:close_connection(PID),
            if 
                Rows == [] ->
                    jiffy:encode(#{<<"Status">> => <<"error">>, <<"Error">> => <<"Not found">>});
                true -> build_json(ColumnNames, Rows)
            end           
    end.

listen(Server) ->
    {ok, Socket} = gen_tcp:accept(Server),
    io:format("--~n# New socket (~p) has been opened.~n", [Socket]),

    % {ok, RegexRequest} = re:compile("^GET (?<A>\/[\-_\.%\/[:alnum:]]*) (?<B>.*)\r\n.*", [unicode]),
    {ok, RegexRequest} = re:compile("^GET \/(?<A>.*) (?<B>.*)\r\n.*", [unicode]),
    {ok, RegexUrl} = re:compile("^(?<H>[^\/]*)(\/(?<T>.*))*$", [unicode]),
    
    receive
        {tcp, Socket, Msg} ->
            io:format("~nSocket (~p) got message:~n\t~p~n~n", [Socket, Msg]),
            case re:run(Msg, RegexRequest, [{capture, all_names, list}]) of
                {match, [Request, _Protocol]} ->
                    {match, [Res, Tail]} = re:run(Request, RegexUrl, [{capture, all_names, list}]),
                    case Res of
                        R when (R == "") or (R == "index") ->
                            response(Socket,
                            "HTTP/1.1 200 OK",
                            "Content-type: text/html",
                            "<h1>Hello world!</h1>");

                        "favicon.ico" -> 
                            pass;

                        "users.get_users" when (Tail =="") or (Tail == "/") ->
                            response(Socket,
                            "HTTP/1.1 200 OK",
                            "Content-type: application/json",
                            get_users());

                        "users.get_user" ->
                            {match, [ID, Tail_mb]} = re:run(Tail, RegexUrl, [{capture, all_names, list}]),
                            case ID of
                                Id when (Tail_mb == "") ->
                                    response(Socket,
                                    "HTTP/1.1 200 OK",
                                    "Content-type: application/json",
                                    get_user(Id));
                                _ -> 
                                    response(Socket,
                                "HTTP/1.1 404 Not Found",
                                "Content-type: text/html",
                                "<h1>404 Not Found</h1><h2>Page("++ Request ++") not found</h2>")
                                end;
                        
                        _ ->
                            response(Socket,
                            "HTTP/1.1 404 Not Found",
                            "Content-type: text/html",
                            "<h1>404 Not Found</h1><h2>Page("++ Request ++") not found</h2>")
                    end;
                {nomatch} ->
                    response(Socket,
                    "HTTP/1.1 400 Bad Request",
                    "Content-type: text/html",
                    "<h1>400 Bad Request</h1>");
                _ ->
                    response(Socket,
                    "HTTP/1.1 418 I'm a teapot",
                    "Content-type: text/html",
                    "<h1>418 I'm a teapot</h1>")
                end
    end,

    gen_tcp:close(Socket),
    io:format("# Socket (~p), session closed.~n--~n~n", [Socket]),
    listen(Server).


-define(TCP_OPTIONS, [binary, {active, true}, {reuseaddr, true}]).

start(Port) ->
    {ok, ServerSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("### Server has been started. (~p)~n", [ServerSocket]),
    listen(ServerSocket),
    gen_tcp:close(ServerSocket).


test() ->
    % {ok, Regex} = re:compile("^GET /(?<A>.*) (.*)\r\n.*", [unicode]),
    % Str = "GET /favicon.ico HTTP/1.1\r\nHost: localhost:8080\r\nConnection: keep-alive\r\nsec-ch-ua: \" Not A;Brand\";v=\"99\", \"Chromium\";v=\"101\", \"Google Chrome\";v=\"101\"\r\nDNT: 1\r\nsec-ch-ua-mobile: ?0\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.0.0 Safari/537.36\r\nsec-ch-ua-platform: \"Linux\"\r\nAccept: image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8\r\nSec-Fetch-Site: same-origin\r\nSec-Fetch-Mode: no-cors\r\nSec-Fetch-Dest: image\r\nReferer: http://localhost:8080/\r\nAccept-Encoding: gzip, deflate, br\r\nAccept-Language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7\r\n\r\n",
    % re:run(Str, Regex, [{capture, all_names, list}]).
    {ok, RegexUsers} = re:compile("^\/users\.(?<A>[\-_\.\/[:alnum:]]*)$", [unicode]),
    Str = "/users.gerge",
    re:run(Str, RegexUsers,[{capture, all_names, list}]).
    % string:slice(Str, 5, 11).