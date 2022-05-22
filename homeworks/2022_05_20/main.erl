-module(main).

-export([test_arithmetic_operations/0]).
-export([collecting_information/0]).

test_arithmetic_operations() ->
    io:format("~w ~s ~w = ~w~n", [2, "+", 3, 2 + 3]),
    io:format("~w ~s ~w = ~w~n", [2, "-", 3, 2 - 3]),
    io:format("~w ~s ~w = ~w~n", [2, "*", 3, 2 * 3]),
    io:format("~w ~s ~w = ~w~n", [2, "/", 3, 2 / 3]),
    io:format("~n"),
    io:format("~w ~s ~w = ~w~n", [2, "**", 1000, math:pow(2, 1000)]),
    io:format("~n"),
    io:format("~w ~s ~w = ~w~n", [0.2, "+", 0.1, 0.2 + 0.1]),
    io:format("~w ~s ~w = ~w~n", [0.6, "-", 0.3, 0.6 - 0.3]),
    io:format("~w ~s ~w = ~w~n", [0.2, "*", 0.3, 0.2 * 0.3]),
    io:format("~w ~s ~w = ~w~n", [0.6, "/", 0.3, 0.6 / 0.3]),
    io:format("~n"),
    io:format("~w ~s ~w = ~w~n", [10, "div", 3, 10 div 3]),
    io:format("~w ~s ~w = ~w~n", [10, "rem", 3, 10 rem 3]),
    io:format("~n"),
    io:format("~w ~s ~w = ~w~n", [2#1010, "&", 2#1000, 2#1010 band 2#1000]),
    io:format("~w ~s ~w = ~w~n", [2#1010, "|", 2#1000, 2#1010 bor 2#1000]),
    io:format("~w ~s ~w = ~w~n", [2#1010, "^", 2#1000, 2#1010 bxor 2#1000]),
    io:format("~w ~s ~w = ~w~n", [2#1000, "<<", 2#1, 2#1000 bsl 2#1]),
    io:format("~w ~s ~w = ~w~n", [2#1000, ">>", 2#1, 2#1000 bsr 2#1]).

collecting_information() ->
    Data = request_information(),
    io:format("~nOutput of the entered information:~n"),
    print_information(Data).

request_information() ->
    Name = element(2, io:fread("Name  : ", "~s")),
    Email = element(2,io:fread("Email : ", "~s")),
    Name ++ Email.

print_information(Data) ->
    io:format("     Name : ~s~n     Email: ~s~n", Data).
