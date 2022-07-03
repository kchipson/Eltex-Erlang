-module(main).
-export([main/0]).
-export([fibonacci_num/1]).
-export([factorial_num/1]).
-export([pow/2]).
-export([ackermann_function/2]).
-export([read_users_from_csv/1]).
-export([average_age_users_using_list_comprehensions/1]).

-record(user, {first_name :: string(), last_name :: string(), age :: integer(), email :: string()}).


main() ->
    TimeListFibonacci = execution_time({fibonacci_num, [40]}, 10),
    io:format("Fibonacci function(fibonacci_num(40)) running 10 times has an average time: ~w ms.~n", [lists:sum(TimeListFibonacci)/10]),
    
    TimeListFactorial = execution_time({factorial_num, [10000]}, 10),
    io:format("Factorial function(factorial_num(10000)) running 10 times has an average time: ~w ms.~n", [lists:sum(TimeListFactorial)/10]),

    TimeListPow = execution_time({pow, [2, 65000]}, 10),
    io:format("Pow function(pow(2, 65000)) running 10 times has an average time: ~w ms.~n", [lists:sum(TimeListPow)/10]),

    TimeListAckermann = execution_time({ackermann_function, [4, 1]}, 10),
    io:format("Ackermann function(ackermann_function(4, 1)) running 10 times has an average time: ~w ms.~n", [lists:sum(TimeListAckermann)/10]),

    TimeListReadFile = execution_time({read_users_from_csv, ["data.csv"]}, 10),
    io:format("ReadFile function(read_users_from_csv(\"data.csv\")) running 10 times has an average time: ~w ms.~n", [lists:sum(TimeListReadFile)/10]).


execution_time({Fun, Args}, Count) -> execution_time({Fun, Args}, Count, []).
execution_time({_, _}, 0, TimeList) -> TimeList;
execution_time({Fun, Args}, Count, TimeList) -> {Time,_} = timer:tc(?MODULE, Fun, Args), execution_time({Fun, Args}, Count - 1, [Time | TimeList]).


fibonacci_num(N) when (N < 0) or not erlang:is_integer(N) -> error;
fibonacci_num(0) -> 0;
fibonacci_num(1) -> 1;
fibonacci_num(N) -> fibonacci_num(N - 1) + fibonacci_num(N - 2).


factorial_num(N) when (N < 0) or not erlang:is_integer(N) -> error;
factorial_num(0) -> 1;
factorial_num(1) -> 1;
factorial_num(N) -> N * factorial_num(N - 1).


pow(N, E) when ((N == 0) and (E == 0)) or not erlang:is_integer(N) or not erlang:is_integer(E) -> error;
pow(0, _) -> 0;
pow(1, _) -> 1;
pow(N, E) when E < 0 -> (1 / N) * pow(N, E + 1);
pow(N, E) when E > 0 -> N * pow(N, E - 1);
pow(_, 0) -> 1. 


ackermann_function(M, N) when (N < 0) or (M < 0) or not erlang:is_integer(N) or not erlang:is_integer(M) -> error;
ackermann_function(0, N) -> N + 1;
ackermann_function(M, 0) -> ackermann_function(M - 1, 1);
ackermann_function(M, N) -> ackermann_function(M - 1, ackermann_function(M, N - 1)).


read_users_from_csv(FileName) ->
    {Status, PID} = file:open(FileName, [read, binary]),
    if
        Status == error -> error("Could not open the file / the file is missing");
        true -> read_file(PID)
    end.
read_file(PID_Device) ->
    read_lines(PID_Device, []).
read_lines(PID_Device, Result) ->
    case file:read_line(PID_Device) of
        eof  -> {ok, lists:reverse(Result)};
        {ok, Data} -> 
            Line = string:strip((unicode:characters_to_list(Data, unicode)), both, 16#000A), % LF перевод строки, символ Юникода 000A
            [User_fn, User_ln, User_age, User_email] = string:split(Line, ";", all),
            User = #user{first_name=User_fn, last_name=User_ln, age=User_age, email=User_email},
            read_lines(PID_Device, [User|Result]);
        {error, Err} -> error(io:format("~p ~p~n", ["An error occurred while reading the lines", Err]))
    end.


average_age_users_using_list_comprehensions(Users) ->
    Ages = [list_to_integer(X#user.age) || X <- Users],
    lists:sum(Ages) / length(Ages).