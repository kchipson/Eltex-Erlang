-module(main).
-export([read_users_from_csv/1]).
-export([print_users/1]).
-export([average_age_users/1]).

-record(user, {first_name :: string(), last_name :: string(), age :: integer(), email :: string()}).


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


print_users([H|T]) ->
    io:format("~ts ~ts ~ts ~ts~n", [H#user.first_name, H#user.last_name, H#user.age, H#user.email]),
    print_users(T);

print_users([]) -> ok.


average_age_users(Users) -> average_age_users(Users, 0, 0).

average_age_users([H|T], GeneralAge, Count) -> average_age_users(T, GeneralAge + list_to_integer(H#user.age), Count + 1);

average_age_users([],GeneralAge, Count) -> GeneralAge / Count.
