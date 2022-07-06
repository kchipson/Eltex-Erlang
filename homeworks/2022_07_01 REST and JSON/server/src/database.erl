-module(database).
-export([connection/0, close_connection/1]).
-export([create_table/1, fill_init_data/1, delete_table/1]).
-export([add_user/5, delete_user_by_ID/2]).
-export([get_users/1, get_user_by_ID/2]).

connection() ->
    {ok, PID} = mysql:start_link([{host, "localhost"}, {user, "kchipson"}, {database, "erlang"}]),
    PID.

close_connection(PID) ->
    mysql:stop(PID).

create_table(PID) ->
    ok = mysql:query(PID, <<"CREATE TABLE IF NOT EXISTS Users (id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT, lastname VARCHAR(255) NOT NULL, name VARCHAR(255) NOT NULL, age INT UNSIGNED NOT NULL, email VARCHAR(255));">>).


delete_table(PID) ->
    ok = mysql:query(PID, <<"DROP TABLE IF EXISTS Users;">>).


fill_init_data(PID) ->
    ok = mysql:query(PID, <<"""
        INSERT INTO `Users` (name, lastname, age, email) VALUES 
        ('Иван','Иванов',18,'ivan.ivanov@mail.ru'),
        ('Петр', 'Петров', 52, 'pertovich70@yandex.ru'),
        ('John', 'Cena', 45, 'john_cena@gmail.com'),
        ('Rick', 'Sanchez', 60, 'RickAndMorty@adultswim.cartoonnetwork.com');
    """>>).

    
add_user(PID, Name, LastName, Age, Email) ->
    ok = mysql:query(PID, <<"INSERT INTO Users (name, lastname, age, email) VALUES (?, ?, ?, ?);">>, [Name, LastName, Age, Email]).

delete_user_by_ID(PID, ID) -> 
    ok = mysql:query(PID, <<"DELETE FROM Users WHERE id=(?);">>, [ID]).

get_users(PID) ->
    {ok, _ColumnNames, _Rows} = mysql:query(PID, <<"SELECT * FROM Users;">>).

get_user_by_ID(PID, ID) ->
    {ok, _ColumnNames, _Row} = mysql:query(PID, <<"SELECT * FROM Users WHERE id=(?);">>, [ID]).
