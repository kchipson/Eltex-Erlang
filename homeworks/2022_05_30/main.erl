-module(main).
-export([fibonacci_num/1, fibonacci_num_if/1]).
-export([factorial_num/1]).
-export([pow/2]).
-export([ackermann_function/2]).
-export([exact_pow_2/1]).
-export([sum_digits_num/1]).
-export([reverse_num/1]).


fibonacci_num(N) when (N < 0) or not erlang:is_integer(N) -> error;

fibonacci_num(0) -> 0;
fibonacci_num(1) -> 1;
fibonacci_num(N) -> fibonacci_num(N - 1) + fibonacci_num(N - 2).


fibonacci_num_if(N) ->
    if
        (N < 0) or not erlang:is_integer(N) -> error;
        N == 1 -> 1;
        N == 0 -> 0;
        true -> fibonacci_num_if(N - 1) + fibonacci_num_if(N - 2)
    end.


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


% 
% https://habr.com/en/post/275813/
% 

% Функция Аккермана
ackermann_function(M, N) when (N < 0) or (M < 0) or not erlang:is_integer(N) or not erlang:is_integer(M) -> error;
ackermann_function(0, N) -> N + 1;
ackermann_function(M, 0) -> ackermann_function(M - 1, 1);
ackermann_function(M, N) -> ackermann_function(M - 1, ackermann_function(M, N - 1)).

% Точная степень двойки
exact_pow_2(N) when (N =< 0) or not erlang:is_integer(N) -> error;
exact_pow_2(N) -> exact_pow_2_(N).

exact_pow_2_(1) -> true;
exact_pow_2_(N) when N rem 2 =/= 0 -> false;
exact_pow_2_(N) -> exact_pow_2_(N div 2).


% Сумма цифр числа
sum_digits_num(N) when (N < 0) or not erlang:is_integer(N) -> error;
sum_digits_num(N) -> sum_digits_list_(erlang:integer_to_list(N)).

sum_digits_list_([H|T]) -> (H - 48) + sum_digits_list_(T);
sum_digits_list_([]) -> 0.


% U: Разворот числа
reverse_num(N) when (N < 0) or not erlang:is_integer(N) -> error;
reverse_num(N) -> reverse_num(N, 0).
reverse_num(0, A) -> A;
reverse_num(N, A) -> reverse_num(N div 10, A * 10 + N rem 10). 
