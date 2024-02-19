-module(mobius).
-export([
    is_prime/1,
    prime_factors/1,
    is_square_multiple/1,
    find_square_multiples/2
]).

%%% 2.1 Проверка простого числа
is_prime(N) when N > 1 ->
    is_prime(N, 2).

is_prime(N, Divisor) when Divisor * Divisor > N -> true;
is_prime(N, Divisor) ->
    if
        N rem Divisor =:= 0 -> false;
        true -> is_prime(N, Divisor + 1)
    end.



%%% 2.2 Функция для нахождения простых сомножителей числа N
prime_factors(N) when N > 1 ->
    prime_factors(N, 2, []).

prime_factors(1, _, Factors) -> 
    Factors;
prime_factors(N, Factor, Factors) ->
    case N rem Factor of
        0 ->
            prime_factors(N div Factor, Factor, [Factor | Factors]);
        _ ->
            prime_factors(N, Factor + 1, Factors)
    end.


%%% 2.3 Функция для проверки, является ли число квадратом простого числа
is_square_multiple(N) when N > 1 ->
    Factors = prime_factors(N),
    SortedFactors = lists:sort(Factors),
    is_square_multiple(SortedFactors, false).

is_square_multiple([], Acc) ->
    Acc;
is_square_multiple([_], Acc) ->
    Acc;
is_square_multiple([X, X | _], _) ->
    true;
is_square_multiple([_ | T], Acc) ->
    is_square_multiple(T, Acc).


%%% 2.4 Нахождение последовательности
find_square_multiples(Count, MaxN) -> find_square_multiples(Count, 0, 2, MaxN).


find_square_multiples(Count, Increment, Number, _) when Count == Increment -> Number - Count;
find_square_multiples(_, _, Number, MaxN) when Number > MaxN -> fail;
find_square_multiples(Count, Increment, Number, MaxN) ->
  case is_square_multiple(Number) of
    false -> find_square_multiples(Count, 0, Number + 1, MaxN);
    true -> find_square_multiples(Count, Increment + 1, Number + 1, MaxN)
  end.