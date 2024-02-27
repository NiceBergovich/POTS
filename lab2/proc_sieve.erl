-module(proc_sieve).
-export([
    generate/1,
    sieve2/2,
    gen_print/1
]).

-define(TIMEOUT, 1_000_000).


sieve2(0, InvalidPid) ->
    receive 
        P -> sieve2(P, InvalidPid)
    after ?TIMEOUT ->
        io:format("Время на таймаут вышло!\n")
    end; 

sieve2(P, NextPid) when is_pid(NextPid) ->
    receive 
        {done, From} ->
            NextPid ! {done, self()},
            receive 
                LstOfRes -> From ! [P] ++ LstOfRes 
            end;
        N when N rem P == 0 -> % Когда остаток от деления равен 0
            sieve2(P, NextPid); 
        N when N rem P /= 0 -> % Когда остаток от деления не равен 0
            NextPid ! N,
            sieve2(P, NextPid)
    after ?TIMEOUT -> io:format("Время на таймаут вышло!\n")
    end;
 sieve2(P, Invalid) ->
    receive 
        {done, From} ->
            %% нет последующего процесса, просто отправляем результат обратно
            From ! [P];
        N when N rem P == 0 -> 
            sieve2(P, Invalid);
        N when N rem P /= 0 -> 
            Pid = spawn(proc_sieve, sieve2, [0, void]),
            Pid ! N,
            sieve2(P, Pid)
    after ?TIMEOUT -> io:format("Время на таймаут вышло!\n")
    end.
    
sieve() -> spawn(proc_sieve, sieve2, [0, void]).

generate(MaxN) ->
    Pid = sieve(),
    generate2(Pid, 2, MaxN).

generate2(Pid, End, End) ->
    Pid ! {done, self()},
    receive
        Res -> Res
    end;

generate2(Pid, N, End) ->
    Pid ! N,
    generate2(Pid, N + 1, End).

gen_print(MaxN) ->
    Lst = generate(MaxN),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Lst).
