# Erlang – Projektowanie Aplikacji Współbieżnych

## Wprowadzenie

Celem projektu jest stworzenie systemu przypomnień (Reminder App) działającego na współbieżnej architekturze Erlanga, z obsługą wielu klientów, osobnymi procesami zdarzeń oraz serwerem zarządzającym całością.

Projekt demonstruje najważniejsze cechy Erlanga:

-   izolację procesów
    
-   zarządzanie stanem
    
-   odporność na awarie
    
-   hot code swapping
    
-   łatwe skalowanie
    

----------

# 📌 CZĘŚĆ 1 – Jak zaprojektować aplikację?

## 1.1. Definicja problemu

Tworzymy system przypomnień.

**Wymagania funkcjonalne:**

-   Dodanie wydarzenia (Nazwa, Opis, Czas)
    
-   Powiadomienie, gdy czas nadejdzie
    
-   Anulowanie wydarzenia po nazwie
    
-   Możliwość obsługi wielu klientów (np. GUI, WWW, API)
    

## 1.2. Klasyczne podejście (np. C++/Java)

Typowe rozwiązanie:

-   lista obiektów
    
-   pętla `while`, która sprawdza upływ czasu
    

**Problemy:**

-   przy dużej liczbie zdarzeń pętla nie wyrabia
    
-   awaria pętli zabija cały system
    

## 1.3. Architektura w Erlangu

W Erlangu rozbijamy problem na trzy role:

-   **Client** – użytkownik lub inny system
    
-   **Event Server** – menedżer:
    
    -   przyjmuje polecenia
        
    -   zarządza klientami
        
    -   nie odlicza czasu!
        
-   **Event Process** – osobny proces dla każdego przypomnienia:
    
    -   startuje
        
    -   czeka X sekund
        
    -   wysyła powiadomienie
        
    -   umiera
        

Dzięki temu awaria jednego wydarzenia nie zabija systemu.

## 1.4. Projekt komunikacji (Protokół)

### Klient → Serwer

```
{subscribe, Pid}
{add, Name, Description, TimeOut}
{cancel, Name}

```

### Serwer → Klient

```
{Ref, ok}
{error, Reason}
{done, Name, Description}

```

### Serwer ↔ Proces wydarzenia

```
cancel
{done, Id}

```

----------

# ⚙️ CZĘŚĆ 2 – Implementacja procesu wydarzenia (`event.erl`)

## 2.1. Wersja podstawowa

```erlang
%% writefile event.erl
-module(event).
-compile(export_all).
-record(state, {server, name, to_go=0}).

loop(S = #state{server=Server, to_go=ToGo}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after ToGo * 1000 ->
        Server ! {done, S#state.name}
    end.

```

## 2.2. Problem: limit 50 dni

`after` ma limit około `2^32` ms → ~49 dni.

Jak dodać przypomnienie za rok?

Rozwiązanie: dzielimy czas na kawałki poniżej limitu.

```erlang
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

```

## 2.3. Zaktualizowana pętla obsługująca listę czasów

```erlang
loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        if
            Next =:= [] ->
                Server ! {done, S#state.name};
            true ->
                loop(S#state{to_go=Next})
        end
    end.

```

## 2.4. Funkcje startowe

```erlang
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=normalize(Delay)}).

```

----------

# 🗂️ CZĘŚĆ 3 – Serwer wydarzeń (`evserv.erl`)

## 3.1. Stan serwera

```erlang
-record(state, {events, clients}).
%% events: [{Name, Pid}]
%% clients: [Pid]

```

## 3.2. Główna pętla serwera

```erlang
%% writefile evserv.erl
-module(evserv).
-compile(export_all).

-record(state, {events, clients}).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [#state{events=[], clients=[]}])).

loop(S = #state{events=Events, clients=Clients}) ->
    receive
        %% SUBSKRYPCJA
        {Pid, Ref, {subscribe, Client}} ->
            erlang:monitor(process, Client),
            NewClients = [Client | Clients],
            Pid ! {Ref, ok},
            loop(S#state{clients=NewClients});

        %% DODAWANIE
        {Pid, Ref, {add, Name, Description, TimeOut}} ->
            EventPid = event:start_link(Name, TimeOut),
            NewEvents = [{Name, EventPid} | Events],
            Pid ! {Ref, ok},
            loop(S#state{events=NewEvents});

        %% ANULOWANIE
        {Pid, Ref, {cancel, Name}} ->
            NewEvents =
                case lists:keyfind(Name, 1, Events) of
                    {Name, EventPid} ->
                        event:cancel(EventPid),
                        lists:keydelete(Name, 1, Events);
                    false ->
                        Events
                end,
            Pid ! {Ref, ok},
            loop(S#state{events=NewEvents});

        %% DONE od procesu event
        {done, Name} ->
            lists:foreach(fun(C) -> C ! {done, Name} end, Clients),
            NewEvents = lists:keydelete(Name, 1, Events),
            loop(S#state{events=NewEvents});

        %% KLIENT PADŁ
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=Clients});

        shutdown ->
            exit(shutdown);

        code_change ->
            ?MODULE:loop(S);

        Unknown ->
            io:format("Unknown: ~p~n", [Unknown]),
            loop(S)
    end.

```

## 3.3. API (ukrycie komunikacji)

Zaleca się dopisanie funkcji:

```
evserv:subscribe().
evserv:add_event().
...

```

Dzięki temu zmiana protokołu nie psuje aplikacji klienckich.

----------

# 🧪 CZĘŚĆ 4 – Testy i Hot Code Swapping

## 4.1. Test manualny

```erlang
c(event).
c(evserv).
evserv:start().
evserv:subscribe(self()).
evserv:add_event("Test", "Opis", 5).
%% po 5 sekundach:
flush().

```

## 4.2. Wymiana kodu w locie

Zmodyfikuj kod w `evserv.erl`, np. wypisz nowy komunikat w `code_change`.

W konsoli:

```erlang
c(evserv).
evserv ! code_change.

```

Serwer przeładuje kod bez zatrzymywania pracy i utraty stanu.

----------

# 🛡️ CZĘŚĆ 5 – Supervisor (`sup.erl`)

## Kod

```erlang
%% writefile sup.erl
-module(sup).
-export([start/2, init/1, loop/1]).

start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
    process_flag(trap_exit, true),
    loop({Mod, Args}).

loop({Mod, Args}) ->
    Pid = apply(Mod, start_link, Args),
    receive
        {'EXIT', Pid, Reason} ->
            io:format("Proces ~p umarł z powodu ~p. Restartuję...~n", [Pid, Reason]),
            loop({Mod, Args})
    end.

```

## Test awarii

```erlang
sup:start(evserv, []).
exit(whereis(evserv), kill).

```

Supervisor powinien automatycznie podnieść serwer ponownie.

----------

# 🎯 Podsumowanie

-   Procesy zamiast wątków – każdy event to osobny, lekki proces.
    
-   Stan prywatny – jest trzymany lokalnie w procesie.
    
-   Protokół przed kodem – najpierw komunikacja, później implementacja.
    
-   Limity istnieją – 50 dni `after` trzeba obejść.
    
-   Hot code swap – można naprawiać system bez przestojów.
    

----------

# 🧩 Zadania do samodzielnego wykonania

----------

## Zadanie 1 – Key-Value Store (`store.erl`)

```erlang
-module(store).
-export([start/0, init/0, put/2, get/1]).

start() ->
    spawn(?MODULE, init, []).

put(Pid, Key, Value) ->
    Pid ! {put, Key, Value},
    ok.

get(Pid, Key) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, {get, Key}},
    receive
        {Ref, Value} -> Value
    after 2000 ->
        {error, timeout}
    end.

init() ->
    loop(#{}).

loop(State) ->
    receive
        {put, Key, Value} ->
            NewState = maps:put(Key, Value, State),
            loop(NewState);

        {Pid, Ref, {get, Key}} ->
            Value = maps:get(Key, State, undefined),
            Pid ! {Ref, Value},
            loop(State);

        stop ->
            ok
    end.

```

----------

## Zadanie 2 – Gra „Gorący Ziemniak” (`ring.erl`)

```erlang
-module(ring).
-export([start_game/0, node_init/0]).

start_game() ->
    P1 = spawn(?MODULE, node_init, []),
    P2 = spawn(?MODULE, node_init, []),
    P3 = spawn(?MODULE, node_init, []),

    P1 ! {set_next, P2},
    P2 ! {set_next, P3},
    P3 ! {set_next, P1},

    P1 ! {token, 15},
    ok.

node_init() ->
    receive
        {set_next, NextPid} ->
            io:format("~p: Mam sąsiada ~p~n", [self(), NextPid]),
            node_loop(NextPid)
    end.

node_loop(NextPid) ->
    receive
        {token, 0} ->
            io:format("~p: Koniec gry!~n", [self()]);

        {token, Round} ->
            io:format("~p: Round ~p → dalej do ~p~n", [self(), Round, NextPid]),
            timer:sleep(500),
            NextPid ! {token, Round - 1},
            node_loop(NextPid)
    end.

```

----------

## Zadanie 3 – Load Balancer (`lb.erl`)

```erlang
-module(lb).
-export([start/0, send_package/1, manager_init/0, worker_loop/1]).

start() ->
    register(manager, spawn(?MODULE, manager_init, [])).

send_package(PackageName) ->
    manager ! {job, PackageName},
    ok.

manager_init() ->
    W1 = spawn(?MODULE, worker_loop, [1]),
    W2 = spawn(?MODULE, worker_loop, [2]),
    W3 = spawn(?MODULE, worker_loop, [3]),
    Workers = [W1, W2, W3],
    manager_loop(Workers).

manager_loop(Workers) ->
    receive
        {job, PackageName} ->
            Index = rand:uniform(length(Workers)),
            Selected = lists:nth(Index, Workers),
            Selected ! {deliver, PackageName},
            manager_loop(Workers)
    end.

worker_loop(Id) ->
    receive
        {deliver, PackageName} ->
            io:format("Kurier ~p: odbieram '~s'~n", [Id, PackageName]),
            timer:sleep(2000),
            io:format("Kurier ~p: dostarczono '~s'~n", [Id, PackageName]),
            worker_loop(Id)
    end.

```
