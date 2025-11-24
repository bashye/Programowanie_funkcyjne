# Programowanie_funkcyjne
# Erlang - Lekcja 2: Projektowanie Aplikacji Współbieżnych
2025y. [Twoje Imię], [Twoja Uczelnia]

## 🎯 Cel zajęć
Celem dzisiejszych zajęć jest przejście od prostych skryptów do **projektowania architektury** systemu współbieżnego. Nauczymy się:
1.  Jak podzielić duży problem na niezależne procesy (Dekompozycja).
2.  Jak projektować komunikację między nimi (Grafy i Protokoły).
3.  [cite_start]Jak zaimplementować system **Event Reminder** (System Przypomnień) bazując na wzorcach OTP [cite: 5-11].

---

## 1. Metodologia: Jak myśleć "Erlangiem"?

W programowaniu obiektowym rysujemy diagramy klas. W Erlangu rysujemy **grafy procesów** i definiujemy **protokoły**.

### A. Dekompozycja: Dziel i Rządź
Zamiast pisać jeden wielki program, dzielimy go na małych "aktorów". [cite_start]W naszym systemie przypomnień wyróżniamy trzy role [cite: 32-36]:

1.  **Client (Klient)**: To interfejs użytkownika. Zleca zadania i odbiera powiadomienia.
2.  **Event Server (Mózg)**: Centralny punkt. Przyjmuje zlecenia, trzyma listę subskrybentów i zarządza procesami potomnymi.
3.  **Event Process (Timer)**: Robotnik. Każde *pojedyncze* przypomnienie to *osobny* proces. Jego zadaniem jest tylko czekać i wysłać sygnał "Gotowe!".

> **Dlaczego osobny proces dla każdego wydarzenia?**
> Jeśli jeden proces-timer ulegnie awarii (np. przez błąd w kodzie), reszta systemu działa dalej. To realizacja filozofii **"Let It Crash"**.

### B. Grafy (Architektura Systemu)
System tworzy **graf skierowany**, gdzie węzłami są procesy, a krawędziami przesyłane komunikaty.

**Wizualizacja Drzewa Nadzoru:**
```text
      [ Supervisor (Opcjonalny) ]
                 |
          [ Event Server ] <---- (Monitoruje) ---- [ Client ]
          /      |       \
     [Event X] [Event Y] [Event Z]
Gdy Client pada, Server to widzi (dzięki monitorowi). Gdy Server pada, Eventy też powinny zniknąć (dzięki linkom).

C. Kolejki (Mailboxes)
W Erlangu nie musisz implementować kolejek ręcznie. Każdy proces JEST kolejką.

Każdy proces ma Mailbox (skrzynkę pocztową).

Wiadomości wpadają tam asynchronicznie.

Instrukcja receive służy do wyciągania wiadomości. Możemy wybierać priorytetowe wiadomości ze środka kolejki dzięki dopasowaniu wzorca (Pattern Matching).

2. Protokół Komunikacji
Zanim napiszemy kod, ustalamy "język", jakim rozmawiają procesy. Każda wiadomość powinna być krotką (tuple). Używamy Ref (unikalny identyfikator), aby wiedzieć, na które zapytanie przychodzi odpowiedź.
Nadawca,Odbiorca,Wiadomość,Znaczenie
Client,Server,"{subscribe, Self}","""Chcę dostawać powiadomienia."""
Client,Server,"{add, Name, Desc, Time}","""Dodaj nowe przypomnienie."""
Client,Server,"{cancel, Name}","""Anuluj to przypomnienie."""
Event,Server,"{done, Name}","""Czas minął! Zrobione."""
Server,Client,"{done, Name, Desc}","""Użytkowniku, oto Twoje powiadomienie!"""
3. Implementacja (Krok po Kroku)
Krok 1: Pojedyncze Wydarzenie (event.erl)
To najprostszy element. Proces, który rodzi się, czeka określoną liczbę milisekund i umiera .
-module(event).
-compile(export_all).
-record(state, {server, name, to_go}).

%% API: Startowanie procesu
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% Inicjalizacja stanu
init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=Delay}).

%% Pętla Główna (The Loop)
loop(S = #state{server=Server, to_go=ToGo}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok} %% Potwierdzenie anulowania
    after ToGo * 1000 -> %% Timeout w milisekundach
        Server ! {done, S#state.name}
    end.
Krok 2: Serwer Zarządzający (evserv.erl)
Serwer musi przechowywać stan (listę klientów i wydarzeń) i działać w nieskończonej pętli .
-module(evserv).
-compile(export_all).

-record(state, {events, clients}).
-record(event, {name, description, pid, timeout}).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    %% Startujemy z pustymi listami
    loop(#state{events=orddict:new(), clients=orddict:new()}).

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

%% PĘTLA SERWERA
loop(S = #state{events=Events, clients=Clients}) ->
    receive
        %% 1. Subskrypcja (Monitorujemy klienta!)
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, Clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});

        %% 2. Dodawanie wydarzenia (Spawnujemy Event Process)
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            EventPid = event:start_link(Name, TimeOut),
            NewEvents = orddict:store(Name,
                                      #event{name=Name, description=Description, pid=EventPid, timeout=TimeOut},
                                      Events),
            Pid ! {MsgRef, ok},
            loop(S#state{events=NewEvents});

        %% 3. Anulowanie
        {Pid, MsgRef, {cancel, Name}} ->
            Events2 = case orddict:find(Name, Events) of
                {ok, E} ->
                    event:cancel(E#event.pid),
                    orddict:erase(Name, Events);
                error ->
                    Events
            end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=Events2});

        %% 4. Obsługa gotowego wydarzenia
        {done, Name} ->
            case orddict:find(Name, Events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, Clients),
                    NewEvents = orddict:erase(Name, Events),
                    loop(S#state{events=NewEvents});
                error ->
                    loop(S)
            end;

        %% 5. Obsługa awarii klienta (DOWN)
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, Clients)});

        %% 6. Hot Code Swapping
        code_change ->
            ?MODULE:loop(S);

        shutdown ->
            exit(shutdown)
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
Krok 3: Hot Code Swapping (Wymiana kodu w locie)
Zwróć uwagę na obsługę wiadomości code_change. Wywołanie ?MODULE:loop(S) jest tzw. wywołaniem zewnętrznym. Powoduje ono, że Erlang ładuje najnowszą wersję skompilowanego modułu, zachowując stary stan procesu! .

4. Zadania dla studentów
Wykonaj poniższe zadania w Jupyter Notebooku, aby przećwiczyć mechanizmy spawn, receive i after.

Zadanie 1: "Leniwy Student" (Timeouts)
Napisz moduł lazy_student.

Funkcja loop ma czekać na wiadomość ucz_sie.

Jeśli ją dostanie, wypisuje "OK, juz sie ucze..." i czeka dalej (rekurencja).

Jeśli NIE dostanie wiadomości przez 3000ms (użyj after), wypisuje "Zasnalem z nudow!" i kończy działanie (brak rekurencji).

Zadanie 2: "Matematyczny Router" (Server-Worker)
Napisz proces router, który nie blokuje się podczas obliczeń.

Przyjmuje wiadomość {oblicz, A, B}.

Zamiast liczyć samemu, tworzy (spawnuje) nowy, anonimowy proces (fun() -> ...), który wykonuje dodawanie, wypisuje wynik i znika.

Router natychmiast wraca do nasłuchiwania kolejnych zadań.

5. Rozwiązania Zadań
<details> <summary>Kliknij, aby zobaczyć rozwiązanie Zadania 1</summary>
-module(lazy_student).
-export([start/0, loop/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
    receive
        ucz_sie ->
            io:format("Student: OK, juz sie ucze...~n"),
            loop()
    after 3000 ->
        io:format("Student: Zasnalem z nudow! (Timeout)~n")
    end.
</details>

<details> <summary>Kliknij, aby zobaczyć rozwiązanie Zadania 2</summary>
-module(router).
-export([start/0, loop/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
    receive
        {oblicz, A, B} ->
            spawn(fun() ->
                Result = A + B,
                io:format("Worker: ~p + ~p = ~p~n", [A, B, Result])
            end),
            loop();
        stop ->
            ok
    end.
</details>
Źródła

Learn You Some Erlang for Great Good! (Fred Hebert) - rozdział "Designing a Concurrent Application".

Dokumentacja Erlang OTP.
