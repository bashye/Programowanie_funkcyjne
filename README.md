Erlang – Lekcja 2: Projektowanie Aplikacji Współbieżnych

2025 • 
𝑇
𝑤
𝑜
𝑗
𝑒
𝐼
𝑚
𝑖
ę
TwojeImię, 
𝑇
𝑤
𝑜
𝑗
𝑎
𝑈
𝑐
𝑧
𝑒
𝑙
𝑛
𝑖
𝑎
TwojaUczelnia

Cel zajęć

Zrozumienie, jak zaprojektować, podzielić i zaimplementować system oparty na wielu procesach w Erlangu.
Stworzymy aplikację Event Reminder (System Przypomnień), opierając się na architekturze OTP.

1. Teoria: Jak myśleć procesami?

W programowaniu obiektowym (OOP) modelujemy system za pomocą klas i obiektów.
W Erlangu modelujemy go za pomocą procesów i protokółów komunikacji.

A. Dekompozycja Problemu (Graf Procesów)

System przypomnień musi działać współbieżnie — nie może być jedną pętlą while.

Procesy:

Client – interfejs użytkownika (np. shell).

Event Server – centralny serwer:

przyjmuje subskrypcje,

zarządza listą wydarzeń,

kontaktuje klientów z procesami wydarzeń.

Event Process (X,Y,Z) – jeden proces = jedno przypomnienie:

czeka X czasu, wysyła „Już!”,

może zostać anulowany,

awaria jednego nie zatrzymuje reszty (fault isolation).

B. Protokół i Skrzynki Pocztowe (Mailboxes)

Każdy proces ma własną skrzynkę odbiorczą:

Pid ! Msg — wysłanie wiadomości

receive ... end — odbiór wiadomości

Protokół:

{subscribe, Self}

{add, Name, Desc, Time}

{cancel, Name}

{done, Name}

2. Implementacja Krok po Kroku
Krok 1: Pojedyncze Wydarzenie (event.erl)

Kod procesu, który czeka określony czas i sygnalizuje zakończenie.

-module(event).
-compile(export_all).

-record(state, {server, name, to_go}).

%% Funkcja startująca proces
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
    loop(#state{server = Server, name = EventName, to_go = Delay}).

%% Główna pętla procesu
loop(S = #state{server = Server, to_go = ToGo}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after ToGo * 1000 ->
        Server ! {done, S#state.name}
    end.

Krok 2: Interfejs — ukrywanie komunikacji cancel/1
cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.

Krok 3: Serwer Wydarzeń (evserv.erl)
-module(evserv).
-compile(export_all).

-record(state, {events, clients}).  %% events = orddict(), clients = orddict()
-record(event, {name, description, pid, timeout}).

%% Pętla serwera
loop(S = #state{events = Events, clients = Clients}) ->
    receive
        %% 1. Subskrypcja klienta
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, Clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients = NewClients});

        %% 2. Dodawanie wydarzenia
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            EventPid = event:start_link(Name, TimeOut),
            NewEvents = orddict:store(Name,
                                       #event{name = Name, description = Description,
                                              pid = EventPid, timeout = TimeOut},
                                       Events),
            Pid ! {MsgRef, ok},
            loop(S#state{events = NewEvents});

        %% 3. Anulowanie wydarzenia
        {Pid, MsgRef, {cancel, Name}} ->
            Events2 =
                case orddict:find(Name, Events) of
                    {ok, E} ->
                        event:cancel(E#event.pid),
                        orddict:erase(Name, Events);
                    error ->
                        Events
                end,
            Pid ! {MsgRef, ok},
            loop(S#state{events = Events2});

        %% 4. Obsługa zakończonego wydarzenia
        {done, Name} ->
            case orddict:find(Name, Events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, Clients),
                    NewEvents = orddict:erase(Name, Events),
                    loop(S#state{events = NewEvents});
                error ->
                    loop(S)
            end;

        %% 5. Hot code swap
        code_change ->
            ?MODULE:loop(S);

        shutdown ->
            exit(shutdown)
    end.

%% Start serwera
init() ->
    loop(#state{events = orddict:new(), clients = orddict:new()}).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

%% Wysyłanie wiadomości do wszystkich klientów
send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

3. Bezpieczeństwo i „Let It Crash”

monitor, link, separacja błędów — Erlang radzi sobie z awarią poprzez izolację procesów.

4. Zadania dla Studentów
Zadanie 1 — „Leniwy Student”

Napisz proces, który:

reaguje na ucz_sie,

zasypia po 3000 ms,

kończy się po koniec.

Zadanie 2 — „Prosty Rozdzielacz Zadań”

Proces router:

przyjmuje {oblicz, dodaj, A, B} i {oblicz, mnoz, A, B},

tworzy proces-worker do obliczeń.

👨‍🏫 Rozwiązania
Rozwiązanie Zadania 1 — lazy_student.erl
-module(lazy_student).
-export([start/0, loop/0]).

start() -> spawn(?MODULE, loop, []).

loop() ->
    receive
        ucz_sie ->
            io:format("Student: OK, juz sie ucze...~n"),
            loop();
        koniec ->
            io:format("Student: Koniec zajec!~n")
    after 3000 ->
        io:format("Student: Zasnalem z nudow! (Timeout)~n")
    end.

Rozwiązanie Zadania 2 — router.erl
-module(router).
-export([start/0, loop/0]).

start() -> spawn(?MODULE, loop, []).

loop() ->
    receive
        {oblicz, dodaj, A, B} ->
            spawn(fun() ->
                io:format("Worker: Wynik dodawania ~p + ~p = ~p~n", [A, B, A + B])
            end),
            loop();

        {oblicz, mnoz, A, B} ->
            spawn(fun() ->
                io:format("Worker: Wynik mnozenia ~p * ~p = ~p~n", [A, B, A * B])
            end),
            loop();

        stop ->
            io:format("Router: Zamykam biuro.~n")
    end.

Diagramy pomocnicze

Drzewo nadzoru (Supervisor → Server → Event processy)

Cykl życia wiadomości:
Client → Server Mailbox → Pattern match → Zmiana stanu → loop()
