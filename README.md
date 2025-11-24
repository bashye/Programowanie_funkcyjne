# Erlang - Lekcja 2: Projektowanie Aplikacji Współbieżnych
2025y. Michał Łepkowski, UMK WMII

## 🎯 Cel zajęć
Na dzisiejszych zajęciach przejdziemy od pisania prostych funkcji do **projektowania architektury** systemu współbieżnego. Nauczymy się:
1.  Jak podzielić problem na procesy (Dekompozycja i Grafy).
2.  Jak zdefiniować protokół komunikacji (Kolejki wiadomości).
3.  [cite_start]Jak zaimplementować **Event Reminder** (System Przypomnień) wykorzystując mechanizmy Erlang/OTP [cite: 5-11].

---

## 1. Metodologia Projektowania: Jak myśleć "Erlangiem"?

W programowaniu obiektowym (OOP) rysujemy diagramy klas. W Erlangu rysujemy **grafy procesów** i definiujemy **protokoły**.

### A. Dekompozycja Problemu
Zamiast pisać jeden wielki program, dzielimy go na niezależnych "aktorów" (procesy). [cite_start]Analizując problem "Przypominacza", wyróżniamy trzy role [cite: 26-32]:

1.  **Client (Klient)**: Interfejs użytkownika (konsola/proces shella). Zleca zadania i odbiera powiadomienia.
2.  **Event Server (Zarządca/Mózg)**:
    * Przyjmuje subskrypcje od klientów.
    * Przechowuje listę aktywnych wydarzeń.
    * [cite_start]Jest jedynym punktem kontaktu dla Klienta [cite: 33-37].
3.  **Event Process (X, Y, Z - Robotnicy)**:
    * Reprezentuje **pojedyncze** oczekujące powiadomienie.
    * Działa jak minutnik: czeka, wysyła sygnał "Gotowe" i umiera.
    * [cite_start]Jeśli jeden proces (X) ulegnie awarii, reszta systemu działa dalej (filozofia **"Let It Crash"**) [cite: 46-49].

### B. Graf Architektury (Drzewo Nadzoru)
System tworzy graf skierowany. Ważne jest zrozumienie relacji "kto kogo stworzył" i "kto kogo obserwuje".

```mermaid
graph TD
    Client((Client)) -- subscribe/add/cancel --> Server((Event Server))
    Server -- spawn --> Event1((Event X))
    Server -- spawn --> Event2((Event Y))
    Server -- spawn --> Event3((Event Z))
    Client -. monitor .-> Server
    Server -. monitor .-> Client
(Gdy Client pada, Server to widzi dzięki monitorowi. Gdy Server pada, Eventy też powinny zniknąć dzięki linkom) .

C. Kolejki Wiadomości (Mailboxes)
W Erlangu każdy proces posiada własną skrzynkę pocztową (Mailbox).

Działa ona jak asynchroniczna kolejka.

Instrukcja receive służy do wyciągania wiadomości.

Dzięki Pattern Matchingowi (dopasowaniu wzorca) możemy wyciągać wiadomości ze środka kolejki (np. priorytety), a nie tylko pierwszą z brzegu (FIFO).

2. Protokół Komunikacji (Interface)
Zanim napiszemy kod, musimy ustalić "język", jakim rozmawiają procesy. Każdą interakcję definiujemy jako komunikat .

Używamy wzorca {Pid, Ref, Wiadomość}, gdzie Ref (unikalna referencja) pozwala powiązać odpowiedź z konkretnym zapytaniem.
Nadawca,Odbiorca,Treść Wiadomości,Opis
Client,Server,"{subscribe, Self}",Klient chce otrzymywać powiadomienia.
Client,Server,"{add, Name, Desc, Time}",Dodaj nowe przypomnienie.
Client,Server,"{cancel, Name}",Anuluj przypomnienie.
Event,Server,"{done, Name}","Proces timera zgłasza: ""Czas minął!""."
Server,Client,"{done, Name, Desc}",Serwer przekazuje powiadomienie użytkownikowi.
3. Implementacja (Krok po Kroku)
Krok 1: Moduł Pojedynczego Wydarzenia (event.erl)
Zaczynamy od najmniejszego klocka. To proces, który używa receive ... after do odliczania czasu.
-module(event).
-compile(export_all).
-record(state, {server, name, to_go}).

%% --- API ---
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% --- Wnętrzności procesu ---
init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=Delay}).

%% Sercem jest pętla loop
loop(S = #state{server=Server, to_go=ToGo}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok} %% Potwierdzenie anulowania
    after ToGo * 1000 -> %% Timeout w milisekundach (Erlang liczy w ms)
        Server ! {done, S#state.name}
    end.

%% Funkcja pomocnicza do bezpiecznego anulowania
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
Oto kompletna treść lekcji przygotowana w jednym bloku Markdown. Możesz skopiować całość i wkleić bezpośrednio do pliku README.md w swoim repozytorium lub do komórki Markdown w Jupyter Notebook.Markdown# Erlang - Lekcja 2: Projektowanie Aplikacji Współbieżnych
2025y. [Twoje Imię], [Twoja Uczelnia]

## 🎯 Cel zajęć
Na dzisiejszych zajęciach przejdziemy od pisania prostych funkcji do **projektowania architektury** systemu współbieżnego. Nauczymy się:
1.  Jak podzielić problem na procesy (Dekompozycja i Grafy).
2.  Jak zdefiniować protokół komunikacji (Kolejki wiadomości).
3.  [cite_start]Jak zaimplementować **Event Reminder** (System Przypomnień) wykorzystując mechanizmy Erlang/OTP [cite: 5-11].

---

## 1. Metodologia Projektowania: Jak myśleć "Erlangiem"?

W programowaniu obiektowym (OOP) rysujemy diagramy klas. W Erlangu rysujemy **grafy procesów** i definiujemy **protokoły**.

### A. Dekompozycja Problemu
Zamiast pisać jeden wielki program, dzielimy go na niezależnych "aktorów" (procesy). [cite_start]Analizując problem "Przypominacza", wyróżniamy trzy role [cite: 26-32]:

1.  **Client (Klient)**: Interfejs użytkownika (konsola/proces shella). Zleca zadania i odbiera powiadomienia.
2.  **Event Server (Zarządca/Mózg)**:
    * Przyjmuje subskrypcje od klientów.
    * Przechowuje listę aktywnych wydarzeń.
    * [cite_start]Jest jedynym punktem kontaktu dla Klienta [cite: 33-37].
3.  **Event Process (X, Y, Z - Robotnicy)**:
    * Reprezentuje **pojedyncze** oczekujące powiadomienie.
    * Działa jak minutnik: czeka, wysyła sygnał "Gotowe" i umiera.
    * [cite_start]Jeśli jeden proces (X) ulegnie awarii, reszta systemu działa dalej (filozofia **"Let It Crash"**) [cite: 46-49].

### B. Graf Architektury (Drzewo Nadzoru)
System tworzy graf skierowany. Ważne jest zrozumienie relacji "kto kogo stworzył" i "kto kogo obserwuje".

```mermaid
graph TD
    Client((Client)) -- subscribe/add/cancel --> Server((Event Server))
    Server -- spawn --> Event1((Event X))
    Server -- spawn --> Event2((Event Y))
    Server -- spawn --> Event3((Event Z))
    Client -. monitor .-> Server
    Server -. monitor .-> Client
(Gdy Client pada, Server to widzi dzięki monitorowi. Gdy Server pada, Eventy też powinny zniknąć dzięki linkom) 1.C. Kolejki Wiadomości (Mailboxes)W Erlangu każdy proces posiada własną skrzynkę pocztową (Mailbox).Działa ona jak asynchroniczna kolejka.Instrukcja receive służy do wyciągania wiadomości.Dzięki Pattern Matchingowi (dopasowaniu wzorca) możemy wyciągać wiadomości ze środka kolejki (np. priorytety), a nie tylko pierwszą z brzegu (FIFO).2. Protokół Komunikacji (Interface)Zanim napiszemy kod, musimy ustalić "język", jakim rozmawiają procesy. Każdą interakcję definiujemy jako komunikat 2.Używamy wzorca {Pid, Ref, Wiadomość}, gdzie Ref (unikalna referencja) pozwala powiązać odpowiedź z konkretnym zapytaniem.NadawcaOdbiorcaTreść WiadomościOpisClientServer{subscribe, Self}Klient chce otrzymywać powiadomienia.ClientServer{add, Name, Desc, Time}Dodaj nowe przypomnienie.ClientServer{cancel, Name}Anuluj przypomnienie.EventServer{done, Name}Proces timera zgłasza: "Czas minął!".ServerClient{done, Name, Desc}Serwer przekazuje powiadomienie użytkownikowi.3. Implementacja (Krok po Kroku)Krok 1: Moduł Pojedynczego Wydarzenia (event.erl)Zaczynamy od najmniejszego klocka. To proces, który używa receive ... after do odliczania czasu.Erlang-module(event).
-compile(export_all).
-record(state, {server, name, to_go}).

%% --- API ---
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% --- Wnętrzności procesu ---
init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=Delay}).

%% Sercem jest pętla loop
loop(S = #state{server=Server, to_go=ToGo}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok} %% Potwierdzenie anulowania
    after ToGo * 1000 -> %% Timeout w milisekundach (Erlang liczy w ms)
        Server ! {done, S#state.name}
    end.

%% Funkcja pomocnicza do bezpiecznego anulowania
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
Krok 2: Serwer Zarządzający (evserv.erl)
Serwer musi przechowywać stan (listę klientów i wydarzeń). Użyjemy do tego słownika (orddict) trzymanego w pętli rekurencyjnej.
-module(evserv).
-compile(export_all).

-record(state, {events, clients}).
-record(event, {name, description, pid, timeout}).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    %% Startujemy z pustymi listami
    loop(#state{events=orddict:new(), clients=orddict:new()}).

%% --- Pętla Główna Serwera ---
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

        %% 3. Anulowanie wydarzenia
        {Pid, MsgRef, {cancel, Name}} ->
            Events2 = case orddict:find(Name, Events) of
                {ok, E} ->
                    event:cancel(E#event.pid), %% Zabijamy proces timera
                    orddict:erase(Name, Events);
                error ->
                    Events
            end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=Events2});

        %% 4. Obsługa gotowego wydarzenia (Wiadomość od procesu Event)
        {done, Name} ->
            case orddict:find(Name, Events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, Clients),
                    NewEvents = orddict:erase(Name, Events),
                    loop(S#state{events=NewEvents});
                error ->
                    loop(S)
            end;

        %% 5. Sprzątanie po awarii klienta (Sygnał z Monitora)
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, Clients)});

        %% 6. Hot Code Swapping
        code_change ->
            ?MODULE:loop(S);

        shutdown ->
            exit(shutdown);

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

%% Pomocnicze funkcje
add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
Krok 3: Hot Code Swapping (Wymiana kodu w locie)
Erlang pozwala podmienić kod działającego serwera bez zatrzymywania go. W kodzie powyżej odpowiada za to fragment:
code_change ->
    ?MODULE:loop(S);
Wywołanie ?MODULE:loop(S) (tzw. "external call") zmusza VM Erlanga do załadowania najnowszej wersji modułu z dysku, zachowując przy tym obecny stan S .
4. Zadania dla studentów 💻
Wykonaj poniższe zadania w Jupyter Notebooku, aby przećwiczyć mechanizmy współbieżności.

Zadanie 1: "Leniwy Student" (Mechanizm Timeout)
Stwórz proces symulujący studenta przed sesją.

Proces ma funkcję loop.

Czeka na wiadomość ucz_sie. Jeśli ją dostanie, wypisuje "OK, ucze sie..." i wraca do pętli (rekurencja).

Jeśli NIE dostanie żadnej wiadomości przez 3000ms (użyj after), proces wypisuje "Zasnalem z nudow!" i kończy działanie (brak rekurencji).

Zadanie 2: "Matematyczny Router" (Server-Worker)
Napisz proces router, który rozdziela zadania, aby nie blokować głównej pętli.

Router przyjmuje wiadomość {oblicz, A, B}.

Dla każdego zadania spawnuje nowy, krótko żyjący proces (anonimową funkcję fun), który dodaje liczby, wypisuje wynik i znika.

Router natychmiast wraca do nasłuchiwania (receive), gotowy na kolejne zadania, podczas gdy tamte się liczą w tle.

5. Rozwiązania Zadań 🔐
<details> <summary><b>Kliknij tutaj, aby zobaczyć rozwiązanie Zadania 1</b></summary>
-module(lazy_student).
-export([start/0, loop/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
    receive
        ucz_sie ->
            io:format("Student: OK, juz sie ucze...~n"),
            loop() %% Rekurencja - student czuwa dalej
    after 3000 ->
        io:format("Student: Zasnalem z nudow! (Timeout)~n")
        %% Brak rekurencji - proces umiera naturalnie
    end.
</details>

<details> <summary><b>Kliknij tutaj, aby zobaczyć rozwiązanie Zadania 2</b></summary>
-module(router).
-export([start/0, loop/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
    receive
        {oblicz, A, B} ->
            %% Spawnujemy workera - router nie jest blokowany przez obliczenia!
            spawn(fun() ->
                Wynik = A + B,
                io:format("Worker (PID ~p): ~p + ~p = ~p~n", [self(), A, B, Wynik])
            end),
            loop();
        stop ->
            io:format("Router zamyka biuro.~n"),
            ok
    end.
</details>

Materiały źródłowe:

Learn You Some Erlang for Great Good! (Fred Hebert) - Chapter: Designing a Concurrent Application.
