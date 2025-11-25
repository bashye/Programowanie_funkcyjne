# Erlang: Projektowanie Aplikacji Współbieżnych

## CZĘŚĆ 1: Teoria i Projektowanie (20 min)
*Nie zaczynaj od kodu. Zacznij od problemu.*

### 1.1. Definicja problemu
Chcemy napisać system przypomnień (Reminder App).

#### Wymagania 1:
- Dodawanie wydarzenia (Nazwa, Opis, Czas).
- Powiadomienie, gdy czas nadejdzie.
- Anulowanie wydarzenia po nazwie.
- Obsługa wielu klientów (możliwość podpięcia GUI, WWW w przyszłości).

### 1.2. Dyskusja: Jak to zaprojektować? (Interakcja ze studentami)
Zadaj pytanie: *"Jak byście to napisali w C++ lub Javie?"*

- Odp: Pewnie lista obiektów i pętla while, która co sekundę sprawdza czas.
- Problem: Co jeśli mamy miliony wydarzeń? Pętla się dławi. Co jeśli pętla padnie? Tracimy wszystko.

### 1.3. Architektura w Erlangu (Rysujemy na tablicy/ekranie)
W Erlangu dzielimy problem na małe, niezależne byty. W PDF zaproponowano podział na 3 role:

1. **Klient (Client):** Użytkownik lub inny system. Zleca zadania.
2. **Serwer Wydarzeń (Event Server):** Menadżer.
- Przyjmuje zlecenia (dodaj/anuluj).
- Zarządza subskrybentami (komu wysłać powiadomienie?).
- NIE odlicza czasu! (To kluczowe dla dekompozycji).
3. **Procesy Wydarzeń (Event Processes - X, Y, Z):**
- Każde przypomnienie to osobny proces.
- Proces startuje, czeka X czasu, wysyła wiadomość i umiera.
- Są lekkie (można ich mieć miliony).
> **Kluczowa uwaga profesora:** To jest dekompozycja problemu. Jeśli proces "Przypomnienie o pizzy" ulegnie awarii, serwer działa dalej, a "Przypomnienie o spotkaniu" jest bezpieczne.

### Projektowanie Protokołu (Kontrakt)
Zanim napiszemy linijkę kodu, musimy ustalić język komunikacji. To symuluje pracę inżyniera.

Wypiszcie wspólnie wiadomości 4:

- **Klient -> Serwer:**
	- `{subscribe, Pid} `
	- `{add, Name, Description, TimeOut}`
	- `{cancel, Name}`
- **Serwer -> Klient:**
	- `{Ref, ok}` lub `{error, Reason}`
	- `{done, Name, Description}` (Gdy czas minie)
- **Serwer <-> Proces Wydarzenia (Wewnętrzne):**
	- Serwer -> Proces: cancel
	- Proces -> Serwer: {done, Id}

---
## CZĘŚĆ 2: Implementacja "Robotnika" (Moduł event) (25 min)
*Tu wchodzi "mięso" i rozwiązywanie problemów implementacyjnych.*

### 2.1. Wersja naiwna (Szybki kod)
Piszemy prosty proces, który czeka.
```erlang
%%writefile event.erl
-module(event).
-compile(export_all).
-record(state, {server, name, to_go=0}).

loop(S = #state{server=Server, to_go=ToGo}) ->
    receive
        {Server, Ref, cancel} -> Server ! {Ref, ok}
    after ToGo * 1000 ->
        Server ! {done, S#state.name}
    end.
```

### 2.2. Problem inżynierski: Limit 50 dni (Ważne!)
Tutaj zatrzymaj studentów.

Pytanie: "Co się stanie, jeśli ustawię przypomnienie na za 2 lata?"
Odp: Erlang crashnie. Wartość after w milisekundach jest ograniczona do około 50 dni (dokładnie $2^{32}$ ms)5.

Zadanie: Jak to naprawić bez zmieniania języka?
Rozwiązanie: Musimy podzielić długi czas na pętle po 49 dni.
To świetne ćwiczenie na rekurencję i myślenie algorytmiczne.
Zaimplementujcie funkcję normalize:
```erlang
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
```
> **Tłumaczenie:** Jeśli mamy czekać 100 dni, a limit to 49, tworzymy listę [2, 49, 49] (kolejność nie ma znaczenia przy sumowaniu, ale technicznie czekamy kawałkami).

### Poprawiona pętla loop
Musimy obsłużyć listę czasów zamiast jednej liczby.
```erlang
loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        if
            Next =:= [] -> Server ! {done, S#state.name};
            true -> loop(S#state{to_go=Next})
        end
    end.
```
> Teraz nasz system jest robust (solidny).

### 2.4. Finalne funkcje startowe
Dodajemy start i init, które używają normalizacji.
```erlang
start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=normalize(Delay)}).
```

---
## CZĘŚĆ 3: Serwer i Zarządzanie Stanem (25 min)

### 3.1. Struktura danych
Serwer musi pamiętać stan. Nie używamy bazy danych, tylko pamięci procesu.
```erlang
-record(state, {events, clients}).
%% events: lista krotek {Name, Pid}
%% clients: lista Pid
```
### 3.2. Implementacja evserv.erl
```erlang
%%writefile evserv.erl
-module(evserv).
-compile(export_all).

-record(state, {events, clients}).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [#state{events=[], clients=[]}])).

loop(S = #state{events=Events, clients=Clients}) ->
    receive
        %% SUBSKRYPCJA
        {Pid, Ref, {subscribe, Client}} ->
            erlang:monitor(process, Client), %% [cite: 344] Monitorujemy klienta!
            NewClients = [Client | Clients],
            Pid ! {Ref, ok},
            loop(S#state{clients=NewClients});

        %% DODAWANIE
        {Pid, Ref, {add, Name, Description, TimeOut}} ->
            %% Tu można dodać walidację czasu (zadanie dodatkowe)
            EventPid = event:start_link(Name, TimeOut), %% [cite: 374] Linkujemy proces!
            NewEvents = [{Name, EventPid} | Events],
            Pid ! {Ref, ok},
            loop(S#state{events=NewEvents});

        %% ANULOWANIE
        {Pid, Ref, {cancel, Name}} ->
            NewEvents = case lists:keyfind(Name, 1, Events) of
                {Name, EventPid} ->
                    event:cancel(EventPid), %% [cite: 395] Wywołujemy funkcję cancel w module event
                    lists:keydelete(Name, 1, Events);
                false ->
                    Events
            end,
            Pid ! {Ref, ok},
            loop(S#state{events=NewEvents});

        %% DONE - Wiadomość od procesu event
        {done, Name} ->
            %% Wysyłamy info do wszystkich klientów
            lists:foreach(fun(C) -> C ! {done, Name} end, Clients),
            NewEvents = lists:keydelete(Name, 1, Events),
            loop(S#state{events=NewEvents});

        %% KLIENT PADŁ (Obsługa Monitora)
        {'DOWN', Ref, process, _Pid, _Reason} ->
             %% Usuwamy martwego klienta, żeby nie wysyłać w próżnię [cite: 432]
             loop(S#state{clients=Clients}); %% Uproszczenie: normalnie usuwamy po Ref

        shutdown ->
            exit(shutdown);
        
        code_change ->
            ?MODULE:loop(S); %% [cite: 425] Hot Code Swapping!

        Unknown ->
            io:format("Unknown: ~p~n", [Unknown]),
            loop(S)
    end.

```
### 3.3. Ukrywanie wiadomości (API)
Dlaczego to ważne? Bo jeśli zmienimy format wiadomości wewnętrznych, nie chcemy psuć kodu u wszystkich klientów.
Zadanie: Dopisz funkcje add_event/3 i subscribe/1 w module evserv, które ukrywają ! i receive8.

---
## CZĘŚĆ 4: Testowanie i Hot Code Swapping (15 min)

### 4.1. Test manualny w Notebooku
```erlang
c(event).
c(evserv).
evserv:start().
evserv:subscribe(self()).
evserv:add_event("Test", "Opis", 5).
%% Czekamy 5 sekund...
flush().
```
### 4.2. Hot Code Swapping (Wymiana silnika w locie)


---
## CZĘŚĆ 5: Supervision
System jest fajny, ale co jak serwer padnie? Potrzebujemy "Nadzorcy" (Supervisor).
### 5.1. Prosty Supervisor
Supervisor to proces, którego jedynym zadaniem jest pilnowanie, czy inny proces żyje. Jeśli tamten umrze, supervisor go wskrzesza.
```erlang
%%writefile sup.erl
-module(sup).
-export([start/2, init/1, loop/1]).

start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
    process_flag(trap_exit, true), %% Chcemy dostać info o śmierci dziecka
    loop({Mod, Args}).

loop({Mod, Args}) ->
    Pid = apply(Mod, start_link, Args), %% Startujemy dziecko
    receive
        {'EXIT', Pid, Reason} ->
            io:format("Proces ~p umarł z powodu ~p. Restartuję...~n", [Pid, Reason]),
            loop({Mod, Args}) %% Rekurencyjny restart
    end.
```
### 5.2. Symulacja awarii
-	Uruchom sup:start(evserv, []).
-	Zabij serwer komendą exit(whereis(evserv), kill)..
-	Zobacz, że Supervisor natychmiast uruchomił nową instancję.
---
## Zadania do samodzielnego wykonania
### Zadanie 1: Prosty Magazyn (Key-Value Store)
**Cel: Napisanie procesu serwera, który przechowuje stan (słownik/mapę) i obsługuje zapytania. To klasyka systemów rozproszonych.
Opis problemu: Potrzebujemy prostego serwera store, który działa jak pamięć podręczna. Można w nim zapisać wartość pod danym kluczem i ją odczytać.**
Wymagania:
-	Stwórz moduł store.
-	Proces ma przechowywać w swoim stanie mapę `(użyj #{} lub dict)`.
- Obsłuż komunikaty:
	- `{put, Key, Value}` – dodaje/nadpisuje element w mapie.
	- `{Pid, Ref, {get, Key}}` – odsyła do Pid wartość `{Ref, Value}` lub `{Ref, undefined}` (jeśli klucza nie ma).
-	Napisz funkcję `start/0`, która uruchamia proces.
Scenariusz testowy (w konsoli):**
```erlang
P = store:start().
P ! {put, rower, "niebieski"}.
P ! {put, auto, "czerwone"}.
P ! {self(), make_ref(), {get, rower}}.
flush(). % Powinno zwrócić "niebieski"
```
### Zadanie 2: "Gorący Ziemniak" (Process Ring)
**Cel: Praca z wieloma procesami i przekazywanie PID-ów (Topology).
Opis problemu: Stwórz grę, w której N procesów stoi w "kółku". Proces 1 wysyła wiadomość do Procesu 2, Proces 2 do 3, ..., a Ostatni z powrotem do Procesu 1. Wiadomość ma krążyć w kółko określoną liczbę razy.**
Wymagania:
-	Stwórz moduł ring.
-	Napisz funkcję `process_node(NextPid)`, która:
	- Czeka na wiadomość `{token, Round}`.
	- Jeśli `Round > 0`, wypisuje "`Proces [MójPID]` przekazuje dalej", zmniejsza `Round` o 1 i wysyła `{token, Round-1}` do `NextPid`.
	- Jeśli `Round == 0`, wypisuje "Koniec gry!" i kończy działanie.
-	Napisz funkcję `start_game()`, która:
	- Spawnuje 3 procesy. Musisz je połączyć tak, by P1 znał P2, P2 znał P3, a P3 znał P1.
	- Wrzuca "ziemniaka" (wiadomość `{token, 5}`) do pierwszego procesu.

**Podpowiedź: To zadanie wymaga sprytu przy spawnowaniu. Możesz spawnować procesy "od tyłu" (najpierw ostatni, potem środkowy, potem pierwszy) lub wysłać im PID sąsiada w osobnej wiadomości konfiguracyjnej ({set_neighbor, Pid}).**
### Zadanie 3: Load Balancer (Rozdzielacz Zadań)
**Cel: Architektura Zarządca-Robotnicy (Manager-Worker).
Opis problemu: Symulujemy firmę kurierską. Mamy jednego "Kierownika" (Load Balancer) i 3 "Kurierów" (Procesy Worker). Klient zleca zadanie Kierownikowi, a ten wybiera losowego kuriera do wykonania zadania.**
Wymagania:
-	Stwórz moduł lb (Load Balancer).
-	Kurier (Worker): Prosta funkcja pętli. Czeka na `{deliver, PackageName}`. Po otrzymaniu: usypia proces na 2 sekundy (`timer:sleep(2000)` - symulacja pracy), wypisuje "Dostarczono: PackageName" i czeka na kolejne zlecenia.
-	Kierownik (Manager):
	- Przy starcie spawnuje 3 procesy Kurierów i zapisuje ich PIDy w liście w swoim stanie.
	- Czeka na wiadomość `{job, PackageName}`.
	- Losuje jednego PID-a z listy (użyj `lists:nth` i `rand:uniform`) i przekazuje mu zadanie.
- Ukryj komunikację za funkcją `lb:send_package(Name)`.

**Scenariusz testowy:**
```erlang
lb:start().
lb:send_package("Paczka 1").
lb:send_package("Paczka 2").
lb:send_package("Paczka 3").
% Powinieneś zobaczyć w konsoli, że różne procesy (różne PIDy) wypisują komunikaty w losowej kolejności.
```
**Rozwiązania:**
### Zadanie 1:
**Kluczowe zagadnienia: Przechowywanie stanu (mapa), obsługa synchroniczna (request-reply).**
```erlang
-module(store).
-export([start/0, init/0, put/2, get/1]). %% API + funkcje wewnętrzne

%% --- API (Ukrywanie komunikatów) ---

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

%% --- Logika Procesu ---

init() ->
    %% Stanem początkowym jest pusta mapa
    loop(#{}).

loop(State) ->
    receive
        {put, Key, Value} ->
            %% maps:put zwraca NOWĄ mapę z dodanym elementem
            NewState = maps:put(Key, Value, State),
            loop(NewState);

        {Pid, Ref, {get, Key}} ->
            %% maps:get(Key, Map, Default) - zwraca Default jeśli klucza brak
            Value = maps:get(Key, State, undefined),
            Pid ! {Ref, Value},
            loop(State);

        stop ->
            ok
    end.
```
### Zadanie 2:
**Kluczowe zagadnienia: Topologia procesów. Wyzwanie: Jak zamknąć koło? Proces 1 musi znać Proces 2, P2 musi znać P3, a P3 musi znać P1. Rozwiązanie: Najpierw uruchamiamy procesy, a dopiero potem wysyłamy im wiadomość konfiguracyjną `{set_next, Pid}`.**
```erlang
-module(ring).
-export([start_game/0, node_init/0]).

start_game() ->
    %% 1. Spawnowanie procesów (na razie nie wiedzą o sobie)
    P1 = spawn(?MODULE, node_init, []),
    P2 = spawn(?MODULE, node_init, []),
    P3 = spawn(?MODULE, node_init, []),

    io:format("P1: ~p, P2: ~p, P3: ~p~n", [P1, P2, P3]),

    %% 2. Konfiguracja sąsiadów (zamykamy koło)
    P1 ! {set_next, P2},
    P2 ! {set_next, P3},
    P3 ! {set_next, P1},

    %% 3. Start gry - wrzucamy żeton do P1 (obiegnie koło 5 razy)
    P1 ! {token, 15}, %% 15 przeskoków = 5 pełnych okrążeń (dla 3 graczy)
    ok.

%% Faza inicjalizacji - czekamy na sąsiada
node_init() ->
    receive
        {set_next, NextPid} ->
            io:format("~p: Mam sąsiada ~p. Gotowy!~n", [self(), NextPid]),
            node_loop(NextPid)
    end.

%% Główna pętla gry
node_loop(NextPid) ->
    receive
        {token, 0} ->
            io:format("~p: Token padł u mnie. Koniec gry!~n", [self()]);
        
        {token, Round} ->
            io:format("~p: Dostałem (Round: ~p). Przekazuję do ~p~n", [self(), Round, NextPid]),
            timer:sleep(500), %% Żebyśmy zdążyli zobaczyć logi (opcjonalne)
            NextPid ! {token, Round - 1},
            node_loop(NextPid)
    end.
```
### Zadanie 3:
**Kluczowe zagadnienia: Architektura Manager-Worker, losowość (rand).**
```erlang
-module(lb).
-export([start/0, send_package/1, manager_init/0, worker_loop/1]).

%% --- API ---

start() ->
    %% Rejestrujemy managera pod nazwą 'manager', żeby łatwiej było słać do niego
    register(manager, spawn(?MODULE, manager_init, [])).

send_package(PackageName) ->
    manager ! {job, PackageName},
    ok.

%% --- Manager (Load Balancer) ---

manager_init() ->
    %% Manager uruchamia swoich pracowników
    W1 = spawn(?MODULE, worker_loop, [1]),
    W2 = spawn(?MODULE, worker_loop, [2]),
    W3 = spawn(?MODULE, worker_loop, [3]),
    Workers = [W1, W2, W3],
    io:format("Manager wystartował z pracownikami: ~p~n", [Workers]),
    manager_loop(Workers).

manager_loop(Workers) ->
    receive
        {job, PackageName} ->
            %% Losowanie pracownika
            Index = rand:uniform(length(Workers)), %% Losuje od 1 do 3
            SelectedWorker = lists:nth(Index, Workers),
            
            io:format("Manager: Zlecam '~s' do pracownika nr ~p~n", [PackageName, Index]),
            SelectedWorker ! {deliver, PackageName},
            manager_loop(Workers)
    end.

%% --- Worker (Kurier) ---

worker_loop(Id) ->
    receive
        {deliver, PackageName} ->
            io:format("Kurier ~p: Odbieram paczkę '~s'...~n", [Id, PackageName]),
            timer:sleep(2000), %% Symulacja ciężkiej pracy (2 sekundy)
            io:format("Kurier ~p: Dostarczono '~s'!~n", [Id, PackageName]),
            worker_loop(Id)
    end.
```
**Jak to testować?
Zalecam testowanie Load Balancera w taki sposób:**
```erlang
c(lb).
lb:start().
% Wysyłamy szybko 3 paczki - powinny trafić do różnych (losowych) kurierów
lb:send_package("Paczka A").
lb:send_package("Paczka B").
lb:send_package("Paczka C").
% Konsola powinna pokazać "Odbieram..." dla wszystkich od razu,
% a po 2 sekundach "Dostarczono..." dla wszystkich.
% To dowodzi, że pracują równolegle!
```

**Podsumowanie dla studentów (Wnioski)**
1.	Podział na procesy: Serwer zarządza logiką, Eventy zarządzają czasem. To pozwala na skalowanie i izolację błędów.
2.	Stan: Jest prywatny dla procesu.
3.	Protokół: Należy go zdefiniować przed kodowaniem.
4.	Limity: Nawet w potężnym Erlangu są limity (50 dni), które trzeba obejść inżyniersko.
5.	Hot Code Swap: Możemy naprawiać błędy bez zatrzymywania systemu.
