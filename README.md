# Erlang: Projektowanie Aplikacji Współbieżnych

## CZĘŚĆ 1: Teoria i Projektowanie

### 1.1. Definicja problemu

Chcemy napisać system przypomnień (Reminder App).

#### Wymagania:
- Dodawanie wydarzeń:
	- `nazwa`
	- `opis`
	- `czas` (kiedy wywołać przypomnienie)
- Gdy czas nadejdzie → system wysyła powiadomienie.
- Możliwość anulowania wydarzenia po nazwie.
- Możliwość wielu klientów (CLI teraz, w przyszłości GUI/WWW/IM/etc.).
- System powinien umożliwiać:
	- monitorowanie serwera,
	- bezpieczne wyłączanie serwera,
	- aktualizację kodu podczas działania (hot code reload).

### 1.2. Architektura w Erlang

W Erlangu system dzielimy na niezależne procesy, które nie dzielą pamięci i komunikują się jedynie wiadomościami.

1. Klient (Client)
		- wysyła żądania: `add`, `cancel`, `subscribe`, `shutdown`,
		- odbiera powiadomienia o zakończonych wydarzeniach,
		- może monitorować serwer (wykryć awarię).
Każdy klient to osobny proces.

2. Serwer Wydarzeń (Event Server)
	- Centralny proces zarządzający systemem.
	- przyjmuje subskrypcje klientów,
	- uruchamia procesy wydarzeń,
	- przekazuje powiadomienia do klientów,
	- obsługuje anulowanie i zamknięcie systemu,
	- może mieć przeładowany kod w trakcie działania.
Serwer nie odlicza czasu — to zadanie procesów wydarzeń.

3. Procesy Wydarzeń (Event Processes)
Każde wydarzenie działa jako osobny, lekki proces.
Zachowanie:
	- czeka określony czas (`receive after`),
	- wysyła do serwera: `{done, Id}`,
	- kończy działanie,
	- jeśli dostanie `cancel`, kończy się natychmiast.
Dzięki temu awaria pojedynczego wydarzenia nie wpływa na resztę systemu.

### 1.3. Projektowanie Protokołu

- **Klient -> Serwer:**
	- `{subscribe, Pid}` - Zapisz mnie jako odbiorcę powiadomień
	- `{add, Name, Description, TimeOut}` - Utwórz nowe wydarzenie
	- `{cancel, Name}` - Anuluj wydarzenie
- **Serwer -> Klient:**
	- `{Ref, ok}` lub `{error, Reason}` - Potwierdzenie wykonania operacji lub błąd
	- `{done, Name, Description}` Powiadomienie, że wydarzenie się wykonało
- **Serwer <-> Proces Wydarzenia (Wewnętrzne):**
	- Serwer -> Proces: `cancel` - Proces zdarzenia ma się zakończyć
	- Proces -> Serwer: `{done, Id}` - Czas minął, powiadom klientów

---

## CZĘŚĆ 2: Fundamenty i Moduł Event

### 2.1. Przygotowanie projektu
Na początku tworzymy standardową strukturę katalogów używaną w projektach Erlang:
```css
ebin/
include/
priv/
src/
```
- `src/` – tutaj umieszczamy cały kod `.erl`.
- `ebin/` – tu trafią skompilowane pliki `.beam`.
- `include/` – nagłówki `.hrl`.
- `priv/` – pliki dodatkowe.

### 2.2. Tworzymy moduł zdarzeń (event.erl)

Moduł zdarzeń odpowiada za działanie pojedynczego timera.

####**Tworzymy szkic funkcji `loop/1`**
Najpierw definiujemy **pętlę procesu**, która będzie wykonywać się w każdym „timerze” wydarzenia:

```erlang
loop(State) ->
    receive
        {Server, Ref, cancel} ->
            ...
    after Delay ->
            ...
    end.
```
Oznacz to, że:
	- Proces czeka na wiadomość cancel, która pozwala przerwać wydarzenie.
	- Jeśli nie dostanie anulowania, po czasie Delay wykona kod w sekcji after, czyli zgłosi, że wydarzenie się zakończyło.
	- Wszystko to dzieje się wewnątrz jednego lightweight procesu Erlanga.

####**Potrzebujemy stanu — tworzymy rekord `state`**
Aby ten proces wiedział:
	- ile czasu ma czekać,
	- jak nazywa się wydarzenie,
	- do kogo ma wysłać powiadomienie,
```erlang
-module(event).
-compile(export_all).
-record(state, {
    server,
    name = "",
    to_go = 0
}).
```

####**Uzupełniamy pętlę loop/1**
```erlang
loop(S = #state{server=Server}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after S#state.to_go * 1000 ->
            Server ! {done, S#state.name}
    end.
```
Dodaliśmy:
	- `#state{server=Server}` - pobiera PID serwera, żeby móc wysyłać mu odpowiedź
	- `S#state.to_go * 1000` – zamieniamy sekundy na milisekundy.
	- Powiadomienie `done` – po upływie czasu wysyłamy do serwera komunikat:
		`{done, Name}`
To informuje go, że wydarzenie się zakończyło.

Test:
```erlang
c(event).
rr(event, state).

Pid = spawn(event, loop, [#state{server=self(), name="test", to_go=5}]).
flush().
```
####**Problem ~49dni**

W Erlangu limit czasu w receive … after wynosi około 49 dni (w milisekundach).
Jeśli chcemy ustawić timer np. na rok, otrzymamy błąd:
```
timeout_value error
```
To dlatego, że Erlang nie akceptuje zbyt dużej liczby milisekund naraz.

**Dzielenie czasu na mniejsze części**
Zamiast czekać na cały czas jednorazowo, dzielimy timeout na porcje po 49 dni.
Do tego służy funkcja `normalize/1`:
```erlang
normalize(N) ->
    Limit = 49*24*60*60,     % sekundy
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
```
- Limit = maksymalny czas jednego odliczania (49 dni).
- Jeśli czas jest większy:
	- dzielimy go na części po 49 dni,
	- ostatnia część to reszta.
- Wynikiem jest lista timeoutów, np.:
```css
98 dni → [49, 49]
100 dni → [49, 49, 2]
```
**Zmiana pętli `loop/1`: obsługa listy timeoutów**
```erlang
loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T*1000 ->
        case Next of
            [] ->
                Server ! {done, S#state.name};
            _ ->
                loop(S#state{to_go=Next})
        end
    end.
```

##CZĘŚĆ 3: Moduł Server

###3.1: Format wiadomości {Pid, Ref, Message}
Serwer dogaduje się z klientami zawsze w tym samym formacie:
```erlang
{Pid, MsgRef, {subscribe, Client}}
{Pid, MsgRef, {add, Name, Description, TimeOut}}
{Pid, MsgRef, {cancel, Name}}
```
- `Pid` – kto wysłał (klient / inny proces).
- `MsgRef` – unikalna referencja odpowiedzi (klient wie, której odpowiedzi dotyczy).
- `{...}` – właściwa treść polecenia (subscribe/add/cancel).

###3.2: Rekord state – stan całego serwera
```erlang
-record(state, {
    events,   %% lista zdarzeń
    clients   %% lista klientów
}).
```
Ten rekord przechowuje:
	- events – informacje o aktywnych zdarzeniach
	- clients – PID-y klientów, którzy chcą dostawać powiadomienia
	
###3.3: Rekord event – opis jednego wydarzenia
```erlang
-record(event, {
    name = "",
    description = "",
    pid,
    timeout = {{1970,1,1},{0,0,0}}
}).
```
Dla każdego zdarzenia serwer pamięta:
	- `name – identyfikator,
	- `description` – treść powiadomienia,
	- `pid` – PID procesu timera (`event.erl),
	- `timeout` – kiedy ma się wykonać.
Dzięki temu:
	- po nazwie możemy znaleźć PID procesu i np. je anulować,
	- po done, Name możemy dobrać opis z rekordu i wysłać go klientom.
---
## CZĘŚĆ 4: Testowanie

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
---
## CZĘŚĆ 5: Supervision
System jest dobry, ale co jak serwer padnie? Potrzebujemy "Nadzorcy" (Supervisor).
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
**Jak to testować?**

**Zalecam testowanie Load Balancera w taki sposób:**
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
