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
- Możliwość wielu klientów.
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
- **Serwer <-> Proces Wydarzenia:**
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

**Tworzymy szkic funkcji `loop/1`**
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
- Proces czeka na wiadomość cancel, która pozwala przerwać wydarzenie.
- Jeśli nie dostanie anulowania, po czasie Delay wykona kod w sekcji after, czyli zgłosi, że wydarzenie się zakończyło.
- Wszystko to dzieje się wewnątrz jednego lightweight procesu Erlanga.

**Potrzebujemy stanu — tworzymy rekord `state`:**  
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

**Uzupełniamy pętlę `loop/1`**
```erlang
loop(S = #state{server=Server}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after S#state.to_go * 1000 ->
            Server ! {done, S#state.name}
    end.
```
- `#state{server=Server}` - pobiera PID serwera, żeby móc wysyłać mu odpowiedź
- `S#state.to_go * 1000` – zamieniamy sekundy na milisekundy.
- Powiadomienie `done` – po upływie czasu wysyłamy do serwera komunikat: `{done, Name}`
To informuje go, że wydarzenie się zakończyło.

Test:
```erlang
1> c(event).
{ok,event}
2> rr(event, state).
[record definitions loaded from event]
3> Pid = spawn(event, loop, [#state{server=self(), name="test", to_go=5}]).
<0.95.0>
4> flush().
Shell got {done,"test"}
ok
```
### **2.3 Problem ~49dni**

W Erlangu limit czasu w `receive … after` wynosi około 49 dni (w milisekundach).
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

---

## CZĘŚĆ 3: Moduł Server

**Format wiadomości `{Pid, Ref, Message}`**  
Serwer dogaduje się z klientami zawsze w tym samym formacie:
```erlang
{Pid, MsgRef, {subscribe, Client}}
{Pid, MsgRef, {add, Name, Description, TimeOut}}
{Pid, MsgRef, {cancel, Name}}
```
- `Pid` – kto wysłał (klient / inny proces).
- `MsgRef` – unikalna referencja odpowiedzi (klient wie, której odpowiedzi dotyczy).
- `{...}` – właściwa treść polecenia (subscribe/add/cancel).

**Rekord state – stan całego serwera**
```erlang
-record(state, {
    events,   %% lista zdarzeń
    clients   %% lista klientów
}).
```
Ten rekord przechowuje:
- `events` – informacje o aktywnych zdarzeniach
- `clients` – PID-y klientów, którzy chcą dostawać powiadomienia
	
**Rekord event – opis jednego wydarzenia**
```erlang
-record(event, {
    name = "",
    description = "",
    pid,
    timeout = {{1970,1,1},{0,0,0}}
}).
```
Dla każdego zdarzenia serwer pamięta:
- `name` – identyfikator,
- `description` – treść powiadomienia,
- `pid` – PID procesu timera (`event.erl`),
- `timeout` – kiedy ma się wykonać.  
Dzięki temu:
- po nazwie możemy znaleźć PID procesu i np. je anulować,
- po done, Name możemy dobrać opis z rekordu i wysłać go klientom.

**Pętla serwera z wczytanym stanem**
```erlang
loop(S = #state{}) ->
    receive
        ...
    end.
```
- `S` to aktualny stan (`events`, `clients`),
- w każdej gałęzi `receive` modyfikujemy stan (np. dodajemy event),
- i wywołujemy `loop/1` z nową wersją stanu.

Czyli: brak zmiennych globalnych, brak mutacji “w miejscu” – tylko przekazywanie nowego `S` dalej.
To jest esencja bezpiecznej współbieżności w Erlangu.

**Funkcja `init/0` — start serwera**
```erlang
init() ->
    loop(#state{
        events = orddict:new(),
        clients = orddict:new()
    }).
```
Przy starcie serwer:  
- Tworzy początkowy stan serwera:
	- events – pusta lista wydarzeń,
	- clients – pusta lista klientów.
- Uruchamia główną pętlę serwera (loop/1) z tym stanem.
- Dzięki temu serwer startuje zawsze w czystym, zdefiniowanym środowisku.

**Obsługa subskrypcji `{subscribe, Client}`**
```erlang
{Pid, MsgRef, {subscribe, Client}} ->
    Ref = erlang:monitor(process, Client),
    NewClients = orddict:store(Ref, Client, S#state.clients),
    Pid ! {MsgRef, ok},
    loop(S#state{clients=NewClients});
```
- serwer monitoruje klienta, aby wiedzieć, kiedy umrze,
- zapisuje go do słownika klientów (kluczem jest referencja monitora),
- odpowiada `{ok}`,
- przechodzi do kolejnej iteracji pętli.

**Walidacja daty i czasu (`valid_datetime/1`)**  
Zdarzenia są zgłaszane jako:  
`{{Year, Month, Day}, {Hour, Minute, Second}}`

**Dodawanie zdarzenia `{add, Name, Description, Timeout}`**
```erlang
{Pid, MsgRef, {add, Name, Description, TimeOut}} ->
    case valid_datetime(TimeOut) of
        true ->
            EventPid = event:start_link(Name, TimeOut),
            NewEvents = orddict:store(
                Name,
                #event{name=Name, description=Description,
                       pid=EventPid, timeout=TimeOut},
                S#state.events),
            Pid ! {MsgRef, ok},
            loop(S#state{events=NewEvents});
        false ->
            Pid ! {MsgRef, {error, bad_timeout}},
            loop(S)
    end;
```
- Serwer sprawdza, czy czas jest poprawny (`valid_datetime`).
- Jeśli tak → tworzy nowy proces zdarzenia (`event:start_link`).
- Dodaje rekord zdarzenia do stanu (`events`).
- Odpowiada klientowi `{ok}`.  
Jeśli czas niepoprawny:
- serwer nie tworzy procesu,
- wysyła `{error, bad_timeout}`.  
To chroni system przed „martwymi zdarzeniami”.

**Anulowanie zdarzenia `{cancel, Name}`**
```erlang
{Pid, MsgRef, {cancel, Name}} ->
    Events = case orddict:find(Name, S#state.events) of
        {ok, E} ->
            event:cancel(E#event.pid),
            orddict:erase(Name, S#state.events);
        error ->
            S#state.events
    end,
    Pid ! {MsgRef, ok},
    loop(S#state{events=Events});
```
Mechanizm:
- serwer sprawdza, czy zdarzenie istnieje,
- jeśli tak:
	- wywołuje `event:cancel/1`, aby zatrzymać proces timera,
	- usuwa zdarzenie z listy,
- jeśli nie, po prostu odpowiada `{ok}` (bo „i tak już nie istnieje”).
	
**Obsługa powiadomień od zdarzeń `{done, Name}`**
```erlang
{done, Name} ->
    case orddict:find(Name, S#state.events) of
        {ok, E} ->
            send_to_clients(
                {done, E#event.name, E#event.description},
                S#state.clients
            ),
            NewEvents = orddict:erase(Name, S#state.events),
            loop(S#state{events=NewEvents});
        error ->
            loop(S)
    end;
```
Proces zdarzenia (timer) wysyła {done, Name}.
Serwer:
- wyszukuje rekord zdarzenia,
- wysyła powiadomienie do każdego zasubskrybowanego klienta:
```erlang
{done, Name, Description}
```

**Komunikaty systemowe**
```erlang
shutdown ->
    exit(shutdown);
```
Proces serwera kończy pracę.

**`DOWN` – klient umarł**
```erlang
{'DOWN', Ref, process, _Pid, _Reason} ->
    loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
```
Serwer usuwa klienta z listy subskrypcji —
nie będzie wysyłał powiadomień do martwego procesu.

---

## Część 4: Hot Code Swapping
Erlang posiada unikatową funkcję, której nie mają typowe języki:
możliwość wymiany kodu działającej aplikacji bez zatrzymywania procesu.
To kluczowe w systemach telekomunikacyjnych, bankowych i wszędzie tam, gdzie aplikacja ma działać 24/7.

### Dwie wersje modułu naraz
Code server może przechowywać:
- starą wersję modułu (old)
- nową wersję (new)
Nowa wersja pojawia się po kompilacji:
```erlang
c(Module).
```
Procesy mogą nadal działać na starej wersji, dopóki same nie przełączą się na nową.

### Przełączenie procesu na nowy kod
Każdy proces ma swoją pętlę:
```erlang
loop(State) ->
    ...
    loop(NewState).
```
Aby wejść w nową wersję modułu, wywołujemy:
```erlang
?MODULE:loop(State).
```
To jest zewnętrzne wywołanie → proces natychmiast zaczyna działać w nowym kodzie, ale z tym samym stanem.

**Uwaga: 3-cia wersja modułu = zabity proces**
VM trzyma max 2 wersje:
- old
- new  

Jeśli pojawi się **trzecia**, procesy nadal działające na najstarszej wersji są ubijane, bo VM zakłada, że nie potrafią się zaktualizować.

### Przykład pełnego upgrade’u
```erlang
-module(hotload).
-export([server/1, upgrade/1]).

server(State) ->
    receive
        update ->
            NewState = ?MODULE:upgrade(State),
            ?MODULE:server(NewState);    %% przejście do nowego kodu
        SomeMessage ->
            server(State)
    end.

upgrade(OldState) ->
    OldState.   %% ewentualna transformacja stanu
```
Działanie:
- po `update` proces wywołuje `?MODULE:server`, czyli wskakuje w nową wersję modułu,
- stan pozostaje (również może zostać przekształcony w `upgrade/1`),
- nie trzeba restartować systemu.

---

## Część 5: Supervisor
W aplikacjach współbieżnych w Erlangu procesy mogą umierać w każdej chwili – to normalne i akceptowane zachowanie.
Dlatego potrzebujemy mechanizmu, który pilnuje innych procesów i automatycznie je restartuje, jeśli padną.

**Supervisor:**
- uruchamia proces, który ma nadzorować (np. nasz event server),
- monitoruje go,
- jeśli proces padnie z dowolnego powodu → restartuje go automatycznie,
- jeśli supervisor dostanie sygnał shutdown → kończy pracę i zabija swoje dziecko.
To przenosi odpowiedzialność za stabilność aplikacji z procesu na superwizora.

**Funkcje `start/2` i `start_link/2` uruchamiają proces supervisora:**
```erlang
-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
    spawn_link(?MODULE, init, [{Mod, Args}]).
```
**init/1**
Supervisor ustawia `trap_exit`, żeby móc przechwytywać zakończenia procesów:
```erlang
init({Mod, Args}) ->
    process_flag(trap_exit, true),
    loop({Mod, start_link, Args}).
```
**`loop/1` – serce supervisora**
```erlang
loop({M,F,A}) ->
    Pid = apply(M,F,A),
    receive
        {'EXIT', Pid, shutdown} ->
            exit(shutdown);   % supervisor też umiera
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
            loop({M,F,A})     % restart dziecka!
    end.
```
- Supervisor uruchamia proces dziecka.
- Jeśli dziecko umiera:
	- Reason = shutdown → supervisor kończy pracę (to celowe).
	- Reason ≠ shutdown → drukuje powód i restartuje proces.

**Przykład działania:**
1. Kompilacja modułów:
```erlang
1> c(eserv), c(sup).
{ok,eserv}
{ok,sup}
```
2. Uruchamiamy supervisora
```erlang
2> SupPid = sup:start(eserv, []).
<0.43.0>
```
- Supervisor startuje i od razu uruchamia proces `eserv` jako swoje dziecko.
- Zwracany jest PID supervisora — zapisujemy go w zmiennej `SupPid`.
- Supervisor ustawia `trap_exit`, więc będzie odbierał komunikaty `'EXIT', ....`
- 
3. Sprawdzamy, czy event server działa
```erlang
3> whereis(eserv).
<0.44.0>
```
- `eserv` został poprawnie uruchomiony przez supervisora.
- Widzimy jego PID `(<0.44.0>)`.
4. Zabijamy event server
```erlang
4> exit(whereis(eserv), die).
true
```
- Wysyłamy procesowi eserv sygnał zakończenia z powodem die.
- Event server umiera NATYCHMIAST.
- Supervisor dostaje komunikat:
```erlang
{'EXIT', <0.44.0>, die}
```
Po czym uruchamia nową kopię event servera

5. Sprawdzamy, czy żyje nowy event server
```erlang
5> whereis(eserv).
<0.48.0>
```
6. Zabijamy supervisora
```erlang
6> exit(SupPid, shutdown).
true
```
Kiedy supervisor umiera - zabija swoje dziecko.

7. Sprawdzamy, czy dziecko również zniknęło
```erlang
7> whereis(eserv).
undefined
```
Event server nie istnieje.
Supervisor przestał działać, więc jego dziecko również.

---

## Zadania do samodzielnego wykonania

### **ZADANIE 1 – Prosty aktor (proces) odbierający wiadomości**
**Polecenie:**
1. Napisz funkcję `loop/0`, która:
	- wypisuje na ekran otrzymaną wiadomość,
	- wraca do oczekiwania w pętli.
2. Utwórz proces za pomocą `spawn`.
3. Wyślij do niego kilka różnych wiadomości.

### **ZADANIE 2 – Proces z timeoutem**
**Polecenie:**
1. Napisz proces, który:
	- czeka 20 sekund na wiadomość `{ping, Pid}`,
	- jeśli ją dostanie → odsyła `{pong}`,
	- jeśli *nie* dostanie → wypisuje "`timeout`" i kończy działanie.
2. Uruchom proces i przetestuj oba przypadki:
	- wyślij do niego wiadomość na czas,
	- oraz nie wysyłaj nic, aby wywołać timeout.

### **ZADANIE 3 – Mini-serwer zarządzający stanem**
**Polecenie:**
1. Napisz proces-serwer (pętla) przechowujący listę liczb.
2. Serwer ma obsługiwać wiadomości:
	- `{add, Value}` — dodaje liczbę do listy,
	- `get` — odsyła aktualną listę nadawcy.
3. Napisz funkcję pomocniczą `rpc(Pid, Msg)`, która:
	- wysyła wiadomość `{self(), Msg}`,
	- robi `receive` i zwraca odpowiedź,
	- tak by komunikacja wyglądała jak „zdalne wywołanie”.
4. Przetestuj:
	- tworzenie serwera,
	- dodawanie 3 wartości,
	- pobranie całej listy.

### **ZADANIE 4 – Proces-timer (prosty „event”)**
**Polecenie:**
Napisz funkcję, która:
1. Tworzy nowy proces-timer tak:
	- czeka określoną liczbę sekund,
	- po czasie odsyła do nadawcy wiadomość `{done, Name}`.
2. Timer ma działać niezależnie od reszty notebooka.
3. Przetestuj:
	- utworzenie dwóch timerów o różnych czasach,
	- sprawdzenie, że oba zwracają `done` we właściwej kolejności,
	- pokazanie, że kolejne komórki w notebooku nadal działają (studencka obserwacja współbieżności).

## **Rozwiązania zadań**

### Zadanie 1:
```erlang
-module(ex1_actor).
-export([start/0, loop/0]).

%% Startuje nowy proces-aktora
start() ->
    spawn(?MODULE, loop, []).

%% Pętla procesu: wypisuje każdą otrzymaną wiadomość
loop() ->
    receive
        Msg ->
            io:format("received: ~p~n", [Msg]),
            loop()
    end.
```
Test:
```erlang
%% === TEST ZADANIA 1: ex1_actor ===

%% 1> Kompilacja modułu
1> c(ex1_actor).
%% Oczekiwane:
{ok,ex1_actor}

%% 2> Startujemy proces aktora
2> Pid = ex1_actor:start().
%% Oczekiwane (PID będzie różny):
<0.75.0>

%% 3> Wysyłamy pierwszą wiadomość
3> Pid ! hello.
%% Oczekiwane na ekranie:
received: hello
%% Oczekiwany wynik zwrócony przez komórkę:
hello

%% 4> Wysyłamy drugą wiadomość
4> Pid ! 123.
%% Oczekiwane na ekranie:
received: 123
%% Oczekiwany wynik:
123

%% 5> Wysyłamy trzecią wiadomość (krotkę)
5> Pid ! {test, 42}.
%% Oczekiwane na ekranie:
received: {test,42}
%% Oczekiwany wynik:
{test,42}

%% Proces nadal działa i czeka na kolejne wiadomości
```

### Zadanie 2:
```erlang
-module(ex2_timeout).
-export([start/0, loop/0]).

%% Startuje proces z timeoutem
start() ->
    spawn(?MODULE, loop, []).

%% Czeka na {ping, From}; jeśli nie dostanie w 5s -> "timeout"
loop() ->
    receive
        {ping, From} ->
            From ! pong
    after 20000 ->
            io:format("timeout~n")
    end.
```
Testy:
```erlang
%% === TEST ZADANIA 2: ex2_timeout ===

%% --- Test 1: Wiadomość przychodzi na czas ---

%% 1> Kompilacja modułu
1> c(ex2_timeout).
%% Wynik:
{ok,ex2_timeout}

%% 2> Start procesu z timeoutem
2> Pid = ex2_timeout:start().
%% Wynik (PID będzie różny):
<0.80.0>

%% 3> Wysyłamy wiadomość ping na czas
3> Pid ! {ping, self()}.
%% Wynik wyrażenia:
{ping,<0.xx.0>}

%% 4> Odbieramy odpowiedź od procesu
4> receive Msg -> Msg end.
%% Wynik:
pong

%% (W tym teście NIE ma timeoutu, bo proces odpowiedział przed 5 sekundami)

%% --- Test 2: Brak wiadomości → timeout ---

%% 5> Startujemy drugi proces
5> Pid2 = ex2_timeout:start().
%% Wynik:
<0.81.0>

%% 6> NIE wysyłamy do Pid2 żadnej wiadomości.
%%    Proces sam wyświetli po ~5 sekundach:
timeout

%% (Po wyświetleniu "timeout" proces kończy działanie)
```

### Zadanie 3:
```erlang
-module(ex3_kv).
-export([start/0, put/2, get/1, loop/1]).

%% Startuje serwer i rejestruje go pod nazwą ex3_kv
start() ->
    Pid = spawn(?MODULE, loop, [[]]),
    register(ex3_kv, Pid),
    Pid.

%% Główna pętla serwera, State = lista liczb
loop(State) ->
    receive
        {From, {add, V}} ->
            NewState = [V | State],
            From ! ok,
            loop(NewState);

        {From, get} ->
            From ! State,
            loop(State)
    end.

%% API: dodaje liczbę do serwera
put(Name, V) ->
    ex3_kv ! {self(), {add, V}},
    receive
        Reply -> Reply
    end.

%% API: pobiera aktualną listę
get(Name) ->
    ex3_kv ! {self(), get},
    receive
        Reply -> Reply
    end.
```
Testy:
```erlang
%% === TEST ZADANIA 3: ex3_kv ===

%% 1> Kompilacja modułu serwera klucz-wartość
1> c(ex3_kv).
%% Wynik:
{ok,ex3_kv}

%% 2> Start procesu serwera
2> ex3_kv:start().
%% Wynik (PID będzie różny):
<0.90.0>
%% (Dodatkowo proces został zarejestrowany pod atomem ex3_kv)



%% --- Dodawanie wartości do serwera ---

%% 3> Dodajemy pierwszą wartość
3> ex3_kv:put(list, 10).
%% Wynik:
ok

%% 4> Dodajemy drugą wartość
4> ex3_kv:put(list, 20).
%% Wynik:
ok

%% 5> Dodajemy trzecią wartość
5> ex3_kv:put(list, 30).
%% Wynik:
ok



%% --- Pobranie stanu z serwera ---

%% 6> Pobieramy aktualną listę
6> ex3_kv:get(list).
%% Wynik:
[30,20,10]
%% (Kolejność jest odwrotna, bo każdą wartość dodajemy na początek listy)
```

### Zadanie 4:
```erlang
-module(ex4_timer).
-export([start_timer/3]).

%% Startuje timer:
%% Name    - nazwa zdarzenia (np. "A")
%% Seconds - ile sekund czekać
%% Receiver - PID procesu, który ma dostać {done, Name}
start_timer(Name, Seconds, Receiver) ->
    spawn(fun() ->
        receive
        after Seconds * 1000 ->
            Receiver ! {done, Name}
        end
    end).
```
Testy:
```erlang
%% === TEST ZADANIA 4: ex4_timer ===

%% 1> Kompilacja modułu timera
1> c(ex4_timer).
%% Wynik:
{ok,ex4_timer}

%% 2> Pobieramy PID bieżącego procesu (shella)
2> Self = self().
%% Wynik (PID będzie różny):
<0.100.0>

%% --- Uruchamiamy timery równolegle ---

%% 3> Start timera "A" ustawionego na 20 sekundy
3> ex4_timer:start_timer("A", 20, Self).
%% Wynik (PID procesu timera będzie różny):
<0.101.0>

%% 4> Start timera "B" ustawionego na 30 sekund
4> ex4_timer:start_timer("B", 30, Self).
%% Wynik:
<0.102.0>

%% --- Odbieranie wyników z timerów ---

%% 5> Odbieramy pierwszą wiadomość
5> receive Msg1 -> Msg1 end.
%% Wynik po ok. 20 sekundach:
{done,"A"}

%% 6> Odbieramy drugą wiadomość
6> receive Msg2 -> Msg2 end.
%% Wynik po ok. 30 sekundach od startu:
{done,"B"}

%% Timery działają równolegle — krótszy ("A") kończy się pierwszy,
%% dłuższy ("B") kończy się później.
```
