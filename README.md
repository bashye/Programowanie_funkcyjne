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
