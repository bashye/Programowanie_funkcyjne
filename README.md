Erlang - Lekcja 2: Projektowanie Aplikacji Współbieżnych
========================================================

2025y. \[Twoje Imię\], \[Twoja Uczelnia\]

Cel zajęć
---------

Zrozumienie, jak zaprojektować, podzielić i zaimplementować system oparty na wielu procesach w Erlangu. Stworzymy aplikację **Event Reminder** (System Przypomnień), opierając się na architekturze OTP.

1\. Teoria: Jak myśleć procesami?
---------------------------------

W programowaniu obiektowym (OOP) modelujemy system za pomocą klas i obiektów. W Erlangu modelujemy go za pomocą **Procesów** i **Protokołów Komunikacji**.

### A. Dekompozycja Problemu (Graf Procesów)

Zanim napiszemy kod, musimy narysować strukturę. Nasz system przypomnień nie może być jedną pętlą while, ponieważ musi robić kilka rzeczy naraz (czekać na czas, słuchać poleceń anulowania, dodawać nowe zadania).

Dzielimy problem na mniejsze, niezależne części (Procesy) :

1.  **Client (Klient)**: Interfejs użytkownika (my w konsoli). Wysyła polecenia.
    
2.  **Event Server (Mózg operacji)**:
    
    *   Przyjmuje subskrypcje od klientów.
        
    *   Zarządza listą aktywnych wydarzeń.
        
    *   Jest jedynym punktem kontaktu dla Klienta.
        
3.  **Event Processes (X, Y, Z - Robotnicy)**:
    
    *   Pojedynczy proces = Pojedyncze przypomnienie.
        
    *   Jego jedyne zadanie to: czekać X czasu, a potem wysłać wiadomość "Już!".
        
    *   Jeśli jeden proces (X) ulegnie awarii, reszta systemu działa dalej (**Izolacja błędów**).
        

### B. Protokół i Kolejki (Mailboxes)

Każdy proces w Erlangu posiada **skrzynkę pocztową (Mailbox)**, która działa jak kolejka FIFO (First In, First Out).

*   **Wysyłanie (Pid ! Msg)**: Wrzuca wiadomość do skrzynki odbiorcy.
    
*   **Odbieranie (receive)**: Wyciąga wiadomość z kolejki.
    

Musimy zdefiniować **Protokół** – czyli słownik wiadomości, jakie procesy mogą do siebie wysyłać .

**Nasz Protokół:**

*   {subscribe, Self} – Klient chce otrzymywać powiadomienia.
    
*   {add, Name, Desc, Time} – Klient dodaje wydarzenie.
    
*   {cancel, Name} – Klient anuluje wydarzenie.
    
*   {done, Name} – Proces Event informuje Serwer, że czas minął.
    

2\. Implementacja Krok po Kroku
-------------------------------

### Krok 1: Pojedyncze Wydarzenie (event.erl)

Zaczynamy od najmniejszego elementu – procesu, który odlicza czas.

**Logika:**Proces ma pętlę (loop), która czeka na wiadomość cancel. Jeśli jej nie dostanie przez określony czas (after), uznajemy, że czas minął .

Erlang

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -module(event).  -compile(export_all).  -record(state, {server, name, to_go}).  %% Funkcja startująca proces i inicjalizująca go  start(EventName, Delay) ->      spawn(?MODULE, init, [self(), EventName, Delay]).  start_link(EventName, Delay) ->      spawn_link(?MODULE, init, [self(), EventName, Delay]).  init(Server, EventName, Delay) ->      loop(#state{server=Server, name=EventName, to_go=Delay}).  %% Pętla główna procesu  loop(S = #state{server=Server, to_go=ToGo}) ->      receive          {Server, Ref, cancel} ->              Server ! {Ref, ok}      after ToGo * 1000 -> %% Timeout w milisekundach          Server ! {done, S#state.name}      end.   `

> **Wskazówka:** Używamy rekordu #state, aby przekazywać dane między wywołaniami pętli, choć tutaj pętla kończy się po jednym przebiegu (chyba że implementujemy normalizację czasu dla bardzo długich okresów) .

### Krok 2: Interfejs (Ukrywanie Wiadomości)

Nie chcemy, aby użytkownik musiał ręcznie wpisywać Pid ! {self(), Ref, cancel}. Tworzymy funkcję pomocniczą :

Erlang

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   cancel(Pid) ->      %% Monitorujemy proces, na wypadek gdyby już nie istniał (zdechł wcześniej)      Ref = erlang:monitor(process, Pid),      Pid ! {self(), Ref, cancel},      receive          {Ref, ok} ->              erlang:demonitor(Ref, [flush]),              ok;          {'DOWN', Ref, process, Pid, _Reason} ->              ok      end.   `

### Krok 3: Serwer Wydarzeń (evserv.erl)

To centrum dowodzenia. Serwer musi żyć wiecznie (pętla rekurencyjna) i przechowywać stan: listę klientów i listę wydarzeń.

**Szkielet pętli serwera:**

Erlang

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -module(evserv).  -compile(export_all).  -record(state, {events, clients}). %% events = lista wydarzeń, clients = lista subskrybentów  -record(event, {name, description, pid, timeout}).  loop(S = #state{events=Events, clients=Clients}) ->      receive          %% 1. Subskrypcja klienta          {Pid, MsgRef, {subscribe, Client}} ->              Ref = erlang:monitor(process, Client), %% Monitorujemy klienta!              NewClients = orddict:store(Ref, Client, Clients),              Pid ! {MsgRef, ok},              loop(S#state{clients=NewClients});          %% 2. Dodawanie wydarzenia          {Pid, MsgRef, {add, Name, Description, TimeOut}} ->              EventPid = event:start_link(Name, TimeOut), %% Spawnujemy proces timera              NewEvents = orddict:store(Name,                                        #event{name=Name, description=Description, pid=EventPid, timeout=TimeOut},                                        Events),              Pid ! {MsgRef, ok},              loop(S#state{events=NewEvents});          %% 3. Anulowanie wydarzenia          {Pid, MsgRef, {cancel, Name}} ->              Events2 = case orddict:find(Name, Events) of                  {ok, E} ->                      event:cancel(E#event.pid), %% Zabijamy proces timera                      orddict:erase(Name, Events);                  error ->                      Events              end,              Pid ! {MsgRef, ok},              loop(S#state{events=Events2});          %% 4. Obsługa gotowego wydarzenia (Message from Event Process)          {done, Name} ->              case orddict:find(Name, Events) of                  {ok, E} ->                      send_to_clients({done, E#event.name, E#event.description}, Clients),                      NewEvents = orddict:erase(Name, Events),                      loop(S#state{events=NewEvents});                  error ->                      loop(S)              end;          %% 5. Hot Code Swapping (Wymiana kodu w locie)          code_change ->              ?MODULE:loop(S); %% External call ładuje nową wersję modułu!          shutdown ->              exit(shutdown)      end.  %% Pomocnicza funkcja inicjująca  init() ->      loop(#state{events=orddict:new(), clients=orddict:new()}).  start() ->      register(?MODULE, spawn(?MODULE, init, [])).  %% Wysyłanie do wszystkich klientów  send_to_clients(Msg, ClientDict) ->      orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).   `

3\. Bezpieczeństwo i "Let It Crash"
-----------------------------------

### Monitory i Linki

4\. Zadania dla Studentów (Jupyter Notebook)
--------------------------------------------

Poniższe zadania mają na celu przećwiczenie mechanizmu receive, after oraz komunikacji między procesami.

### Zadanie 1: "Leniwy Student" (Wprowadzenie do timeoutów)

Napisz moduł lazy\_student, który symuluje studenta uczącego się do egzaminu.

1.  Proces czeka na wiadomość ucz\_sie.
    
2.  Jeśli dostanie wiadomość ucz\_sie, wypisuje "OK, juz sie ucze..." i wraca do pętli.
    
3.  Jeśli **NIE** dostanie żadnej wiadomości przez 3000ms (użyj after), wypisuje "Zasnalem z nudow!" i kończy działanie.
    
4.  Wiadomość koniec kończy proces natychmiastowo.
    

### Zadanie 2: "Prosty Rozdzielacz Zadań" (Architektura Server-Worker)

Napisz proces router, który rozdziela zadania matematyczne.

1.  Proces przyjmuje wiadomości: {oblicz, dodaj, A, B} oraz {oblicz, mnoz, A, B}.
    
2.  Router **nie liczy sam!** Dla każdego zadania tworzy (**spawnuje**) nowy, anonimowy proces (fun() -> ... end), który wykonuje obliczenie, wypisuje wynik io:format(...) i kończy się.
    
3.  Router wraca do pętli, gotowy na kolejne zadania.
    

👨‍🏫 Rozwiązania dla Prowadzącego
----------------------------------

### Rozwiązanie Zadania 1 ("Leniwy Student")

Tutaj kluczowe jest użycie konstrukcji receive ... after.

Erlang

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -module(lazy_student).  -export([start/0, loop/0]).  start() -> spawn(?MODULE, loop, []).  loop() ->      receive          ucz_sie ->              io:format("Student: OK, juz sie ucze...~n"),              loop(); %% Rekurencja - student czuwa dalej          koniec ->              io:format("Student: Koniec zajec!~n")      after 3000 ->          io:format("Student: Zasnalem z nudow! (Timeout)~n")          %% Brak rekurencji - proces umiera      end.  %% Test w shellu:  %% Pid = lazy_student:start().  %% Pid ! ucz_sie.  %% (Czekamy 3 sekundy...) -> "Zasnalem z nudow!"   `

### Rozwiązanie Zadania 2 ("Prosty Rozdzielacz Zadań")

To zadanie uczy, że proces główny (serwer) nie powinien być blokowany przez długie obliczenia.

Erlang

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -module(router).  -export([start/0, loop/0]).  start() -> spawn(?MODULE, loop, []).  loop() ->      receive          {oblicz, dodaj, A, B} ->              %% Spawnujemy anonimową funkcję (lambdę) dla operacji              spawn(fun() ->                  io:format("Worker: Wynik dodawania ~p + ~p = ~p~n", [A, B, A+B])              end),              loop();          {oblicz, mnoz, A, B} ->              spawn(fun() ->                  io:format("Worker: Wynik mnozenia ~p * ~p = ~p~n", [A, B, A*B])              end),              loop();          stop ->              io:format("Router: Zamykam biuro.~n")      end.  %% Test:  %% R = router:start().  %% R ! {oblicz, mnoz, 5, 5}.  %% R ! {oblicz, dodaj, 2, 2}.   `

### Dodatek: Diagramy pomocnicze (do narysowania na tablicy)

1.  **Drzewo Nadzoru (Supervision Tree):**Sup (Supervisor) -> EvServ (Server) -> \[Event1, Event2, ...\]
    
2.  **Cykl Życia Wiadomości:**Client -> (Msg) -> Server Mailbox -> (Pattern Match w pętli) -> Zmiana Stanu -> Rekurencyjne loop()