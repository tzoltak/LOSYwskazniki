# LOSYwskazniki 0.6.0 (22.03.2026)

## Nowe funkcje

-   `dopisz_wskaznik_pd_liczba_abs` pozwala dodać do zestawienia zagregowanych wskaźników w formie odpowiedniej do wykorzystania w zastosowaniach, w których takie zestawienia wskaźników mają być publicznie dostępne (np. w interaktywnych raportach przygotowanych w formie statycznych stron www) wskaznik opisujący liczbę absolwentów (obliczany na podstawie atrybutów wskazanego, już wcześniej obliczonego wskaźnika).

## Zmiany nazw argumentów funkcji

-   Nazwę argumentu `zmTylkoWartosciWdanych` zmieniono na, w pełni zgodną z konwencją, `zmTylkoWartosciWDanych` w funkcjach `oblicz_wskazniki_pd_grupy()` i `oblicz_wskazniki_pd_grupy()`.
    -   Ta zmiana ma potencjał do generowania błędów w już napisanym kodzie korzystającym z pakietu, ale biorąc pod uwagę, że poza mną raczej nikt go jeszcze nie używa, zdecydowałem się ją wprowadzić w sposób *bezkompromisowy*.
-   Domyślna wartość argumentu `zmTylkoWartosciWDanych` w funkcjach `oblicz_wskazniki_pd_grupy()` i `oblicz_wskazniki_pd_grupy()` została zmieniona tak, aby obejmowała również zmienną grupującą `mlodoc` w ramach `typ_szk`.

## Udoskonalenia

-   W `oblicz_wskazniki_pd_grupy()` zrównoleglono część kodu odpowiedzialną za wywoływanie `oblicz_wskazniki_pd()`.
    -   `oblicz_wskazniki_pd_grupy()` otrzymała nowy argument `liczbaWatkow`, pozwalający wskazać liczbę procesów wykorzystywanych do obliczeń.
    -   `oblicz_wskazniki_pd_jst()` otrzymała nowy argument `liczbaWatkowGrupy`, którego wartość przekazywana jest do argumentu `liczbaWatkow` przy wywoływaniu `oblicz_wskazniki_pd_grupy()`.
-   W `oblicz_wskazniki_pd_jst()` zrównoleglono część kodu odpowiedzialną za wywoływanie `oblicz_wskazniki_pd_grupy()`.
    -   `oblicz_wskazniki_pd_jst()` otrzymała nowy argument `liczbaWatkow`, pozwalający wskazać liczbę procesów wykorzystywanych do obliczeń.
    -   Zrównoleglanie na tym poziomie oczywiście może zostać zastosowane tylko przy obliczaniu wskaźników dla województw lub powiatów.
    -   Testy empiryczne na danych do prototypu raportu ogólnopolskiego wskazują, że .
-   W związku z implementacją wielowątkowości pakiet zależy teraz od pakietu *parallel*.
-   Przeprowadzono (niezbyt rozbudowane) testy empiryczne mające na celu ustalenie optymalnych wartości argumentów opisujących zrównoleglannie - ich wyniki zostały dołączone do pakietu w postaci zbioru danych `czasy_wskazniki_pd` i omówione w dokumentacji funkcji `oblicz_wskazniki_pd_JST()`.

## Naprawione błędy

-   `oblicz_wskazniki_pd_grupy()`:
    -   rozpatrując, które kombinacje wartości nie występują w danych (na podstawie argumentu `zmTylkoWartosciWDanych`) definiując pogrupowanie bierze pod uwagę również zmienną opisaną nazwą elementu listy,
    -   sprawdzając, czy dana zmienna grupująca opisuje kategorię *ogółem* przyrównuje jej wartość do weartości podanej argumentem `etykietaOgolem`, zamiast do ciagu znaków "Ogółem",
    -   radzi sobie w sytuacji, gdy wśród zmiennych grupujących nie ma żadnych, które występowałyby również w `zmTylkoWartosciWDanych`.

# LOSYwskazniki 0.5.0 (28.01.2026)

## Udoskonalenia

-   `przygotuj_wskazniki_pd_toJSON()` otrzymała dwa nowe argumenty: `komunikatBrakDanych` i `komunikatCenzura`, które pozwalają zapisać bezpośrednio w danych treść komunikatów opisujących sytuację, gdy dla danego wskaźnika nie było żadnych absolwentów, na podstawie których można by go obliczyć lub gdy wartość wskaźnika została *ocenzurowana* (zanonimizowana).

## Naprawione błędy

-   `oblicz_wskazniki_pd()` (a w konsekwencji także `oblicz_wskazniki_pd_grupy()` i `oblicz_wskazniki_pd_jst()`) poprawnie tworzy nazwę wskaźnika w sytuacji, gdy na podstawie P3 obliczany jest tylko jeden wskaźnik.
-   Literówki w dokumentacji.

# LOSYwskazniki 0.4.0 (16.01.2026)

## Nowe funkcje

-   Funkcje pozwalające obliczać zestawienia zagregowanych wskaźników w formie odpowiedniej do wykorzystania w zastosowaniach, w których takie zestawienia wskaźników mają być publicznie dostępne (np. w interaktywnych raportach przygotowanych w formie statycznych stron www):
    -   `oblicz_wskazniki_pd_jst()` - najogólniejsza funkcja, pozwalająca przygotować zestawienie wartości zagregowanych wskaźników z danej edycji monitoringu dla wszystkich JST na określonym poziomie (Polska jako całość, województwa, powiaty),
    -   `oblicz_wskazniki_pd_grupy()` - wykorzystywana przez `oblicz_wskazniki_pd_jst()` (w ramach konkretnej JST) do przygotowania wszystkich kombinacji wartości zmiennych niezależnych (filtrujących), a także dodanie wartości *ogółem*, i obliczenie wartości zagregowanych wskaźników dla każdej z nich,
    -   `oblicz_wskazniki_pd()` - wykorzystywana przez `oblicz_wskazniki_pd_grupy()` do obliczenia wartości zagregowanych wskaźników na podstawie kolumn w przekazanych podzbiorach tabel *pośrednich* P4 i P3, adekwatnie do typu agregowanego wskaźnika,
    -   `zanonimizuj_wskazniki_pd()` - pozwala zanonimizować (zastąpić brakami danych) wartości zagregowanych wskaźników, które zostały obliczone na podstawie mniej niż zadanej liczby absolwentów lub szkół;
    -   `przygotuj_wskazniki_pd_toJSON()` - pozwalać przekształcić wskaźniki obliczone przez funkcję `oblicz_wskazniki_pd()`(a więc również przez `oblicz_wskazniki_pd_grupy()` lub `oblicz_wskazniki_pd_jst()` do formatu, który będzie przyjazny zapisaniu ich w formacie JSON przy pomocy funkcji `toJSON()` z pakietu *jsonlite*.
-   Funkcje pomocnicze, wykorzystywane przez `oblicz_wskazniki_pd_grupy()`, ale potencjalnie użyteczne również w innych kontekstach:
    -   `podmien_braki_danych()` - pozwala podmienić braki danych w wektorze/czynniku na podaną wartość; w odróżnieniu od wielu innych podobnych funkcji (w innych pakietach) **obsługuje również czynniki**;
    -   `dodaj_wartosc_ogolem()` - pozwala dodać do zestawu wartości danego czynnika (wektora, przekształcając go przy tym na czynnik) dodatkową wartość, z założenia mającą opisywać, że chodzi o zestawienie ogółem ze względu na daną zmienną.

## Drobne poprawki

-   `przygotuj_dane_przeplywy()` ostrzega, jeśli argument `punkyCzasu` podaje tylko jedną wartość.
-   Literówki w dokumentacji.

# LOSYwskazniki 0.3.0 (17.12.2025)

## Nowe funkcje

-   `przygotuj_dane_przeplywy()` pozwala zagregować dane - typowo zawarte w tabeli *pośredniej* P3 - do postaci, w której mogą one zostac łatwo wykorzystane do przygotowania wykresu przepływów, w szczególności z wykorzystaniem pakietu *ggalluvial* (i korzystającego z tego pakietu szablonu wykresu `wykresPrzeplywyStatusy`, zawartego w pakiecie *LOSYkolory*).

## Udoskonalenia

-   Funkcje `dodaj_wskazniki_dyplomy()`, `dodaj_wskazniki_kontynuacje()` i `dodaj_wskazniki_praca()` sprawdzają, czy zmienne, które będą tworzyć nie istnieją już w danych przekazanych argumentem `p4`, a jeśli tak, to je usuwają (generując ostrzeżenie).
-   W dokumentacji funkcji `dodaj_wskazniki_dyplomy()`, `dodaj_wskazniki_kontynuacje()` i `dodaj_wskazniki_praca()` dodano przykłady użycia, przy czym do uruchomienia wymagają one posiadania załadowanych w ramach aktywnej sesji R odpowiednich danych z tabelami *pośrednimi* (które **nie** są dołączone do pakietu).

# LOSYwskazniki 0.2.0 (21.11.2025)

## Naprawione błędy

-   `dodaj_wskazniki_prace()` oblicza `sr_wynagr_uop_nauka_r0_wrzgru` i `sr_wynagr_uop_bez_nauki_r0_wrzgru` jako **względne** wynagrodzenia (w odniesieniu do śr. miesięcznych wynagrodzeń w powiecie zamieszkania w danym roku).

## Nowe funkcje

-   `oblicz_wynagrodzenia_wzgledne()` pozwala obliczyć wskaźniki względnych wynagrodzeń (w odniesieniu do śr. miesięcznych wynagrodzeń w powiecie zamieszkania w danym roku); jej wyodrębnienie wynika z chęci umożliwienia jej użycia w ramach pakietu *MLASdaneAdm*.

# LOSYwskazniki 0.1.3 (17.11.2025)

## Naprawione błędy

-   `dodaj_wskazniki_kontynuacje()` odnajduje informacje o nauce na KKZ i KUZ (w przypadku KKZ wykluczając te, związane z nauką jako uczeń w BS II).
-   `dodaj_wskazniki_kontynuacje()` poprawnie odfiltrowuje tylko *legalne* kontynuacje, biorąc pod uwagę również KKZ i KUZ.

# LOSYwskazniki 0.1.2 (14.10.2025)

-   `dodaj_wskazniki_prace()` oblicza wskaźniki opisujące wynagrodzenia z zatrudnienia w formie umowy o pracę od września do grudnia roku ukończenia szkoły w podziale na okresy kontynuacji i niekontynuowania nauki.

# LOSYwskazniki 0.1.1 (25.08.2025)

-   `dodaj_wskazniki_prace()` oblicza również wskaźnik opisujący wynagrodzenia w I kwartale roku następującego po roku ukończenia szkoły.

# LOSYwskazniki 0.1.0 (08.08.2025)

-   Pierwsza działająca wersja pakietu.
