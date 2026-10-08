# LOSYwskazniki

![FERS+RP+UE+IBE-PIB](inst/Belka-FERS-IBE-PIB.png)

Pakiet został opracowany w ramach projektu *Rozwój Systemu Monitoringu Karier Absolwentów i Absolwentek Szkół Ponadpodstawowych* (FERS.01.04-IP.05-0013/23) prowadzonego w Instytucie Badań Edukacyjnych Państwowym Instytucie Badawczym, który jest finansowany z Funduszy Europejskich dla Rozwoju Społecznego (FERS).

Pakiet zawiera funkcje pozwalające obliczać wskaźniki charakteryzujące przebieg karier poszczególnych absolwentów, wykorzystywane w systemie monitoringu karier absolwentów polskich szkół ponadpodstawowych.

# Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN, więc trzeba instalować go ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

``` r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/LOSYwskazniki')
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

# Użycie

## Tworzeniu tabeli *pośredniej* p4 (i analogiczne zastosowania)

Pakiet zawiera trzy funkcje, których domyślny sposób użycia polega na dodaniu zestawu wskaźników - wykorzystywanych w publikowanych w ramach systemu monitoringu raportach - do tabeli *pośredniej* P4:

-   `dodaj_wskazniki_dyplomy()` - wskaźniki dotyczące uzyskania świadectwa dojrzałości, dyplomu zawodowego, certyfikatów kwalifikacji i tytułu czeladnika,
-   `dodaj_wskazniki_kontynuacje()` - wskaźniki dotyczące form i kierunków kontynuowania nauki,
-   `dodaj_wskazniki_prace()` - wskaźniki dotyczące wynagrodzeń oraz czasu posiadania zatrudnienia lub bycia bezrobotnym.

Wszystkie one są wywoływane podczas przygotowywania tabel *pośrednich* przez `MLASdaneAdm::przygotuj_tabele_posrednie()` (począwszy od wersji 1.2.0 pakietu *MLASdaneAdm*) i ich późniejsze wywoływanie raczej mija się z celem, gdyż - może poza `dodaj_wskazniki_dyplomy()` wywołanej z inną wartością argumentu `maksMiesOdUkoncz` - nie spowoduje obliczenia żadnych nowych wskaźników.

Pakiet zawiera też dwie funkcje trochę *niższego poziomu*, wykorzystywane intensywnie wewnątrz tych opisanych wyżej:

-   `oblicz_wskaznik_macierz()` - pozwala obliczyć wartość wskaźnika opisującego wiele niewykluczających się wzajemnie stanów (np. kontynuowanie nauki w różnych formach w danym miesiącu od ukończenia szkoły); tworzony wskaźnik ma postać kolumny-macierzy;
-   `oblicz_wskaznik_z_p3()` - pozwala dokonać agregacji wartości zmiennej po czasie w ramach poszczególnych absolwentów.

## Przygotowywanie danych do wykresów przepływów

Ponadto zawiera też funkcję pozwalającą zagregować dane - typowo zawarte w tabeli *pośredniej* P3 - do postaci, w której mogą one zostac łatwo wykorzystane do przygotowania wykresu przepływów, w szczególności z wykorzystaniem pakietu *ggalluvial* (i korzystającego z tego pakietu szablonu wykresu `wykresPrzeplywyStatusy`, zawartego w pakiecie *LOSYkolory*):

-   `przygotuj_dane_przeplywy()`.

## Przygotowywanie zestawień *publicznych* wskaźników zagregowanych

Oddzielny zestaw funkcji pozwala przygotować zestawiania wskaźników zagregowanych, które mają być dostępne publicznie, w związku z czym muszą zostać obliczone we wszystkich pożądanych przekrojach analitycznych definiowanych przez kombinację wartości wskazanych zmiennych (tak, aby nie zachodziła potrzeba żadnej ich dalszej agregacji):

-   `oblicz_wskazniki_pd_jst()` - najogólniejsza funkcja, pozwalająca przygotować zestawienie wartości zagregowanych wskaźników z danej edycji monitoringu dla wszystkich JST na określonym poziomie (Polska jako całość, województwa, powiaty);
-   `oblicz_wskazniki_pd_grupy()` - wykorzystywana przez `oblicz_wskazniki_pd_jst()` (w ramach konkretnej JST) do przygotowania wszystkich kombinacji wartości zmiennych niezależnych (filtrujących), a także dodanie wartości *ogółem*, i obliczenie wartości zagregowanych wskaźników dla każdej z nich;
-   `oblicz_wskazniki_pd()` - wykorzystywana przez `oblicz_wskazniki_pd_grupy()` do obliczenia wartości zagregowanych wskaźników na podstawie kolumn w przekazanych podzbiorach tabel *pośrednich* P4 i P3, adekwatnie do typu agregowanego wskaźnika;
-   `usun_puste_wskazniki_pd()` - pozwala usunąć z zestawienia wartości zagregowanych wskaźników (w formacie *długim*) wiersze opisujące takie kombinacje cech absolwentów, które nie wysępowały w danych (lub zostały zanonimizowane);
-   `zanonimizuj_wskazniki_pd()` - pozwala zanonimizować (zastąpić brakami danych) wartości zagregowanych wskaźników, które zostały obliczone na podstawie mniej niż zadanej liczby absolwentów lub szkół;
-   `dodaj_procenty_wskazniki_pd()` - pozwala dodać do już policzonych wskaźników będących rozkładami liczebności również rozkład częstości;
-   `splaszcz_wskazniki_pd()` - pozwala *spłaszczyć* strukturę zestawienia wartości zagregowanych wskaźników tak, aby nie zawierała już ona kolumn-list-wektorów; uwaga, zwracana ramka danych może mieć bardzo dużo kolumn;
-   `przygotuj_wskazniki_pd_toJSON()` - pozwalać przekształcić wskaźniki obliczone przez funkcję `oblicz_wskazniki_pd()`(a więc również przez `oblicz_wskazniki_pd_grupy()` lub `oblicz_wskazniki_pd_jst()` do formatu, który będzie przyjazny zapisaniu ich w formacie JSON przy pomocy funkcji `toJSON()` z pakietu *jsonlite*.

Wykorzystywane są do tego również funkcje pomocnicze, które mogą okazać się przydatne i w innych kontekstach:

-   `podmien_braki_danych()` - pozwala podmienić braki danych w wektorze/czynniku na podaną wartość; w odróżnieniu od wielu innych podobnych funkcji (w innych pakietach) **obsługuje również czynniki**;
-   `dodaj_wartosc_ogolem()` - pozwala dodać do zestawu wartości danego czynnika (lub wektora, jednocześnie przekształcając go przy tym na czynnik - wtedy pod uwagę brane unikalne są wartości występujące w tym wektorze) dodatkową wartość, z założenia mającą opisywać, że chodzi o zestawienie ogółem ze względu na daną zmienną.

Historycznie, ten zbiór funkcji został przygotowany z myślą o tworzeniu zestawień wskaźników na potrzeby publikacji wyników monitoringu w formie ogólnodostępnego raportu interaktywnego (*dashboardu*). **Można jednak wykorzystać je do łatwego przygotowania dowolnego zestawienia wskaźników będących agregatami zmiennych już istniejących w tabelach pośrednich *p3* lub *p4***:

```{r}
library(dplyr)
library(LOSYwskazniki)
# 1. Wczytanie pliku z tabelami pośrednimi
load("tabele-posrednie-2021.RData") 
# 2. Odfiltrowanie interesujących nas absolwentów
p4 <- p4 |>
  filter(rok_abs == 2019,
         !szk_specjalna)
# 3. Odfiltrowanie p3 do wybranych miesiecy
p3 <- p3 |>
  filter(mies_od_ukoncz %in% c(6, 18))
# 4. Obliczenie zagregowanych wskaźników
wyniki <-
  oblicz_wskazniki_pd_jst(p4 = p4, p3 = p3,
                          poziom = "Polska",
                          zmGrupujace = "typ_szk",
                          zmWskaznikiP4 = c("matura_zdana", "dyplom_zaw",
                                            "typ_szk_kont6", "typ_szk_kont18"),
                          zmWskaznikiP3 = "status")
# 5. Usunięcie kombinacji wartości niewystępujących w danych, zanonimizowanie,
#    dodanie procentów i spłaszczenie
wynikiSplaszczone <- wyniki |>
  zanonimizuj_wskazniki_pd() |>
  usun_puste_wskazniki_pd() |>
  dodaj_procenty_wskazniki_pd(dopiszZadenZWymienionych = TRUE) |>
  splaszcz_wskazniki_pd(dopiszAtrybuty = "lAbs")
# Alternatywnie
wynikiSplaszczoneKazdyWskaznikOddzielnie <- wyniki |>
  zanonimizuj_wskazniki_pd() |>
  usun_puste_wskazniki_pd() |>
  dodaj_procenty_wskazniki_pd(dopiszZadenZWymienionych = TRUE)
wynikiSplaszczoneKazdyWskaznikOddzielnie <- wynikiSplaszczoneKazdyWskaznikOddzielnie
  split(wynikiSplaszczoneKazdyWskaznikOddzielnie$wskaznik) |>
  lapply(splaszcz_wskazniki_pd, dopiszAtrybuty = "lAbs")
```

Warto przy tym pamiętać o następujących kwestiach:

-  Wskaźniki na poziomie ogólnopolskim można równie dobrze obliczyć przy pomocy `oblicz_wskazniki_pd_jst()` jak i `oblicz_wskazniki_pd_grupy()`, ale użycie tej pierwszej ma tę zaletę, że zawsze grupuje po kohorcie absolwentów, a więc zabezpiecza przed omyłkowym obliczeniem agregatów dla kilku kohort naraz (czego co do zasady nie chcemy robić).
-  `oblicz_wskazniki_pd_jst()` i `oblicz_wskazniki_pd_grupy()` dają się wywołać bez podania `zmWskaznikiP4` i `zmWskaznikiP3`, ale domyślne wartości tych argumentów opisują zestaw wskaźników wybrany w 2025 r. do prezentacji w ogólnodostepnym raporcie interaktywnym.
-  Możliwe jest obliczenie wskaźników tylko na podstawie zmiennych z *p3* - należy wtedy wywołać funkcję z argumentem `zmWskaznikiP3=NULL` (i, opcjonalnie,  `p3=NULL`).
-  Możliwe jest obliczenie wskaźników tylko na podstawie zmiennych z *p4* - należy wtedy wywołać funkcję z argumentem `zmWskaznikiP4=NULL`.
-  `oblicz_wskazniki_pd_jst()` i `oblicz_wskazniki_pd_grupy()` **nie** grupują domyślnie po typie szkoły (choć co do zasady chcemy obliczać wskaźniki w podziale na typy szkół) - trzeba im to kazać używając argumentu `zmGrupujace`.
-  Wyjąwszy zmienne podane argumentem `zmTylkoWartosciWDanych` (domyślnie `typ_szk`, `kod_zaw`, `nazwa_zaw` i `mlodoc`) `oblicz_wskazniki_pd_jst()` i `oblicz_wskazniki_pd_grupy()` obliczają wartości wskaźników również dla tych kombinacji wartości zmiennych grupujacych, które nie występują w danych. Można chcieć je potem usunąć, korzystając z funkcji `usun_puste_wskazniki_pd()`.
   -  Sposób konstruowania argumentu `zmTylkoWartosciWDanych` jest skomplikowany - p. dokumentacja funkcji `oblicz_wskazniki_pd_grupy()`.
   -  Takie *puste* kombinacje wartości zmiennych grupujących można potem usunąć przy pomocy funkcji `usun_puste_wskazniki_pd()` - ale należy się zastanowić, czy w konkretym kontekście jest to pożądane (czasem zaprezentowanie explicite informacji, że dana kombinacja wartości zmiennych *grupujących* jest pusta, może być użyteczne).
   -  Wywołania `usun_puste_wskazniki_pd(usunZanonimizowane = TRUE)` można użyć do usunięcia z zestawienia również tych kombinacje wartości zmiennych grupujących, które zostały zanonimizowane ze względu na zbyt małą liczebność.
-  Zestaw miesięcy, dla których mają zostać obliczone agregaty zmiennych zawartych w *p3* określa się odpowiednio odfiltrowując dane przekazane argumentem `p3`. Można to oczywiście zrobić *w locie* w wywołaniu funkcji.
-  Nie ma obecnie możliwości grupowania po żadnej innej zmiennej z *p3*, niż `mies_od_ukoncz` (a to grupowanie dokonuje się automatycznie zawsze).
-  Anonimizację i dodanie rozkładów częstości można dokonać w dowolnej kolejności.
-  Funkcja `zanonimizuj_wskazniki_pd()` oprócz tego, że zamienia wartości anonimizowanych wskaźników na braki danych (w przypadku tych wskazanych argumentem `wskUsuwajZestawWartosci` również usuwa informacje o tym, jakie wartości wystąpiły w danych), zmienia również wartości atrybutów `lAbs` i `lSzk` na liczby ujemne: minus wartości zastosowanych progów anonimizacji.
   -  Warto o tym pamiętać, jeśli zanonimizowane kombinacje wartości zmiennych grupujących nie zostały usunięte z danych, a w wywołaniu `splaszcz_wskazniki_pd()` używa się argumentu `dopiszAtrybuty` do utworzenia w *spłaszczonym* zbiorze kolumn opisujących liczbę absolwentów lub szkół.
-  Jeśli obliczyliśmy jednocześnie wiele różnych wskaźników, to sensowne może być najpierw podzielić uzyskany obiekt na ramki danych zawierające tylko pojedyncze wskaźniki (lub wręcz wskaźniko-okresy) i każdy z nich spłaszczać oddzielnie. Oczywiście, jeśli ktoś czuje się komfortowo z bardzo *szerokimi* ramkami danych, to nie ma takiej potrzeby.
-  Wskaźniki migracyjne można obliczać na podstawie zmiennych `teryt_zam` i `teryt_pow_szk_kont` z *p3* (lub jakiejś utworzonej samemu zmiennej będącej ich przekształceniem), tylko wcześniej należy je zamienić na ciagi znaków albo czynniki.

