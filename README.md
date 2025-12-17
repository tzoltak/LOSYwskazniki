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

Pakiet zawiera trzy funkcje, których domyślny sposób użycia polega na dodaniu zestawu wskaźników - wykorzystywanych w publikowanych w ramach systemu monitoringu raportach - do tabeli *pośredniej* P4:

-   `dodaj_wskazniki_dyplomy()` - wskaźniki dotyczące uzyskania świadectwa dojrzałości, dyplomu zawodowego, certyfikatów kwalifikacji i tytułu czeladnika,
-   `dodaj_wskazniki_kontynuacje()` - wskaźniki dotyczące form i kierunków kontynuowania nauki,
-   `dodaj_wskazniki_prace()` - wskaźniki dotyczące wynagrodzeń oraz czasu posiadania zatrudnienia lub bycia bezrobotnym.

Wszystkie one są wywoływane podczas przygotowywania tabel *pośrednich* przez `MLASdaneAdm::przygotuj_tabele_posrednie()` (począwszy od wersji 1.2.0 pakietu *MLASdaneAdm*) i ich późniejsze wywoływanie raczej mija się z celem, gdyż - może poza `dodaj_wskazniki_dyplomy()` wywołanej z inną wartością argumentu `maksMiesOdUkoncz` - nie spowoduje obliczenia żadnych nowych wskaźników.

Pakiet zawiera też dwie funkcje trochę *niższego poziomu*, wykorzystywane intensywnie wewnątrz tych opisanych wyżej:

-   `oblicz_wskaznik_macierz()` - pozwala obliczyć wartość wskaźnika opisującego wiele niewykluczających się wzajemnie stanów (np. kontynuowanie nauki w różnych formach w danym miesiącu od ukończenia szkoły); tworzony wskaźnik ma postać kolumny-macierzy;
-   `oblicz_wskaznik_z_p3()` - pozwala dokonać agregacji wartości zmiennej po czasie w ramach poszczególnych absolwentów.

Ponadto zawiera też funkcję pozwalającą zagregować dane - typowo zawarte w tabeli *pośredniej* P3 - do postaci, w której mogą one zostac łatwo wykorzystane do przygotowania wykresu przepływów, w szczególności z wykorzystaniem pakietu *ggalluvial* (i korzystającego z tego pakietu szablonu wykresu `wykresPrzeplywyStatusy`, zawartego w pakiecie *LOSYkolory*):

-   `przygotuj_dane_przeplywy()`.
