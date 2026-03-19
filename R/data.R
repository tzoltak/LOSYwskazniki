#' @title Legalne kontynuacje nauki
#' @description Zestawienie nominalnie jedynych możliwych form kontynuowania
#' nauki w zależności od typu ukończonej szkoły ponadpodstawowej.
#' @format Ramka danych z 26 wierszami i 2 kolumnami:
#' \itemize{
#'   \item{`typ_szk` - typ ukończonej szkoły (czynnik),}
#'   \item{`typ_szk_kont` - forma kontynuacji nauki (wektor ciągów znaków).}
#' }
#' @seealso [dodaj_wskazniki_kontynuacje()]
"legalneKontynuacje"
#' @title Czasy obliczania wskaznikow w zaleznosci od liczby rdzeni
#' @description Zestawienie czasów obliczania wskaźników przez
#' [oblicz_wskazniki_pd_jst()] w zależności od wartości argumentów
#' `liczbaWatkow` i `liczbaWatkowGrupy`. Wykorzystano dane dla rocznika
#' absolwentów 2020 z edycji monitoringu 2022. Jako zestawu zmiennych
#' grupujących użyto: `typ_szk`, `szk_specjalna`, `kod_zaw`, `nazwa_zaw`,
#' `plec`, `mlodoc`, przy domyślnym zestawie obliczanych wskaźników. Obliczenia
#' zostały wykonane na laptopie z dwunastordzeniowym procesorem AMD Ryzen 5 5600H
#' z 64 GB RAM.
#'
#' Dla każdej kombinacji wartości parametrów obliczenia uruchamiano tylko raz,
#' nie dbając przesadnie o standaryzację warunków - niekiedy (gdy
#' wykorzystywanych było łącznie mniej rdzeni) w tle działała również
#' przeglądarka internetowa i aplikacja poczty elektornicznej, więc **wyniki nie
#' są zbyt dokładne**.
#' @format Ramka danych z 51 wierszami i 9 kolumnami:
#' \itemize{
#'   \item{`lWatkowJST` - liczba procesów na poziomie JST (w wywołaniu `oblicz_wskazniki_pd_jst()`),}
#'   \item{`lWatkowGrupy` - liczba procesów na poziomie grup (w wywołaniu `oblicz_wskazniki_grupy()` - w ramach każdego procesu na poziomie JST).}
#'   \item{`lWatkowOgolem` - łączna liczba równoległych procesów.}
#'   \item{`poziom` - poziom agregacji JST (w wywołaniu `oblicz_wskazniki_pd_jst()`).}
#'   \item{`czas` - czas obliczania wskaźników \[s].}
#'   \item{`czasRel1.1` - iloraz czasu obliczania wskaźników do czasu obliczania wskaźników na tym samym poziomie agregacji JST, bez zrównoleglania.}
#'   \item{`czasRelJST.1` - iloraz czasu obliczania wskaźników do czasu obliczania wskaźników na tym samym poziomie agregacji JST, z taką samą liczbą równoległych wątków na poziomie JST i bez zrównoleglania na poziomie grup.}
#'   \item{`czasRel1.Grupy` - iloraz czasu obliczania wskaźników do czasu obliczania wskaźników na tym samym poziomie agregacji JST, bez zrównoleglania na poziomie JST i z taką samą liczbą równoległych wątków na poziomie grup.}
#'   \item{`czasRel1Ogolem` - iloraz czasu obliczania wskaźników do czasu obliczania wskaźników na tym samym poziomie agregacji JST, z tą samą liczbą równoległych wątków ogółem i bez zrównoleglania na poziomie JST.}
#' }
#' @seealso [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()]
"czasy_wskazniki_pd"
