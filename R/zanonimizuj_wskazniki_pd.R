#' @title Anonimizowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala zanonimizować wskaźniki zagregowane (zamienić ich wartości na
#' braki danych) przeznaczone do publicznej prezentacji, które zostały obliczone
#' na podstawie danych opisujących mniej niż zadaną liczbę absolwentów lub
#' na podstawie absolwentów mniej niż zadanej liczby różnych szkół.
#'
#' Warto zwrócić uwagę, że taka anonimizajca ignoruje ewentualną możliwość
#' określenia wartości wskaźnika poprzez znajomość wartości wskaźników w grupie
#' ogólniejszej i we wszystkich innych grupach poza daną (implementacja
#' anonimizacji uwzględniającej ten fakt była by jednak dużo bardziej
#' skomplikowana).
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @param progAbs liczba dodatnia - liczba absolwentów poniżej której wskaźniki
#' zostaną zanonimizowane
#' @param progSzk liczba dodatnia - liczba szkół, poniżej której wskaźniki
#' zostaną zanonimizowane
#' @returns Ramka danych przekazana argumentem `x`, w której wskaźniki zostały
#' zanonimizowane.
#' @details
#' Funkcja anonimizuje **wszystko, co *wyglądają jej na zagregowany wskaźnik
#' przeznaczony do publicznej prezentacji***, co w praktyce oznacza, że
#' sprawdzane są elementy kolumn-list w ramce danych przekazanej argumentem `x`.
#' Jeśli element takiej kolumny listy ma choć jeden z atrybutów nazywających się
#' `lAbs` lub `lSzk`, ich wartości zostaną przyrównane do progów podanych
#' argumentami odpowiednio `probAbs` i `progSzk` w wywołaniu funkcji. Jeśli
#' wartość choć jednego z tych atrybutów jest mniejsza od odpowiedniej wartości
#' progu (**ale większa od zera**), to wartości danego elementu listy zostaną
#' zamienione na braki danych (jeśli jest to wektor czy *array*, wszystkie jego
#' elementy zostaną zamienione na braki danych), tak samo jak wartości atrybutów
#' `lZadenZWymienionych` i `lNieDotyczy` (jeśli były zdefiniowane); z kolei
#'  wartości atrybutów `lAbs` i `lSzk` zostaną zamienione na wartości -1 razy
#'  odpowiednio `probAbs` i `progSzk`.
#'
#' **Wskaźniki obliczone na podstawie pustych grup absolwentów nie są
#' anonimizowane!**
#' @importFrom dplyr %>% across mutate where
#' @export
zanonimizuj_wskazniki_pd <- function(x, progAbs = 10, progSzk = 3) {
  stopifnot(is.data.frame(x),
            is.numeric(progAbs), length(progAbs) == 1L, !is.na(progAbs),
            is.numeric(progSzk), length(progSzk) == 1L, !is.na(progSzk))
  x <-
    mutate(x,
           across(where(is.list),
                  ~lapply(.,
                          function(x, progAbs, progSzk) {
                            a <- attributes(x)
                            if (any(c("lAbs", "lSzk") %in% names(a))) {
                              if (!("lAbs" %in% names(a))) a$lAbs <- 0L
                              if (!("lSzk" %in% names(a))) a$lSzk <- 0L

                              if ((a$lAbs < progAbs & a$lAbs > 0) | is.na(a$lAbs) |
                                  (a$lSzk < progSzk & a$lSzk > 0) | is.na(a$lSzk)) {
                                x[] <- NA_integer_
                                attributes(x)[names(a) %in% c("lZadenZWymienionych",
                                                              "lNieDotyczy")] <-
                                  lapply(a[names(a) %in% c("lZadenZWymienionych",
                                                           "lNieDotyczy")],
                                         function(x) {
                                           x[] <- NA_integer_
                                           return(x)
                                         })
                                attributes(x)$lAbs <- -progAbs
                                attributes(x)$lSzk <- -progSzk
                              }
                            }
                            return(x)
                          }, progAbs = progAbs, progSzk = progSzk)))
  return(x)
}
