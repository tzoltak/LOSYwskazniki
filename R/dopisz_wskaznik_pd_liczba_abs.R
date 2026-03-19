#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala dodać do ramki danych z wartościami wskaźników zwróconej
#' przez [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()] dodatkowe wiersze ze wskaźnikiem "liczba_abs",
#' opisującym liczbę absolwentów, a utworzonym na podstawie informacji o liczbie
#' absolwentów zawartej w atrybutach jednego ze wskaźników już występujących
#' w przekazanych danych.
#' @param x ramka danych z wartościami wskaźników zwrócona przez
#' [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @param uzyjWskaznik ciąg znaków z nazwą wskaźnika, na podstawie
#' którego ma zostać utworzony wskaźnik opisujący liczbę absolwentów
#' @param uzyjCzas opcjonalnie wektor zawierający wartości zmiennej `czas`
#' (w danych przekazanych argumentem `x`), dla których ma zostać obliczony nowy
#' wskaźnik; domyślna wartość - brak danych - wskazuje, że mają zostać użyte
#' wszystkie wiersze (opisujące wskaźnik, na który wskazuje argument
#' `uzyjWskaznik`) występujące w przekazanych danych
#' @returns Ramka danych przekazana argumentem `x` z dodanymi wierszami,
#' w których zmienna `wskaźnik` przyjmuje wartość "liczba_abs", zmienna
#' `czas` przyjmuje wartość "0", a zmienna `wartosc` jest listą
#' jednoelementowych wektorów liczb (każdy taki pojedynczy element ma nazwę
#' "liczba") i atrybutami `lAbs` i `lSzk`.
#' @details
#' Nowe wiersze opisują wszystkie występujące w ramce danych przekazanej
#' argumentem `x` kombinacje wartości *zmiennych grupujących* (a więc wszystkich
#' poza `wskaznik`, `czas` i `wartosc`), dla których kolumna `wskaźnik`
#' przyjmuje wartość podaną argumentem `uzyjWskaznik`, a wartość kolumny `czas`
#' należy do zbioru wartości podanego argumentem `uzyjCzas`.
#' @seealso [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()],
#' [oblicz_wskazniki_pd()], [zanonimizuj_wskazniki_pd()]
#' @importFrom dplyr %>% .data filter mutate
#' @export
dopisz_wskaznik_pd_liczba_abs <- function(x, uzyjWskaznik, uzyjCzas = NA) {
  stopifnot(is.data.frame(x),
            "wskaznik" %in% names(x), "czas" %in% names(x),
            "wartosc" %in% names(x), is.list(x$wartosc),
            is.character(uzyjWskaznik), length(uzyjWskaznik) == 1L,
            !anyNA(uzyjWskaznik), uzyjWskaznik %in% x$wskaznik,
            is.vector(uzyjCzas), length(uzyjCzas) > 0,
            !anyNA(uzyjCzas) | length(uzyjCzas) == 1L)
  lAbs <- x |>
    filter(.data$wskaznik == uzyjWskaznik)
  if (length(uzyjCzas) == 1L & anyNA(uzyjCzas)) {
    uzyjCzas <- unique(lAbs$czas)
  }
  stopifnot("Co najmniej jedna z wartości podanych argumentem `uzyjCzas` nie występuje w zmiennej `czas` (w wierszach, dla których kolumna `wskaznik` przyjmuje wartość podaną argumentem `uzyWskaznik`)." =
              all(uzyjCzas %in% lAbs$czas))
  lAbs <- lAbs |>
    filter(.data$czas %in% uzyjCzas) |>
    mutate(wskaznik = "liczba_abs",
           wartosc = lapply(
             .data$wartosc,
             function(x) {
               do.call(structure,
                       append(list(.Data = c("liczba" = attributes(x)$lAbs)),
                              attributes(x)[c("lAbs", "lSzk")]))
             }))
  return(rbind(x, lAbs))
}
