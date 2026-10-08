#' @title Przeksztalcanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala usunąć z zestawienia zagregowanych wskaźników (w formacie
#' *długim*) wiersze opisujące takie kombinacje cech absolwentów, które nie
#' wysępowały w danych.
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()], przy czym ta ostatnia musiała zostać wywołana
#' z argumentem `format="długi"`
#' @param usunZanonimizowane wartość logiczna - czy usuwać również wiersze
#' opisujące kombinacje cech absolwentów, które wystąpiły w danych, ale zostały
#' zanonimizowane (z wykorzystaniem [zanonimizuj_wskazniki_pd()]), gdyż liczyły
#' zbyt mało absolwentów
#' @returns Ramka danych przekazana argumentem `x`, z której usunięte zostały
#' wiersze opisujące takie kombinacje cech absolwentów, które nie wysępowały
#' w danych.
#' @export
usun_puste_wskazniki_pd <- function(x, usunZanonimizowane = FALSE) {
  stopifnot(is.data.frame(x),
            all(c("wskaznik", "wartosc") %in% names(x)),
            is.list(x$wartosc),
            all(sapply(x$wartosc, \(x) hasName(attributes(x), "lAbs"))),
            is.logical(usunZanonimizowane), length(usunZanonimizowane) == 1L,
            usunZanonimizowane %in% c(FALSE, TRUE))
  lAbs <- sapply(x$wartosc, \(x) attributes(x)$lAbs)
  if (usunZanonimizowane) {
    return(x[lAbs > 0L & !is.na(lAbs), ])
  } else {
    return(x[lAbs != 0L & !is.na(lAbs), ])
  }
}
