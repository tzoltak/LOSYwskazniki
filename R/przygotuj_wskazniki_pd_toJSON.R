#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja przekształca wskaźniki obliczone przez funkcję [oblicz_wskazniki_pd()]
#' (a więc również przez [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd_jst()]) do formatu, który będzie przyjazny zapisaniu
#' ich w formacie JSON przy pomocy funkcji `toJSON()` z pakietu *jsonlite*.
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @returns Ramka danych przekazana argumentem `x`, w której wskaźniki zostały
#' przekształcone na zagnieżdżone listy.
#' @export
przygotuj_wskazniki_pd_toJSON <- function(x) {
  stopifnot(is.data.frame(x))
  x <-
    mutate(x,
           across(where(is.list),
                  ~lapply(.,
                          function(x) {
                            a <- attributes(x)
                            a <- a[names(a) %in% c("lAbs", "lSzk",
                                                   "lNieDotyczy",
                                                   "lZadenZWymienionych")]
                            if (length(a) == 0L) return(x)

                            czyPrzeksztalcac <- FALSE
                            if (is(x, "vector")) {
                              if(!is.null(names(x))) {
                                x <- data.frame(label = names(x),
                                                value = unname(x))
                              }
                            } else if (is.array(x)) {
                              x <- as.data.frame(x, responseName = "value")
                              if (ncol(x) == 2L) names(x)[1] <- "label"
                            }
                            return(list(w = x, a = a))
                          })))
  return(x)
}
