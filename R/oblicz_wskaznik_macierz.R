#' @title Obliczanie wskaznikow opisujacych (potencjalnie) wspolwystepujace stany
#' @description
#' Na podstawie tabeli *pośredniej* przekazanej argumentem `x` oblicza wartości
#' wskaźnika na podstawie zmiennej (kolumny) wskazanej argumentem `zm` dla
#' poszczególnych kombinacji wartości pozostałych zmiennych występujących
#' w przekazanej argumentem `x` ramce danych lub - jeśli został przekazany
#' argument `wszystkieObs` - na podstawie kombinacji wartości zmiennych
#' wspólnych dla obiektów przekazanych argumentami `x` i `wszystkieObs`.
#' @param x ramka danych z tabelą *pośrednią* (lub jej odfiltrowanym
#' podzbiorem); musi zawierać kolumny `id_abs` i `rok_abs` oraz kolumnę wskazaną
#' wartością argumentu `zm`
#' @param zm ciąg znaków - nazwa zmiennej w `x`, na podstawie której ma zostać
#' obliczony wskaźnik
#' @param zestawWartosci opcjonalnie ciąg znaków zawierający zestaw wartości,
#' dla których ma zostać obliczony wskaźnik; domyślnie jest to albo zbiór
#' poziomów zmiennej o nazwie `zm` - jeśli jest ona czynnikiem - albo
#' posortowany zbiór jej unikalnych wartości w danych przekazanych argumentem
#' `x` - jeśli nie jest czynnikiem
#' @param wszystkieObs opcjonalnie ramka danych zawierająca wszystkie kombinacje
#' wartości zmiennych identyfikujących *obserwacje*, dla których mają zostać
#' obliczone wartości wskaźnika, nawet jeśli niektóre z nich nie występują
#' w danych przekazanych argumentem `x`; do identyfikacji takich kombinacji
#' zostaną wykorzystane wszystkie zmienne występujące zarówno w obiekcie
#' przekazanym argumentem `x`, jak i w obiekcie przekazanym argumentem
#' `wszystkieObs` (z wyjątkiem zmiennej o nazwie podanej argumentem `zm`)
#' @returns Ramka danych o kolumnach takich jak w `x`, lub - jeśli podano
#' argument `wszystkieObs` - kolumnach występujących zarówno w `x` jak
#' i we `wszystkieObs` oraz kolumnie o nazwie podanej argumentem `zm`, z tym
#' że kolumna `zm` jest macierzą, której kolumny mają nazwy podane argumentem
#' `zestawWartosci`.  Liczba wierszy odpowiada albo liczbie wierszy obiektu
#' przekazanego argumentem `wszystkieObs` albo liczbie unikalnych kombinacji
#' wartości zmiennych nie będących zmienną o nazwie przekazanej argumentem `zm`
#' w obiekcie przekazanym argumentem `x`.
#' @details
#' Funkcja służy do obliczania wskaźników, które opisują kilka stanów mogących
#' współwystępować jednocześnie, np. kontynuacja nauki w różnych formach,
#' czy studiowanie w poszczególnych dziedzinach nauki. Obliczany wskaźnik ma
#' postać macierzy 0 i 1, której kolumny mają nazwy podane argumentem
#' `zestawWartosci`, w której wartość 1 wskazuje na zaistnienie danego zdarzenia
#' a wartość 0 na niezaistnienie.
#' @seealso [dodaj_wskazniki_kontynuacje()]
#' @importFrom stats setNames
#' @importFrom dplyr %>% across all_of distinct everything full_join
#'                   left_join matches mutate select
#' @export
oblicz_wskaznik_macierz <- function(x, zm,
                                    zestawWartosci = unique(c(levels(x[[zm]]),
                                                              sort(unique(x[[zm]])))),
                                    wszystkieObs = NULL) {
  stopifnot(is.data.frame(x), all(c("id_abs", "rok_abs") %in% names(x)),
            !anyNA(x$id_abs), !anyNA(x$rok_abs),
            is.character(zm), length(zm) == 1L, !is.na(zm),
            zm %in% names(x),
            is.null(wszystkieObs) | is.data.frame(wszystkieObs),
            is.character(zestawWartosci), length(zestawWartosci) > 1L,
            !anyNA(zestawWartosci))
  zmDoLaczenia <- setdiff(names(x), zm)
  if (!is.null(wszystkieObs)) {
    stopifnot(all(c("id_abs", "rok_abs") %in% names(wszystkieObs)),
              mode(wszystkieObs$id_abs) == mode(x$id_abs),
              mode(wszystkieObs$rok_abs) == mode(x$rok_abs),
              !anyNA(wszystkieObs$id_abs), !anyNA(wszystkieObs$rok_abs))
    if ("mies_od_ukoncz" %in% names(x)) {
      stopifnot("Jeśli w ramce danych przekazanej argumentem `x` występuje kolumna `mies_od_ukoncz`, to musi też ona występować w ramce danych przekazenj argumentem `wszystkieObs`." =
                  "mies_od_ukoncz" %in% names(wszystkieObs))
    }
    zmDoLaczenia <- intersect(zmDoLaczenia, names(wszystkieObs))
    stopifnot(!any(duplicated(wszystkieObs[, zmDoLaczenia])))
    wszystkieObs <- wszystkieObs %>%
      select(all_of(zmDoLaczenia))
    for (z in zmDoLaczenia) {
      do.call(stopifnot,
              args = setNames(list(mode(wszystkieObs[[z]]) == mode(x[[z]])),
                              paste0("Zmienna `", z,
                                     "` jest różnych typów w obiekcie przekazanym argumentem `x`",
                                     " i w obiekcie przekazanym argumentem `wszystkieObs`.")))
    }
  } else {
    wszystkieObs <- x %>%
      select(all_of(zmDoLaczenia)) %>%
      distinct()
  }

  x <- lapply(zestawWartosci,
                function(w, x) {
                  x[x[[zm]] %in% w, ] %>%
                    distinct() %>%
                    return()
                }, x = x)
  x <- mapply(
    function(x, i, zm) {
      names(x)[names(x) == zm] <- paste(zm, i, sep = ".")
      return(x)
    }, x, seq_along(x), MoreArgs = list(zm = zm), SIMPLIFY = FALSE)
  x <- wszystkieObs %>%
    left_join(Reduce(function(x, y) {return(full_join(x, y, by = zmDoLaczenia))},
                     x, wszystkieObs[c(), ]),
              by = zmDoLaczenia)
  x <- x %>%
    select(all_of(zmDoLaczenia)) %>%
    mutate(wskaznik = x %>%
             select(matches(paste0("^", zm, "\\.", "[[:digit:]]+$"))) %>%
             setNames(zestawWartosci) %>%
             mutate(across(everything(), ~as.integer(!is.na(.)))) %>%
             as.matrix())
  names(x)[names(x) == "wskaznik"] <- zm
  return(x)
}
