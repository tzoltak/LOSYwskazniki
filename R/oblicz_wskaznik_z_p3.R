#' @title Agregowanie wskaznikow po miesiacach w ramach absolwentow
#' @description
#' Na podstawie tabeli *pośredniej* przekazanej argumentem `x` oblicza wartości
#' wskaźnika na podstawie zmiennej (kolumny) wskazanej argumentem `zm` dla
#' poszczególnych kombinacji wartości pozostałych zmiennych występujących
#' w przekazanej argumentem `x` ramce danych lub - jeśli został przekazany
#' argument `wszystkieObs` - na podstawie kombinacji wartości zmiennych
#' wspólnych dla obiektów przekazanych argumentami `x` i `wszystkieObs`.
#' @param p3 ramka danych z tabelą *pośrednią* P3 (lub jej odfiltrowanym
#' podzbiorem); musi zawierać kolumny `id_abs` i `rok_abs` oraz kolumnę wskazaną
#' wartością argumentu `zm`
#' @param zm ciąg znaków - nazwa zmiennej w `x`, która ma podlegać agregacji
#' po czasie w ramach absolwentów (obserwacji)
#' @param wszystkieObs opcjonalnie ramka danych zawierająca wszystkie kombinacje
#' wartości zmiennych identyfikujących *obserwacje*, dla których mają zostać
#' obliczone wartości wskaźnika, nawet jeśli niektóre z nich nie występują
#' w danych przekazanych argumentem `p3`; do identyfikacji takich kombinacji
#' zostaną wykorzystane wszystkie zmienne występujące zarówno w obiekcie
#' przekazanym argumentem `p3`, jak i w obiekcie przekazanym argumentem
#' `wszystkieObs` (z wyjątkiem zmiennej o nazwie podanej argumentem `zm`)
#' @param fun funkcja lub jednostronna formuła z wyrażeniem, które ma zostać
#' użyte do agregacji wartości zmiennej o nazwie `zm`; ponieważ braki danych są
#' przez `oblicz_wskaznik_z_p3()` usuwane z danych przed przekazaniem do
#' agregacji, nie jest więc konieczne zadawanie ich obsługi w ramach tego
#' argumentu
#' @param pomijaj0 opcjonalnie wartość logiczna - czy z agregacji powinny zostać
#' wyłączone wartości zmiennej o nazwie `zm` równe 0? domyślnie `TRUE` przy
#' agregacji wynagrodzeń i `FALSE` w pozostałych przypadkach
#' @returns Ramka danych o kolumnach takich jak w `p3`, lub - jeśli podano
#' argument `wszystkieObs` - kolumnach występujących zarówno w `p3` jak
#' i we `wszystkieObs` oraz kolumnie o nazwie podanej argumentem `zm`. Liczba
#' wierszy odpowiada albo liczbie wierszy obiektu przekazanego argumentem
#' `wszystkieObs` albo liczbie unikalnych kombinacji wartości zmiennych
#' (`id_abs`, `rok_abs`) w obiekcie przekazanym argumentem `p3`.
#' @details
#' Funkcja służy do agregowania wskaźników po czasie w ramach absolwentów,
#' np. wskaźnika wynagrodzeń (przychodów). Funkcja zakłada, że dane przekazane
#' argumentem `p3` zostały już zawężone do odpowiedniego okresu i że różne
#' wiersze w ramach danej kombinacji wartości zmiennych (`id_abs`, `rok_abs`)
#' reprezentują poszczególne miesiące.
#' @seealso [dodaj_wskazniki_prace()]
#' @importFrom stats setNames
#' @importFrom dplyr %>% across all_of distinct group_by select summarise
#' @export
oblicz_wskaznik_z_p3 <- function(p3, zm, wszystkieObs = NULL, fun = mean,
                                 pomijaj0 = grepl("wynagrodzenie", zm)) {
  stopifnot(is.data.frame(p3), all(c("id_abs", "rok_abs") %in% names(p3)),
            !anyNA(p3$id_abs), !anyNA(p3$rok_abs),
            is.character(zm), length(zm) == 1L, !is.na(zm),
            zm %in% names(p3),
            is.null(wszystkieObs) | is.data.frame(wszystkieObs),
            inherits(fun, "formula") | is.function(fun),
            is.logical(pomijaj0), length(pomijaj0) == 1L, !is.na(pomijaj0))
  if ("mies_od_ukoncz" %in% names(p3)) {
    stopifnot(!anyNA(p3$mies_od_ukoncz),
              "W obiekcie przekazanym argumentem `p3` występują duplikaty kombinacji wartości zmiennych (`id_abs`, `rok_abs`, `mies_od_ukoncz`)" =
                nrow(p3) == nrow(distinct(p3[, c("id_abs", "rok_abs", "mies_od_ukoncz")])))
  } else if ("okres" %in% names(p3)) {
    stopifnot(!anyNA(p3$okres),
              "W obiekcie przekazanym argumentem `p3` występują duplikaty kombinacji wartości zmiennych (`id_abs`, `rok_abs`, `okres`)" =
                nrow(p3) == nrow(distinct(p3[, c("id_abs", "rok_abs", "okres")])))
  } else if (all(c("rok", "miesac")) %in% names(p3)) {
    stopifnot(!anyNA(p3$rok), !anyNA(p3$miesiac),
              "W obiekcie przekazanym argumentem `p3` występują duplikaty kombinacji wartości zmiennych (`id_abs`, `rok_abs`, `rok`, `miesiac`)" =
                nrow(p3) == nrow(distinct(p3[, c("id_abs", "rok_abs", "rok", "miesiac")])))
  }
  zmDoLaczenia <- setdiff(names(p3),
                          c(zm, "okres", "rok", "miesiac", "mies_od_ukoncz"))
  if (!is.null(wszystkieObs)) {
    stopifnot(all(c("id_abs", "rok_abs") %in% names(wszystkieObs)),
              mode(wszystkieObs$id_abs) == mode(p3$id_abs),
              mode(wszystkieObs$rok_abs) == mode(p3$rok_abs),
              !anyNA(wszystkieObs$id_abs), !anyNA(wszystkieObs$rok_abs))
    zmDoLaczenia <- intersect(zmDoLaczenia, names(wszystkieObs))
    stopifnot(!any(duplicated(wszystkieObs[, zmDoLaczenia])))
    wszystkieObs <- wszystkieObs %>%
      select(all_of(zmDoLaczenia))
    for (z in zmDoLaczenia) {
      do.call(stopifnot,
              args = setNames(list(mode(wszystkieObs[[z]]) == mode(p3[[z]])),
                              paste0("Zmienna `", z,
                                     "` jest różnych typów w obiekcie przekazanym argumentem `x`",
                                     " i w obiekcie przekazanym argumentem `wszystkieObs`.")))
    }
  } else {
    wszystkieObs <- p3 %>%
      select(all_of(zmDoLaczenia)) %>%
      distinct()
  }
  p3 <- p3 %>%
    filter(!is.na(.data[[zm]]))
  if (pomijaj0) {
    p3 <- p3 %>%
      filter(.data[[zm]] != 0)
  }
  p3 <- p3 %>%
    group_by(across(all_of(zmDoLaczenia))) %>%
    summarise(across(all_of(zm), fun),
              .groups = "drop")

  p3 <- wszystkieObs %>%
    left_join(p3,
              by = zmDoLaczenia)
  return(p3)
}
