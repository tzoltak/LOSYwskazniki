#' @title Przygotowanie danych do tworzenia wykresu przeplywow
#' @description
#' Funkcja pozwala zagregować dane - domyślnie pochodzące z tabeli *pośredniej*
#' P3 - do postaci, w której mogą one zostac łatwo wykorzystane do przygotowania
#' wykresu przepływów, w szczególności z wykorzystaniem pakietu *ggalluvial*
#' (i korzystającego z tego pakietu szablonu wykresu `wykresPrzeplywyStatusy`,
#' zawartego w pakiecie *LOSYkolory*).
#' @param p3 ramka danych z tabelą *pośrednią* P3 (lub jej odfiltrowanym
#' podzbiorem)
#' @param zm <[tidy-select][dplyr::dplyr_tidy_select]> nazwa kolumny opisującej
#' statusy (pomiędzy którymi może dochodzić do przepływów)
#' @param punktyCzasu wektor wartości, dla których mają być rozpatrywane statusy
#' - domyślnie wartości zmiennej `mies_od_ukoncz`
#' @param zmPunktyCzasu <[tidy-select][dplyr::dplyr_tidy_select]> opcjonalnie
#' nazwa zmiennej zawierającej wartości przekazane argumentem `punktyCzasu`
#' (domyślnie `mies_od_ukoncz`)
#' @returns Ramka danych o kolumnach:
#'
#' -  `idKombinacji` - identyfikator kombinacji (do przypisania *estetyce* `alluvium` w wywołaniach funkcji pakietu *ggalluvial*),
#' -  `<zmPunktyCzasu>` - zmienna wskazana argumentem `zmPunktyCzasu` (do przypisania *estetyce* `x` albo `y` w wywołaniach funkcji pakietu *ggalluvial*),
#' -  `<zm>` - zmienna wskazana argumentem `zm` (do przypisania *estetyce* `stratum` w wywołaniach funkcji pakietu *ggalluvial*),
#' -  `n` - liczba absolwentów o trajektorii opisywanej **daną wartością `idKombinacji`**,
#' -  `pct` - procent absolwentów **w danym punkcie czasu** o statusie opisanym wartością zmiennej `<zm>` (do przypisania *estetyce* `y` albo `x` w wywołaniach funkcji pakietu *ggalluvial*),
#' -  `pctWarstwa` - procent absolwentów o trajektorii opisywanej daną wartością `idKombinacji` **wśród absolwentów o danym statusie w danym punkcie czasu** (do przypisania *estetyce* `weight` w wywołaniach funkcji `stat_flow()` pakietu *ggalluvial* mających na celu pokazanie etykiet wartości poszczególnych przepływów)
#' @details
#' Funkcja może być wykorzystana do przekształcenia również innych danych, niż
#' tabela *pośrednia* P3 (lub jej odfiltrowany podzbiór), z tym ograniczeniem,
#' że oczekuje istnienia w nich zmiennych `id_abs` i `rok_abs`, kombinację
#' wartości których traktuje jako identyfikator absolwenta (jednostki).
#' @examples
#' \dontrun{
#'   przygotuj_dane_przeplywy(p3[p3$rok_abs == 2019, ], status, c(6, 30,54))
#' }
#' @importFrom dplyr %>% .data all_of count filter left_join pull select
#' @importFrom tidyr expand_grid
#' @export
przygotuj_dane_przeplywy <- function(p3, zm, punktyCzasu,
                                     zmPunktyCzasu = "mies_od_ukoncz") {
  stopifnot(is.data.frame(p3),
            all(c("id_abs","rok_abs") %in% names(p3)),
            !("idKombinacji" %in% names(p3)),
            is.vector(punktyCzasu), length(punktyCzasu) > 0L,
            !any(duplicated(punktyCzasu)))
  zm <-
    tryCatch(names(select(p3, {{zm}})),
             error = function(e) stop("W danych przekazanych argumentem `p3` nie ma zmiennej `",
                                      sub("^.*Column `([^']+)` doesn't exist.*$", "\\1", e),
                                      "` (wskazanej argumentem `zm`).",
                                      call. = FALSE))
  stopifnot("Argument `zm` musi wskazywać jedną zmienną." = ncol(zm) == 1L)
  zmPunktyCzasu <-
    tryCatch(names(select(p3, {{zmPunktyCzasu}})),
             error = function(e) stop("W danych przekazanych argumentem `p3` nie ma zmiennej `",
                                      sub("^.*Column `([^']+)` doesn't exist.*$", "\\1", e),
                                      "` (wskazanej argumentem `zmPunktyCzasu`).",
                                      call. = FALSE))
  stopifnot("Argument `zmPunktyCzasu` musi wskazywać jedną zmienną." =
              ncol(zmPunktyCzasu) == 1L,
            "Niektóre wartości podane argumentem `punktyCzasu` nie występują w danych przekazanych argumentem `p3`." =
              all(punktyCzasu %in% select(p3, {{zmPunktyCzasu}})[[1]]))
  p3 <- p3 %>%
    select(all_of(c("id_abs", "rok_abs", zm, zmPunktyCzasu))) %>%
    filter(.data[[zmPunktyCzasu]] %in% punktyCzasu)

  liczbyWierszy <- p3 %>%
    count(across(all_of(c("id_abs", "rok_abs"))),
          name = "n") %>%
    pull("n") %>%
    unique()
  stopifnot("Istnieją absolwenci, dla których liczba wierszy w odfiltrowanych danych jest większa od liczby punktów czasu podanych argumentem `punktyCzasu`." =
              all(liczbyWierszy <= length(punktyCzasu)))
  if (any(liczbyWierszy < length(punktyCzasu))) {
    warning("Istnieją absolwenci, dla których liczba wierszy w odfiltrowanych danych jest mniejsza od liczby punktów czasu podanych argumentem `punktyCzasu`.\n",
            "Brakujące obserwacje zostały wypełnione brakami danych.\n",
            call. = FALSE, immediate. = TRUE)
    p3 <- expand_grid(distinct(select(p3, "id_abs", "rok_abs")),
                      distinct(select(p3, all_of(zmPunktyCzasu)))) %>%
      left_join(p3, by = c("id_abs", "rok_abs", zmPunktyCzasu))
  }

  unikalneWartosci <- sort(unique(p3[[zm]]))
  mapowanie <- seq_along(unikalneWartosci)
  names(mapowanie) <- unikalneWartosci

  p3 <- p3 %>%
    group_by(.data$id_abs, .data$rok_abs) %>%
    mutate(idKombinacji = paste(mapowanie[.data[[zm]]], collapse = ",")) %>%
    ungroup() %>%
    count(across(all_of(c("idKombinacji", zmPunktyCzasu, zm))),
          name = "n") %>%
    group_by(.data[[zmPunktyCzasu]]) %>%
    mutate(pct = .data$n / sum(.data$n)) %>%
    group_by(across(all_of(c(zmPunktyCzasu, zm)))) %>%
    mutate(pctWarstwa = .data$n / sum(.data$n)) %>%
    ungroup()

  unikalneWartosci <- sort(unique(p3$idKombinacji))
  mapowanie <- seq_along(unikalneWartosci)
  names(mapowanie) <- unikalneWartosci
  p3$idKombinacji <- mapowanie[p3$idKombinacji]

  return(p3)
}
