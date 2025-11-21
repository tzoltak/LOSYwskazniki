#' @title Obliczanie wzglednych wskaznikow wynagrodzen
#' @description
#' Funkcja oblicza wartości wskaźnika względnego wynagrodzeń za podany okres
#' (i ew. biorąc pod uwagę wyłącznie miesiące nauki albo bez nauki) na poziomie
#' poszczególnych absolwentów, na podstawie danych z tabeli *pośredniej* P3.
#' @param p3 ramka danych z tabelą *pośrednią* P3 (lub jej odfiltrowanym
#' podzbiorem)
#' @param miesOdUkoncz wektor liczbowy podający wartości zmiennej
#' `miesOdUkoncz`, które mają być uwzględnione przy obliczaniu wartości
#' wskaźnika
#' @param nazwaWsk opcjonalnie ciąg znaków z nazwą kolumny, w której mają się
#' znaleźć obliczone wartości wskaźnika względynych wynagrodzeń (domyślnie
#' "sr_wynagr_uop")
#' @param nauka2 opcjonalnie pojedyncza wartość liczbowa opisująca, czy dla
#' danego absolwenta mają być uwzględnione tylko miesiące, w których kontynuował
#' on naukę (`nauka2 = 1`) albo tylko miesiące, w których nie kontynuował on
#' nauki (`nauka2 = 0`); domyślna wartość (`NA`) wskazuje, że mają zostać
#' uwzględnione wszystkie miesiące
#' @returns Ramka danych o kolumnach takich jak w `p3` z dodaną kolumną o nazwie
#' podanej argumentem `nazwaWsk`. Liczba wierszy odpowiada liczbie unikalnych
#' kombinacji wartości zmiennych (`id_abs`, `rok_abs`) w obiekcie przekazanym
#' argumentem `p3`.
#' @details
#' Względne wynagrodzenia obliczane są jako iloraz
#' `wynagrodzenie_uop / powiat_sr_wynagrodzenie`, a następnie przekazywane do
#' funkcji [oblicz_wskaznik_z_p3()] w celu dokonania agregacji.
#'
#' Ocena, czy absolwent kontynuował naukę dokonywana jest na podstawie zmiennej
#' `nauka2`, co oznacza, że uczestnictwo w KKZ lub KUZ **nie** jest traktowane
#' jako kontynuacja nauki (w przypadku KKZ - o ile nie wiąże się z nauką w BS II).
#'
#' Funkcja została wyodrębiona w dużej mierze po to, aby mogła być użyta
#' również w ramach pakietu *MLASdaneAdm*, wewnątrz funkcji
#' [MLASdaneAdm::uzupelnij_wzgledne_wynagrodzenia()].
#' @importFrom stats setNames
#' @importFrom dplyr %>% .data filter
#' @export
oblicz_wynagrodzenia_wzgledne <- function(p3, miesOdUkoncz,
                                          nazwaWsk = "sr_wynagr_uop",
                                          nauka2 = NA) {
  stopifnot(is.data.frame(p3),
            all(c("mies_od_ukoncz", "nauka2", "wynagrodzenie_uop", "teryt_zam",
                  "powiat_sr_wynagrodzenie") %in% names(p3)),
            is.numeric(miesOdUkoncz), length(miesOdUkoncz) > 0L,
            !anyNA(miesOdUkoncz), all(as.integer(miesOdUkoncz) == miesOdUkoncz),
            is.character(nazwaWsk), length(nazwaWsk) == 1L,
            !anyNA(nazwaWsk),
            is.numeric(nauka2), length(nauka2) == 1L, nauka2 %in% c(0L, 1L))
  if (!is.na(nauka2)) {
    p3 <- p3[p3$nauka2 %in% nauka2, ]
  }
  p3 <- p3 %>%
    filter(.data$mies_od_ukoncz %in% miesOdUkoncz)
  p3 <- do.call(cbind,
                setNames(list(p3[, c("id_abs", "rok_abs", "mies_od_ukoncz")],
                              p3$wynagrodzenie_uop / p3$powiat_sr_wynagrodzenie),
                         c("", nazwaWsk)))
  return(oblicz_wskaznik_z_p3(p3, nazwaWsk, fun = mean, pomijaj0 = TRUE))
}
