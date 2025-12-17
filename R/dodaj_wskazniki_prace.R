#' @title Obliczanie wskaznikow wykorzystywanych w raportach
#' @description
#' Na podstawie tabeli *pośredniej* P3 funkcja oblicza wskaźniki opisujące
#' średnią wysokość wynagrodzeń oraz długość posiadania pracy lub bycia
#' bezrobotnym (w wybranych okresach).
#' @param p4 ramka danych z tabelą *pośrednią* P4 (lub jej odfiltrowanym
#' podzbiorem), musi zawierać co najmniej kolumny `id_abs` i `rok_abs`);
#' jeśli nie ma potrzeby dołączania obliczonych wskaźników do tabeli
#' *pośredniej* P4, można ustawić wartość tego argumentu na `NULL`
#' @param p3 ramka danych z tabelą *pośrednią* P3 (lub jej odfiltrowanym
#' podzbiorem)
#' @returns Ramka danych przekazana argumentem `p4` (lub - jeśli argument `p4`
#' został ustawiony na `NULL` - ramka danych z unikalnymi kombinacjami wartości
#' zmiennych (`id_abs`, `rok_abs`) występującym w ramce danych przekazanej
#' argumentem `p3`) z dodanymi kolumnami:
#'
#' -    `sr_wynagr_r0_ivkw` - średnie wynagrodzenia w IV kw. roku ukończenia szkoły,
#' -    `sr_wynagr_r1` - średnie wynagrodzenia w roku następującym po roku ukończenia szkoły,
#' -    `sr_wynagr_r1_ikw` - średnie wynagrodzenia w I kw. roku następującym po roku ukończenia szkoły,
#' -    `sr_wynagr_r2_ikw` - średnie wynagrodzenia w I kw. roku następującego w dwa lata po roku ukończenia szkoły,
#' -    `sr_wynagr_r2` - średnie wynagrodzenia w roku następującym w dwa lata po roku ukończenia szkoły,
#' -    `sr_wynagr_r3` - średnie wynagrodzenia w roku następującym w trzy lata po roku ukończenia szkoły,
#' -    `sr_wynagr_r4` - średnie wynagrodzenia w roku następującym w cztery lata po roku ukończenia szkoły,
#' -    `sr_wynagr_uop_nauka_r0_wrzgru` - średnie relatywne (odniesione do średnich miesięcznych wynagrodzeń w powiecie w danym roku) wynagrodzenia z umów o pracę od września do grudnia roku ukończenia szkoły w miesiącach kontynuowania nauki,
#' -    `sr_wynagr_uop_bez_nauki_r0_wrzgru` - średnie relatywne (odniesione do średnich miesięcznych wynagrodzeń w powiecie w danym roku) wynagrodzenia z umów o pracę od września do grudnia roku ukończenia szkoły w miesiącach niekontynuowania nauki,
#' -    `praca_nauka_r0_wrzgru` - długość zatrudnienia w okresie kontynuowania nauki między wrześniem a grudniem roku ukończenia szkoły,
#' -    `praca_bez_nauki_r0_wrzgru` - długość zatrudnienia w okresie bez kontynuowania nauki między wrześniem a grudniem roku ukończenia szkoły,
#' -    `bezrobocie_r0_wrzgru` - długość bycia zarejestrowanym bezrobotnym od września do grudnia roku ukończenia szkoły.
#' @details
#' W porównaniu do pozostałych funkcji `dodaj_[...]()` działanie tej jest
#' najmniej elastyczne, ze zdefiniowanymi na stałe okresami, dla których
#' obliczane są poszczególne wskaźniki. Zasadniczo jest ona przeznaczona do
#' użycia w ramach procesu przygotowywania tabel *pośrednich* przez
#' [MLASdaneAdm::przygotuj_tabele_posrednie()] i późniejsze jej użycie jest
#' bezcelowe.
#' @seealso [oblicz_wskaznik_z_p3()] - *koń roboczy* odpowiedzialny za agregację
#' wskaźników w ramach `dodaj_wskazniki_prace()`
#' @examples
#' \dontrun{
#'   p4 <- dodaj_wskazniki_prace(p4, p3)
#'   str(p4)
#'   p3agr <- dodaj_wskazniki_prace(NULL, p3)
#'   str(p3agr)
#' }
#' @importFrom dplyr %>% .data across case_match distinct filter full_join
#'                  left_join mutate select semi_join
#' @export
dodaj_wskazniki_prace <- function(p4, p3) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz", "wynagrodzenie",
                  "wynagrodzenie_uop", "powiat_sr_wynagrodzenie",
                  "praca", "nauka2", "bezrobocie") %in% names(p3)),
            !anyNA(p3$id_abs), !anyNA(p3$rok_abs), !anyNA(p3$mies_od_ukoncz),
            is.null(p4) | is.data.frame(p4))
  if (!is.null(p4)) {
    stopifnot(all(c("id_abs", "rok_abs") %in% names(p4)),
              mode(p3$id_abs) == mode(p4$id_abs),
              mode(p3$rok_abs) == mode(p4$rok_abs),
              !anyNA(p4$id_abs), !anyNA(p4$rok_abs))
    wszystkieObs <- p4 %>%
      select("id_abs", "rok_abs") %>%
      distinct()
  } else {
    p4 <- p3 %>%
      select("id_abs", "rok_abs") %>%
      distinct()
    wszystkieObs <- p4
  }
  p3 <- p3 %>%
    semi_join(wszystkieObs, by = c("id_abs", "rok_abs"))
  zmienneDoUsuniecia <- intersect(c("sr_wynagr_r0_ivkw",
                                    "sr_wynagr_r1_ikw", "sr_wynagr_r2_ikw",
                                    "sr_wynagr_r1", "sr_wynagr_r2",
                                    "sr_wynagr_r3", "sr_wynagr_r4",
                                    "sr_wynagr_uop_nauka_r0_wrzgru",
                                    "sr_wynagr_uop_bez_nauki_r0_wrzgru",
                                    "praca_nauka_r0_wrzgru",
                                    "praca_bez_nauki_r0_wrzgru",
                                    "bezrobocie_r0_wrzgru"), names(p4))
  if (length(zmienneDoUsuniecia) > 0L) {
    warning("Zmienne: `", paste(zmienneDoUsuniecia, collapse = "`, `"),
            ", które już istnieją w danych przekazanych argumentem `p4` zostaną usunięte i będą utworzone na nowo.",
            immediate. = TRUE)
    p4 <- select(p4, -all_of(zmienneDoUsuniecia))
  }

  wynagrodzenia <- list(
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (4L:6L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r0_ivkw = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r0_ivkw", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (7L:18L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r1 = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r1", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (7L:9L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r1_ikw = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r1_ikw", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (19L:21L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r2_ikw = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r2_ikw", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (19L:30L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r2 = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r2", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (31L:42L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r3 = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r3", fun = mean, pomijaj0 = TRUE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (43L:54L)) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             sr_wynagr_r4 = "wynagrodzenie") %>%
      oblicz_wskaznik_z_p3("sr_wynagr_r4", fun = mean, pomijaj0 = TRUE),
    oblicz_wynagrodzenia_wzgledne(p3, miesOdUkoncz = 3L:6L, nauka2 = 1L,
                                  nazwaWsk = "sr_wynagr_uop_nauka_r0_wrzgru"),
    oblicz_wynagrodzenia_wzgledne(p3, miesOdUkoncz = 3L:6L, nauka2 = 0L,
                                  nazwaWsk = "sr_wynagr_uop_bez_nauki_r0_wrzgru")
  )
  wynagrodzenia <-
    Reduce(function(x, y) {return(left_join(x, y,
                                            by = c("id_abs", "rok_abs")))},
           wynagrodzenia, wszystkieObs)

  zatrudnienie <- full_join(
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (3L:6L),
             .data$nauka2 == 1L) %>%
      mutate(praca_nauka_r0_wrzgru =
               as.numeric(.data$praca > 0 & !is.na(.data$praca))) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz", "praca_nauka_r0_wrzgru") %>%
      oblicz_wskaznik_z_p3("praca_nauka_r0_wrzgru",
                           fun = mean, pomijaj0 = FALSE),
    p3 %>%
      filter(.data$mies_od_ukoncz %in% (3L:6L),
             .data$nauka2 == 0L) %>%
      mutate(praca_bez_nauki_r0_wrzgru =
               as.numeric(.data$praca > 0 & !is.na(.data$praca))) %>%
      select("id_abs", "rok_abs", "mies_od_ukoncz",
             "praca_bez_nauki_r0_wrzgru") %>%
      oblicz_wskaznik_z_p3("praca_bez_nauki_r0_wrzgru",
                           fun = mean, pomijaj0 = FALSE),
    by = c("id_abs", "rok_abs")
  ) %>%
    mutate(across(c("praca_nauka_r0_wrzgru", "praca_bez_nauki_r0_wrzgru"),
                  ~factor(case_match(.,
                                     0 ~ "Brak pracy",
                                     1 ~ "Praca przez cały okres",
                                     .default = "Praca przez część okresu"),
                          c("Brak pracy", "Praca przez część okresu",
                            "Praca przez cały okres"))))

  bezrobocie <- p3 %>%
    filter(.data$mies_od_ukoncz %in% (3L:6L)) %>%
    mutate(bezrobocie_r0_wrzgru = as.integer(.data$bezrobocie %in% 1L)) %>%
    select("id_abs", "rok_abs", "mies_od_ukoncz", "bezrobocie_r0_wrzgru") %>%
    oblicz_wskaznik_z_p3("bezrobocie_r0_wrzgru", fun = sum, pomijaj0 = FALSE) %>%
    mutate(bezrobocie_r0_wrzgru = factor(.data$bezrobocie_r0_wrzgru, 0L:4L,
                                         c("Brak bezrobocia", "1 miesiąc",
                                           paste(2:4, "miesiące"))))

  p4 <- p4 %>%
    left_join(wynagrodzenia,
              by = c("id_abs", "rok_abs")) %>%
    left_join(zatrudnienie,
              by = c("id_abs", "rok_abs")) %>%
    left_join(bezrobocie,
              by = c("id_abs", "rok_abs"))
  return(p4)
}
