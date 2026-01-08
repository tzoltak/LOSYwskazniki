#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala obliczyć zestawienia zagregowanych wskaźników z danej edycji
#' monitoringu dla każdej JST na podanym poziomie podziału terytorialnego
#' Polski.
#' @inheritParams oblicz_wskazniki_pd_grupy
#' @param poziom opcjonalnie ciąg znaków - poziom podziału terytorialnego, na
#' jakim mają zostać obliczone wskaźniki: "Polska", "wojewodztwa" lub "powiaty";
#' domyślnie "Polska"
#' @param wyswietlPostep opcjonalnie liczba 0, 1, albo 2 wskazująca, czy i jak
#' szczegółowo ma być raportowany postęp obliczeń: 0 oznacza niepokazywanie
#' żadnej informacji o postępie, 1 i 2 oznaczają rosnącą szczegółowość
#' wyświetlanych informacji (ich zakres zależy też od wartości argumentu
#' `poziom`); domyślnie 1
#' @returns Ramka danych z kolumnami:
#' -    `rok_abs`,
#' -    w zależności od wartości argumentu `poziom`:
#'      -   `obszar` (kolumna tekstowa przyjmująca wartość "Polska")
#'      -   albo `teryt_woj_szk` i `nazwa_woj_szk`,
#'      -   albo `teryt_pow_szk` i `nazwa_pow_szk`,
#' -    kolumnami podanymi argumentem `zmGrupujace`, przekształconymi
#'      w czynniki i - z wyjątkiem kolumn podanych argumentem `zmBezOgolem` -
#'      z dodaną dodatkową wartością "Ogółem",
#' -    `wskaznik` - kolumna tekstowa opisująca wskaźnik,
#' -    `czas` - kolumna tekstowa opisująca czas, jaki opisuje wskaźnik, co do
#'      zasady miesiąc od ukończenia szkoły ponadpodstawowej (jeśli jest to
#'      liczba) lub skrótowiec opisujący kilka miesięcy (np. "r0_ivkw" -
#'      IV kwartał roku ukończenia szkoły ponadpodstawowej),
#' -    `wartosc` - kolumna-lista zawierająca wartości wskaźników
#'      (p. [oblicz_wskazniki_pd()])
#' @seealso [oblicz_wskazniki_pd_grupy()], [oblicz_wskazniki_pd()],
#' [zanonimizuj_wskazniki_pd()]
#' @importFrom dplyr %>% anti_join distinct mutate select semi_join
#' @importFrom tidyr unnest
#' @export
oblicz_wskazniki_pd_jst <- function(p4, p3,
                                    poziom = c("Polska", "wojewodztwa", "powiaty"),
                                    zmGrupujace = vector(mode = "character", length = 0),
                                    zmBezOgolem = "typ_szk",
                                    zmTylkoWartosciWdanych =
                                      list("typ_szk" = c("kod_zaw", "nazwa_zaw")),
                                    zmWskaznikiP4 = c("dyplom_zaw", "matura_zdana",
                                                      "typ_szk_kont6", "typ_szk_kont18",
                                                      "dyscyplina_kont6", "dyscyplina_kont18",
                                                      "sr_wynagr_r1"),
                                    zmWskaznikiP3 = c("status", "bezrobocie"),
                                    statystyki = list("średnia" = ~mean(., na.rm = TRUE)),
                                    wskTylkoNiezerowe = c("dziedzina_kont", "dyscyplina_kont"),
                                    etykietaBrakDanych = "Ndt.",
                                    etykietaOgolem = "Ogółem",
                                    wyswietlPostep = 1) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p3)),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "id_szk") %in% names(p4)),
            is.numeric(wyswietlPostep), length(wyswietlPostep) == 1,
            wyswietlPostep %in% c(0, 1, 2))
  poziom <- match.arg(poziom)
  if (nrow(anti_join(p4, p3,
                     by = c("id_abs", "rok_abs"))) > 0) {
    warning("Niektóre kombinacje wartości (`id_abs`, `rok_abs`) występujące w ramce danych przekazanej argumentem `p4` nie występują w ramce danych przekazanej argumentem `p3`.",
            call. = FALSE, immediate. = TRUE)
  }

  if (poziom == "wojewodztwa") {
    stopifnot(all(c("teryt_woj_szk", "nazwa_woj_szk") %in% names(p4)))
    matryca <- p4 %>%
      select("rok_abs", "teryt_woj_szk", "nazwa_woj_szk") %>%
      distinct()
  } else if (poziom == "powiaty") {
    stopifnot(all(c("teryt_woj_szk", "nazwa_woj_szk") %in% names(p4)))
    matryca <- p4 %>%
      select("rok_abs", "teryt_pow_szk", "nazwa_pow_szk") %>%
      distinct()
  } else {
    matryca <- p4 %>%
      select("rok_abs") %>%
      distinct() %>%
      mutate(obszar = "Polska")
  }

  matryca$wskazniki <- vector(mode = "list", length = nrow(matryca))
  for (i in seq_len(nrow(matryca))) {
    if (wyswietlPostep > 0) {
      message(i, ". z ", nrow(matryca), ": ",
              paste(unlist(matryca[i, names(matryca) != "wskazniki"]),
                    collapse = ", "))
    }
    p4Temp <- p4 %>%
      semi_join(matryca[i, ],
                by = intersect(names(p4), names(matryca)))
    matryca$wskazniki[[i]] <-
      oblicz_wskazniki_pd_grupy(p4 = p4Temp,
                                p3 = semi_join(p3, p4Temp,
                                               by = c("id_abs", "rok_abs")),
                                zmGrupujace = zmGrupujace,
                                zmBezOgolem = zmBezOgolem,
                                zmTylkoWartosciWdanych = zmTylkoWartosciWdanych,
                                zmWskaznikiP4 = zmWskaznikiP4,
                                zmWskaznikiP3 = zmWskaznikiP3,
                                statystyki = statystyki,
                                wskTylkoNiezerowe = wskTylkoNiezerowe,
                                etykietaBrakDanych = etykietaBrakDanych,
                                etykietaOgolem = etykietaOgolem,
                                wyswietlPostep =
                                  (wyswietlPostep > 0 &
                                     poziom %in% c("Polska", "wojewodztwa")) |
                                  wyswietlPostep > 1)
  }
  return(unnest(matryca, "wskazniki"))
}
