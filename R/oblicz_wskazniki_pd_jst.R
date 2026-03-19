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
#' `poziom`, `liczbaWatkowGrupy` - zrównoleglanie na tym poziomie
#' uniemożliwia raportowanie postępu na poziomie wyższym niż 1 - oraz
#' `liczbaWatkow` - zrównoleglanie na tym poziomie uniemożliwia raportowanie
#' postępu); domyślnie 1
#' @param liczbaWatkow opcjonalnie dodatnia liczba całkowita - liczba wątków,
#' w których ma być wywoływana funkcja [oblicz_wskazniki_pd_grupy()], p. sekcja
#' *details* poniżej; domyślnie 1, tj. bez zrównoleglania
#' @param liczbaWatkowGrupy opcjonalnie dodatnia liczba całkowita, przekazywana
#' do wywołań [oblicz_wskazniki_pd_grupy()] - liczba wątków, w których
#' [oblicz_wskazniki_pd_grupy()] ma wywoływać [oblicz_wskazniki_pd()], p. sekcja
#' *details* poniżej; domyślnie 1, tj. bez zrównoleglania w ramach
#' [oblicz_wskazniki_pd_grupy()]
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
#' @details
#' **Wielowątkowość:** rozsądny dobór wartości parametrów `liczbaWatkow`
#' i `liczbaWatkowGrupy` wyraźnie zależy od wartości argumentu `poziom`:
#'
#' -  Przy `poziom = "Polska"` nie ma możliwości zrównoleglania na poziomie JST,
#'    więc wartości `liczbaWatkow` większe od 1 i tak zostaną zignorowane.
#'    Wszystkie wątki należy więc przypisać do poziomu grup, podając ich liczbę
#'    argumentem `liczbaWatkowGrupy`. Testy empiryczne (p. [czasy_wskazniki_pd])
#'    wykazały jednak, że odczuwalne skrócenie czasu obliczeń osiąga się dla
#'    2 lub 3 równoległych wątków; zysk z dalszego zwiększania ich liczby jest
#'    już bardzo niewielki.
#'  - Przy `poziom = "wojewodztwa"` optymalne wyniki (na 12-rdzeniowym
#'    procesorze) osiągnięto w wywołaniach, w których `liczbaWatkowGrupy` była
#'    ustawione w przedziale od 2 do 3, a `liczbaWatkowJST` na 4. Wydaje się, że
#'    dalsze zwiększanie wartości argumentów poza `liczbaWatkowJST = 4`
#'    i `liczbaWatkowGrupy = 2` może przynieść już tylko niewielkie zyski.
#'  - Przy `poziom = "powiaty"` nie odnotowuje się skrócenia czasu obliczeń
#'    w wyniku zwiększania wartości `liczbaWatkowGrupy` i powinna ona być w tym
#'    przypadku ustawiana na 1, a wszystkie dostępne wątki należy przypisać
#'    do zrównoleglania na poziomie JST - w przedziale do 11 wątków nie
#'    odnotowano tu jeszcze wyraźnego wypłaszczenia, choć oczywiście krańcowy
#'    zysk jest malejący (i to wyraźnie szybciej, niż odwrotność liczby wątków).
#' @seealso [oblicz_wskazniki_pd_grupy()], [oblicz_wskazniki_pd()],
#' [dopisz_wskaznik_pd_liczba_abs()], [zanonimizuj_wskazniki_pd()],
#' [uruchom_oblicz_wskazniki_pd_grupy()] [czasy_wskazniki_pd]
#' @importFrom dplyr %>% anti_join distinct mutate select semi_join
#' @importFrom tidyr unnest
#' @export
oblicz_wskazniki_pd_jst <- function(p4, p3,
                                    poziom = c("Polska", "wojewodztwa", "powiaty"),
                                    zmGrupujace = vector(mode = "character", length = 0),
                                    zmBezOgolem = "typ_szk",
                                    zmTylkoWartosciWDanych =
                                      list("typ_szk" = c("kod_zaw", "nazwa_zaw"),
                                           "typ_szk" = c("mlodoc")),
                                    zmWskaznikiP4 = c("dyplom_zaw", "matura_zdana",
                                                      "typ_szk_kont6", "typ_szk_kont18",
                                                      "dyscyplina_kont6", "dyscyplina_kont18",
                                                      "sr_wynagr_r1"),
                                    zmWskaznikiP3 = c("status", "bezrobocie"),
                                    statystyki = list("średnia" = ~mean(., na.rm = TRUE)),
                                    wskTylkoNiezerowe = c("dziedzina_kont", "dyscyplina_kont"),
                                    etykietaBrakDanych = "Ndt.",
                                    etykietaOgolem = "Ogółem",
                                    wyswietlPostep = 1,
                                    liczbaWatkow = 1L, liczbaWatkowGrupy = 1L) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p3)),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "id_szk") %in% names(p4)),
            is.numeric(wyswietlPostep), length(wyswietlPostep) == 1,
            wyswietlPostep %in% c(0, 1, 2),
            is.numeric(liczbaWatkow), length(liczbaWatkow) == 1L,
            !anyNA(liczbaWatkow), is.finite(liczbaWatkow),
            liczbaWatkow == as.integer(liczbaWatkow), liczbaWatkow > 0L)
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
  if (liczbaWatkow > 1L & nrow(matryca) > 1L) {
    cl <- parallel::makeCluster(min(c(liczbaWatkow,
                                      parallel::detectCores() - 1L,
                                      nrow(matryca))))
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(LOSYwskazniki)
    })
    matryca <- split(matryca, seq_len(nrow(matryca)))
    matryca <- parallel::parLapply(cl, matryca,
                                   uruchom_oblicz_wskazniki_pd_grupy,
                                   p4 = p4, p3, zmGrupujace = zmGrupujace,
                                   zmBezOgolem = zmBezOgolem,
                                   zmTylkoWartosciWDanych = zmTylkoWartosciWDanych,
                                   zmWskaznikiP4 = zmWskaznikiP4,
                                   zmWskaznikiP3 = zmWskaznikiP3,
                                   statystyki = statystyki,
                                   wskTylkoNiezerowe = wskTylkoNiezerowe,
                                   etykietaBrakDanych = etykietaBrakDanych,
                                   etykietaOgolem = etykietaOgolem,
                                   wyswietlPostep = FALSE,
                                   liczbaWatkow = liczbaWatkowGrupy) %>%
      bind_rows()
    parallel::stopCluster(cl)
  } else {
    for (i in seq_len(nrow(matryca))) {
      if (wyswietlPostep > 0) {
        message(i, ". z ", nrow(matryca), ": ",
                paste(unlist(matryca[i, names(matryca) != "wskazniki"]),
                      collapse = ", "))
      }
      matryca$wskazniki[[i]] <-
        uruchom_oblicz_wskazniki_pd_grupy(
          matryca[i, ], p4 = p4, p3 = p3, zmGrupujace = zmGrupujace,
          zmBezOgolem = zmBezOgolem, zmTylkoWartosciWDanych = zmTylkoWartosciWDanych,
          zmWskaznikiP4 = zmWskaznikiP4, zmWskaznikiP3 = zmWskaznikiP3,
          statystyki = statystyki, wskTylkoNiezerowe = wskTylkoNiezerowe,
          etykietaBrakDanych = etykietaBrakDanych, etykietaOgolem = etykietaOgolem,
          wyswietlPostep = (wyswietlPostep > 0 &
                              poziom %in% c("Polska", "wojewodztwa")) |
            wyswietlPostep > 1,
          liczbaWatkow = liczbaWatkowGrupy)$wskazniki[[1]]
    }
  }
  return(unnest(matryca, "wskazniki"))
}
#' @title Nieeksportowane funkcje wykorzystywane w agregowaniu wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Dla zadanej kombinacji zmiennych grupujących funkcja odpowiednio zawęża dane
#' i wywołuje na nich [oblicz_wskazniki_pd_grupy()]. Wykorzystywana wewnątrz
#' [oblicz_wskazniki_pd_jst()].
#' @inheritParams oblicz_wskazniki_pd_grupy
#' @inheritParams oblicz_wskazniki_pd_jst
#' @param wierszMatrycy ramka danych o jednym wierszu, zawierająca kolumny
#' opisujące zmienne opisujące podział na JST oraz kolumnę-listę `wskazniki`,
#' w której zostaną umieszczone wyniki wywołania
#' [oblicz_wskazniki_pd_grupy()].
#' @returns `wierszMatrycy` z wypełnioną kolumną `wskazniki`.
#' @details
#' Funkcja została wyodrębniona na potrzeby zrównoleglania wywoływania
#' [oblicz_wskazniki_pd_grupy()] wewnątrz [oblicz_wskazniki_pd_jst()].
#' @importFrom dplyr semi_join
uruchom_oblicz_wskazniki_pd_grupy <- function(wierszMatrycy, p4, p3,
                                              zmGrupujace, zmBezOgolem,
                                              zmTylkoWartosciWDanych,
                                              zmWskaznikiP4, zmWskaznikiP3,
                                              statystyki, wskTylkoNiezerowe,
                                              etykietaBrakDanych,
                                              etykietaOgolem, wyswietlPostep,
                                              liczbaWatkow) {
  p4Temp <- p4 %>%
    semi_join(wierszMatrycy,
              by = intersect(names(p4), names(wierszMatrycy)))
  wierszMatrycy$wskazniki[[1]] <-
    oblicz_wskazniki_pd_grupy(p4 = p4Temp,
                              p3 = semi_join(p3, p4Temp,
                                             by = c("id_abs", "rok_abs")),
                              zmGrupujace = zmGrupujace,
                              zmBezOgolem = zmBezOgolem,
                              zmTylkoWartosciWDanych = zmTylkoWartosciWDanych,
                              zmWskaznikiP4 = zmWskaznikiP4,
                              zmWskaznikiP3 = zmWskaznikiP3,
                              statystyki = statystyki,
                              wskTylkoNiezerowe = wskTylkoNiezerowe,
                              etykietaBrakDanych = etykietaBrakDanych,
                              etykietaOgolem = etykietaOgolem,
                              wyswietlPostep = wyswietlPostep,
                              liczbaWatkow = liczbaWatkow)
  return(wierszMatrycy)
}
