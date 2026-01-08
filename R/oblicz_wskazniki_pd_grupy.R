#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala obliczyć zestawienia zagregowanych wskaźników z danej edycji
#' monitoringu w ramach każdej grupy zdefiniowanej przez kombinację wartości
#' podanych zmiennych (*grupujących/niezależnych*).
#' @inheritParams oblicz_wskazniki_pd
#' @param zmGrupujace wektor tekstowy zawierający nazwy zmiennych z ramki danych
#' przekazanej argumentem `p4`, kombinacje wartości których określają grupy,
#' w ramach których mają zostać obliczone zagregowane wskaźniki (funkcja
#' poradzi sobie również z przekazaniem tu wektora tekstowego zerowej długości
#' lub wartości `NULL` - poskutkuje to obliczeniem zagregowanych wskaźników na
#' całości danych - ale takie wywołanie jest oczywiście nietypowe)
#' @param zmBezOgolem opcjonalnie wektor tekstowy zawierający nazwy zmiennych,
#' dla których mają nie być tworzone grupy "Ogółem" opisujące wszystkich
#' absolwentów, bez względu na konkretną wartość danej zmiennej (podane nazwy
#' **nie** muszą występować wśród nazw przekazanych argumentem `zmGrupujace`
#' - jeśli ich tam nie ma, zostaną zignorowane); domyślnie "typ_szk";
#' przekazanie wektora tekstowego zerowej długości lub wartości `NULL` oznacza,
#' że nie ma być żadnych takich zmiennych
#' @param zmTylkoWartosciWdanych opcjonalnie lista wektorów tekstowych,
#' pisujących dla jakich zestawów zmiennych mają zostać utworzone grupy tylko
#' dla tych kombinacji wartości, które występują w danych wejściowych
#' przekazanych argumentem `p4` - każdy element definiuje oddzielony zestaw
#' zmiennych (podane nazwy **nie** muszą występować wśród nazw przekazanych
#' argumentem `zmGrupujace` - te, których wśród nich nie ma, zostaną
#' zignorowane, a kombinacje będą rozpatrywane ze względu na pozostałe);
#' domyślna wartość opisuje, że mają zostać utworzone tylko występujące w danych
#' kombinacje wartości zmiennych `typ_szk`, `kod_zaw` i `nazwa_zaw`; jeśli
#' nie chce się definiować takich zestawów, można przekazać `NULL` lub listę
#' zerowej długości
#' @param wyswietlPostep opcjonalnie wartość logiczna - czy wyświetlać pasek
#' postępu obrazujący postęp obliczeń? (domyślnie `TRUE`)
#' `poziom`)
#' @returns Ramka danych z kolumnami:
#' -    `rok_abs`,
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
#' @seealso [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd()],
#' [zanonimizuj_wskazniki_pd()]
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr %>% across all_of any_of distinct filter left_join mutate
#'             pick select semi_join where
#' @importFrom tidyr expand_grid unnest
#' @export
oblicz_wskazniki_pd_grupy <- function(p4, p3, zmGrupujace,
                                      zmBezOgolem = "typ_szk",
                                      zmTylkoWartosciWdanych =
                                        list(c("typ_szk", "kod_zaw", "nazwa_zaw")),
                                      statystyki = list("średnia" = ~mean(., na.rm = TRUE)),
                                      wyswietlPostep = TRUE) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p3)),
            !anyNA(p3$id_abs), !anyNA(p3$rok_abs), !anyNA(p3$mies_od_ukoncz),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "id_szk") %in% names(p4)),
            !anyNA(p4$id_abs), !anyNA(p4$rok_abs), !anyNA(p4$id_szk),
            !any(c("___zawieraOgolem___", "___wDanych___",
                   "___wskazniki___") %in% names(p4)),
            is.character(zmGrupujace) | is.null(zmGrupujace),
            is.character(zmBezOgolem) | is.null(zmBezOgolem),
            is.list(zmTylkoWartosciWdanych) | is.null(zmTylkoWartosciWdanych),
            is.logical(wyswietlPostep), length(wyswietlPostep) == 1,
            wyswietlPostep %in% c(FALSE, TRUE))
  if (is.null(zmGrupujace)) zmGrupujace <- vector(mode = "character", length = 0)
  if (is.null(zmBezOgolem)) zmBezOgolem <- vector(mode = "character", length = 0)
  stopifnot(!anyNA(zmGrupujace), !any((duplicated(zmGrupujace))),
            all(zmGrupujace %in% names(p4)),
            !anyNA(zmBezOgolem), !any((duplicated(zmBezOgolem))))
  if (is.null(zmTylkoWartosciWdanych)) {
    zmTylkoWartosciWdanych <- list(vector(mode = "character", length = 0))
  }
  for (i in seq_along(zmTylkoWartosciWdanych)) {
    stopifnot(is.character(zmTylkoWartosciWdanych[[i]]),
              !anyNA(zmTylkoWartosciWdanych[[i]]),
              !any(duplicated(zmTylkoWartosciWdanych[[i]])))
    zmTylkoWartosciWdanych[[i]] <- intersect(zmTylkoWartosciWdanych[[i]],
                                             zmGrupujace) %>%
      intersect(names(p4))
  }
  zmTylkoWartosciWdanych <-
    zmTylkoWartosciWdanych[sapply(zmTylkoWartosciWdanych, length) > 0L]
  miesiace <- unique(p3$mies_od_ukoncz)

  p4 <- p4 %>%
    mutate(across(c(all_of(zmGrupujace) & where(is.factor),
                    -any_of(zmBezOgolem)),
                  ~factor(., levels = unique(c(levels(.), "Ogółem")),
                          exclude = NULL)),
           across(c(all_of(zmGrupujace), -where(is.factor),
                    -any_of(zmBezOgolem)),
                  ~factor(., levels = unique(c(setdiff(as.character(sort(unique(.))),
                                                       NA_character_),
                                               "Ogółem")),
                          exclude = NULL)))
  if (length(zmGrupujace) > 0L) {
    matryca <- do.call(expand_grid,
                       p4 %>%
                         select(all_of(zmGrupujace)) %>%
                         lapply(\(x) factor(levels(x), exclude = NULL))) %>%
      mutate("___zawieraOgolem___" =
               rowSums(pick(any_of(unlist(zmTylkoWartosciWdanych,
                                          use.names = FALSE))) != "Ogółem") == 0,
             "___wDanych___" = FALSE)
    for (i in seq_along(zmTylkoWartosciWdanych)) {
      matryca <- matryca %>%
        left_join(p4 %>%
                    select(any_of(zmTylkoWartosciWdanych[[i]])) %>%
                    distinct() %>%
                    mutate("___wDanychI___" = TRUE),
                  by = intersect(zmTylkoWartosciWdanych[[i]],
                                 names(matryca))) %>%
        mutate("___wDanych___" = .data$`___wDanych___` | .data$`___wDanychI___`) %>%
        select(-"___wDanychI___")
    }
    matryca <- matryca %>%
      filter(.data$`___zawieraOgolem___` | .data$`___wDanych___` %in% TRUE) %>%
      select(-c("___zawieraOgolem___", "___wDanych___"))
  } else {
    matryca <- data.frame(nic = NA)[, c()]
  }

  matryca$`___wskazniki___` <- vector(mode = "list", length = nrow(matryca))
  if (wyswietlPostep) pb <- txtProgressBar(min = 0, max = nrow(matryca), style = 3)
  for (i in seq_len(nrow(matryca))) {
    zmNieOgolem <- setdiff(names(matryca)[as.vector(matryca[i, ] != "Ogółem")],
                           "___wskazniki___")
    if (length(zmNieOgolem) > 0L) {
      p4Temp <- p4 %>%
        semi_join(matryca[i, intersect(zmNieOgolem, names(matryca))],
                  by = intersect(zmNieOgolem, names(matryca)))
    } else {
      p4Temp <- p4
    }
    matryca$`___wskazniki___`[[i]] <-
      oblicz_wskazniki_pd(p4 = p4Temp,
                          p3 = semi_join(p3, p4Temp,
                                         by = c("id_abs", "rok_abs")),
                          statystyki = statystyki,
                          miesiace = miesiace)
    if (wyswietlPostep) setTxtProgressBar(pb, i)
  }
  if (wyswietlPostep) close(pb)
  return(unnest(matryca, "___wskazniki___"))
}
