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
#' @param zmTylkoWartosciWdanych opcjonalnie lista z nazwanymi elementami,
#' z których każdy jest wektorem tekstowym, opisujących dla jakich zestawów
#' zmiennych mają zostać utworzone grupy tylko dla tych kombinacji wartości,
#' które występują w danych wejściowych przekazanych argumentem `p4` (każdy
#' element definiuje oddzielony zestaw zmiennych) - przy sprawdzaniu, czy dana
#' kombinacja wartości występuje w danych używane będą zarówno zmienne, których
#' nazwy zawiera dany element listy, (np. domyślnie `kod_zaw` i `nazwa_zaw`) jak
#'  zmienna, której nazwę podano nazwą tego elementu listy (np.domyślnie
#'  `typ_szk`), przy czym:
#'
#' -    zmienne podane wartościami elementu listy będą miały tworzoną wspólnie
#'      *wartość ogółem* (tj. wśród tworzonych grup albo wszystkie te zmienne
#'      będą mieć przypisaną *wartość ogółem*, albo żadna z nich; uwaga, to
#'      oznacza, że jeśli choć jedna z nich zostanie też wymieniona
#'      w argumencie `zmBezOgolem`, to nie zostaną utworzone żadne grupy,
#'      dla których te zmienne przyjmowały by *wartość ogółem*),
#' -    dla zmiennej, której nazwa została podana nazwą elementu listy
#'      *wartość ogółem* zostanie utworzona niezależnie od zmiennych, których
#'      nazwy podano elementem listy.
#'
#' Zmienne o nazwach podanych tym argumentem **nie** muszą występować wśród nazw
#' zmiennych przekazanych argumentem `zmGrupujace` - te, z nich, których wśród
#' nie ma wśród tych drugich, zostaną zignorowane, a kombinacje będą
#' rozpatrywane ze względu na pozostałe.
#'
#' Domyślna wartość opisuje, że mają zostać utworzone tylko występujące w danych
#' kombinacje wartości zmiennych `typ_szk`, `kod_zaw` i `nazwa_zaw`. Jeśli
#' nie chce się definiować takich zestawów, można przekazać `NULL` lub listę
#' zerowej długości.
#' @param etykietaBrakDanych opcjonalnie ciąg znaków, którym zostaną zastąpione braki
#' danych w zmiennych grupujących (podanych argumentem `zmGrupujace`),
#' z wyjątkiem tych, które **jednocześnie** są zmiennymi liczbowymi lub
#' logicznymi i zostały podane argumentem `zmBezOgolem`; domyślnie "Ndt."
#' @param etykietaOgolem opcjonalnie ciąg znaków, który będzie używany jako
#' ***wartość ogółem***, tj. etykieta (wartość) opisująca, że w danym
#' zestawieniu zostały uwzględnione wszystkie wartości danej zmiennej
#' grupującej; domyślnie "Ogółem"
#' @param wyswietlPostep opcjonalnie wartość logiczna - czy wyświetlać pasek
#' postępu obrazujący postęp obliczeń? (domyślnie `TRUE`)
#' `poziom`)
#' @param zwrocTylkoMatryce opcjonalnie wartość logiczna - czy zwrócić samą
#' *matrycę* grup, bez obliczania wartości wskaźników? przydatne do
#' eksperymentowania ze skutkami modyfikowania wartości argumentu
#' `zmTylkoWartosciWdanych`; domyślnie `FALSE`
#' @returns Ramka danych z kolumnami:
#' -    `rok_abs`,
#' -    kolumnami podanymi argumentem `zmGrupujace`, przekształconymi
#'      w czynniki i - z wyjątkiem kolumn podanych argumentem `zmBezOgolem` -
#'      z dodaną dodatkową wartością "Ogółem" (i w związku z tym przekształcone
#'      na czynniki),
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
                                      wyswietlPostep = TRUE,
                                      zwrocTylkoMatryce = FALSE) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p3)),
            !anyNA(p3$id_abs), !anyNA(p3$rok_abs), !anyNA(p3$mies_od_ukoncz),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "id_szk") %in% names(p4)),
            !anyNA(p4$id_abs), !anyNA(p4$rok_abs), !anyNA(p4$id_szk),
            !any(c("___tylkoWDanychNazwaOgolem___", "___tylkoWDanychOgolem___",
                   "___wDanych___", "___wskazniki___") %in% names(p4)),
            is.character(zmGrupujace) | is.null(zmGrupujace),
            is.character(zmBezOgolem) | is.null(zmBezOgolem),
            is.list(zmTylkoWartosciWdanych) | is.null(zmTylkoWartosciWdanych),
            is.character(etykietaBrakDanych), length(etykietaBrakDanych) == 1L,
            !is.na(etykietaBrakDanych),
            is.logical(wyswietlPostep), length(wyswietlPostep) == 1L,
            wyswietlPostep %in% c(FALSE, TRUE),
            is.logical(zwrocTylkoMatryce), length(zwrocTylkoMatryce) == 1L,
            zwrocTylkoMatryce %in% c(FALSE, TRUE))
  if (is.null(zmGrupujace)) zmGrupujace <- vector(mode = "character", length = 0L)
  if (is.null(zmBezOgolem)) zmBezOgolem <- vector(mode = "character", length = 0L)
  stopifnot(!anyNA(zmGrupujace), !any((duplicated(zmGrupujace))),
            all(zmGrupujace %in% names(p4)),
            !anyNA(zmBezOgolem), !any((duplicated(zmBezOgolem))))
  if (is.null(zmTylkoWartosciWdanych)) {
    zmTylkoWartosciWdanych <- list(vector(mode = "character", length = 0L))
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
  stopifnot(all(names(zmTylkoWartosciWdanych) %in% names(p4)))
  if (is.null(names(zmTylkoWartosciWdanych))) {
    names(zmTylkoWartosciWdanych) <- rep("", length(zmTylkoWartosciWdanych))
  }
  miesiace <- unique(p3$mies_od_ukoncz)

  p4 <- p4 %>%
    mutate(across(all_of(zmGrupujace) &
                    (!(all_of(zmBezOgolem)) | where(is.character) |
                       where(is.factor)),
                  ~podmien_braki_danych(., etykietaBrakDanych,
                                        zawszeDodajPoziom = FALSE))) %>%
    mutate(across(c(all_of(zmGrupujace), -all_of(zmBezOgolem)),
                  ~dodaj_wartosc_ogolem(., wartosc = etykietaOgolem,
                                        zawszeDodawaj = FALSE)))
  if (length(zmGrupujace) > 0L) {
    matryca <- do.call(expand_grid,
                       p4 %>%
                         select(all_of(zmGrupujace)) %>%
                         lapply(
                           function(x) {
                             if (is.factor(x)) {
                               return(factor(levels(x)))
                             } else {
                               return(sort(unique(x), na.last = TRUE))
                             }
                           })) %>%
      mutate("___tylkoWDanychNazwaOgolem___" = FALSE,
             "___wDanych___" = FALSE)
    for (i in seq_along(zmTylkoWartosciWdanych)) {
      matryca <- matryca %>%
        mutate("___tylkoWDanychOgolem___" =
                 rowSums(pick(all_of(zmTylkoWartosciWdanych[[i]])) ==
                           "Ogółem")) %>%
        filter(.data$`___tylkoWDanychOgolem___` %in%
                 c(0L, length(zmTylkoWartosciWdanych[[i]]))) %>%
        left_join(p4 %>%
                    select(any_of(c(names(zmTylkoWartosciWdanych)[i],
                                    zmTylkoWartosciWdanych[[i]]))) %>%
                    distinct() %>%
                    mutate("___wDanychI___" = TRUE),
                  by = intersect(c(names(zmTylkoWartosciWdanych)[i],
                                   zmTylkoWartosciWdanych[[i]]),
                                 names(matryca))) %>%
        mutate("___wDanychI___" = ifelse(is.na(.data$`___wDanychI___`),
                                         FALSE, .data$`___wDanychI___`)) %>%
        group_by(across(all_of(zmTylkoWartosciWdanych[[i]]))) %>%
        filter(any(.data$`___wDanychI___`) |
                 .data$`___tylkoWDanychOgolem___` > 0L) %>%
        ungroup()
      if (names(zmTylkoWartosciWdanych)[i] != "") {
        matryca <- matryca %>%
          mutate("___tylkoWDanychNazwaOgolem___" =
                   .data$`___tylkoWDanychNazwaOgolem___` |
                   .data[[names(zmTylkoWartosciWdanych)[i]]] == "Ogółem") %>%
          group_by(across(all_of(names(zmTylkoWartosciWdanych)[i]))) %>%
          mutate("___wDanychI___" = .data$`___wDanychI___` |
                   (any(.data$`___wDanychI___`) &
                      rowSums(pick(all_of(
                        zmTylkoWartosciWdanych[[i]])) == "Ogółem") ==
                      length(zmTylkoWartosciWdanych[[i]]))) %>%
          group_by(across(all_of(names(zmTylkoWartosciWdanych)[i]))) %>%
          filter(.data$`___tylkoWDanychOgolem___` == 0L |
                   (pick("___tylkoWDanychOgolem___", "___wDanychI___",
                         all_of(zmTylkoWartosciWdanych[[i]])) |>
                      filter(.data$`___tylkoWDanychOgolem___` == 0L &
                               .data$`___wDanychI___`) |>
                      distinct() |>
                      nrow()) > 1L) %>%
          ungroup()
      }
      matryca <- matryca %>%
        mutate("___wDanych___" =
                 .data$`___wDanych___` | .data$`___wDanychI___`) %>%
        select(-"___wDanychI___")
    }
    matryca <- matryca %>%
      filter(.data$`___tylkoWDanychNazwaOgolem___` |
               .data$`___wDanych___` %in% TRUE) %>%
      select(-any_of(c("___tylkoWDanychNazwaOgolem___", "___wDanych___",
                       "___tylkoWDanychOgolem___")))
  } else {
    matryca <- data.frame(nic = NA)[, c()]
  }

  if (zwrocTylkoMatryce) return(matryca)
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
                          zmWskaznikiP4 = zmWskaznikiP4,
                          zmWskaznikiP3 = zmWskaznikiP3,
                          statystyki = statystyki,
                          wskTylkoNiezerowe = wskTylkoNiezerowe,
                          miesiace = miesiace)
    if (wyswietlPostep) setTxtProgressBar(pb, i)
  }
  if (wyswietlPostep) close(pb)
  return(unnest(matryca, "___wskazniki___"))
}
