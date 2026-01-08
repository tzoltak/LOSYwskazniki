#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala obliczyć zestawienia zagregowanych wskaźników z danej edycji
#' monitoringu dla przekazanych danych. Funkcja agreguje **wszystkie kolumny
#' w danych, które *wyglądają jej na zawierające wskaźniki***, co w praktyce
#' oznacza:
#'
#' -    dla ramki danych przekazanej argumentem `p4`, że ich nazwa albo kończy
#'      się liczbą, albo jest postaci "_r\[liczba\]_\[litery\]";
#' -    dla ramki danych przekazanej argumentem `p3`, wszystkie kolumny poza
#'      `id_abs`, `rok_abs`, `mies_od_ukoncz`, `rok`, `miesiac` i `okres`;
#'      **dane z poszczególnych miesięcy (identyfikowanych wartościami kolumny
#'      `mies_od_ukoncz`) będą tworzyć oddzielne wskaźniki zagregowane** (por.
#'      opis argumentu `miesiace`).
#' @param p4 ramka danych z tabelą *pośrednią* P4 (lub jej odfiltrowanym
#' podzbiorem), musi zawierać co najmniej kolumny `id_abs`, `rok_abs` i `id_szk`
#' @param p3 ramka danych z tabelą *pośrednią* P3 (lub jej odfiltrowanym
#' podzbiorem), musi zawierać co najmniej kolumny `id_abs`, `rok_abs`
#' i `mies_od_ukoncz`
#' @param statystyki opcjonalnie lista z nazwanymi elementami, opisującymi
#' statystyki, które mają zostać obliczone w odniesieniu do wskaźników
#' **liczbowych** (w formacie takim, jak argument `.fns` w formie listy
#' w wywołaniach [dplyr::across()]); funkcje/wyrażenia mogą zwracać więcej niż
#' jedną wartość, ale ew. nazwy elementów zwróconego przez funkcję wektora nie
#' zostaną zachowane; domyślnie średnia obliczana z pominięciem
#' braków danych
#' @param oddzielCzasOdWskaznika opcjonalnie wartość logiczna - czy w zwracanej
#' ramce danych nazwa wskaźnika powinna zostać rozbita na dwie kolumny:
#' `wskaznik` i `czas` (`TRUE`), czy też tylko kolumnę `wskaznik`, zawierającą
#' nazwę wskaźnika połączoną z kodem okresu, dla jakiego został on obliczony
#' (`FALSE`)
#' @param format opcjonalnie ciąg znaków opisujący, czy wyniki mają zostać
#' zwrócone w formie *długiej* (domyślnie), czy *szerokiej*
#' @param miesiace opcjonalnie wektor liczb całkowitych (co do zasady dodatnich)
#' opisujący, wiersze o jakich wartościach kolumny `mies_od_ukoncz` z ramki
#' danych przekazanej argumentem `p3` mają zostać wykorzystane do obliczenia
#' zagregowanych wskaźników (wszystkie inne zostaną pominięte); pozwala też
#' uzyskać efekt polegający na utworzeniu pożądanego zestawy wskaźników
#' obliczonych na podstawie danych z tabeli *pośredniej* P3 w sytuacji, gdy
#' dane przekazane do funkcji są puste (mają zero wierszy); domyślnie zostaną
#' wykorzystane wszystkie dane z `p3`
#' @returns Ramka danych z obliczonymi wskaźnikami, w zależności od kombinacji
#' wartości argumentów `oddzielCzasOdWskaznika` i `format` zawierająca:
#'
#' -    wiele wierszy i trzy kolumny: `wskaznik`, `czas` i `wartosc`
#'      (domyślnie: `oddzielCzasOdWskaznika=TRUE` i `format="długi"`),
#' -    wiele wierszy i dwie kolumny: `wskaznik` i `wartosc`
#'      (`oddzielCzasOdWskaznika=FALSE` i `format="długi"`),
#' -    wiele wierszy, kolumnę `czas` i kolejne kolumny zawierające poszczególne
#'      wskaźniki (`oddzielCzasOdWskaznika=TRUE` i `format="szeroki"`),
#' -    jeden wiersz i kolejne kolumny zawierające poszczególne
#'      wskażniki (`oddzielCzasOdWskaznika=FALSE` i `format="szeroki"`).
#'
#' Odnośnie do formatu kolumn zawierających wartości wskaźników, patrz niżej.
#' @details
#' Agregacja przeprowadzana jest w różny sposób w zależności od typu kolumny
#' w wejściowych danych:
#'
#' -    Kolumny będące czynnikami lub wektorami tekstowymi (w sensie sprawdzania
#'      typu z wykorzystaniem funkcji [is.factor()] i [is.character()]) zostaną
#'      zagregowane z wykorzystaniem funkcji [table()],
#' -    Kolumny będące wektorami liczbowymi lub logicznymi (w sensie sprawdzania
#'      typu z wykorzystaniem funkcji [is.numeric()] i [is.logical()]) zostaną
#'      zagregowane przy pomocy funkcji/wyrażeń przekazanych argumentem
#'      `statystyki` - efektem będzie wektor, nazwy elementów którego
#'      odpowiadają nazwom elementów listy przekazanej argumentem `statystyki`
#'      (domyślnie jednoelementowy wektor, z nazwą elementu "średnia"),
#' -    Kolumny będące macierzami liczb lub wartości logicznych zostaną
#'      zagregowane poprzez zsumowanie wartości w kolumnach, przy czym
#'      **kolumny, w których znajdowały się tylko braki danych, zostaną usunięte
#'      z wyników** (nie dotyczy to sytuacji, gdy dane są puste, tzn. mają zero
#'      wierszy, bo wtedy nie ma sensu rozważanie, czy w danej kolumnie znajduje
#'      się brak danych, czy nie).
#'
#' W efekcie każda kolumna zawierająca zagregowane wskażniki w zwracanej ramce
#' danych jest listą. Każdy z jej elementów stanowi wskaźnik obliczony zgodnie
#' z powyższym opisem, który ma dodatkowo przypisane atrybuty
#' (p. [attributes()]), z których każdy jest liczbą naturalną:
#'
#' -    `lAbs` - liczba wierszy (absolwentów), na podstawie których został
#'      obliczony zagregowany wskaźnik,
#' -    `lSzk` - liczba różnych szkół, które ukończyli absolwenci, na podstawie
#'      których został obliczony zagregowany wskaźnik,
#' -    `lNieDotyczy` - liczba wierszy (absolwentów), zawierających braki danych
#'       (w przypadku kolumn wejściowych będących macierzami - we wszystkich
#'       kolumnach macierzy),
#' -   Wskaźniki zagregowane obliczone na podstawie takich kolumn
#'     mają dodatkowy atrybut `lZadenZWymienionych`, który przechowuje
#'     liczbę wierszy, w których wszystkie kolumny macierzy były albo
#'     zerami albo brakami danych (ale z pominięciem wierszy zawierających
#'     same braki danych).
#' @seealso [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()],
#' [zanonimizuj_wskazniki_pd()]
#' @importFrom dplyr %>% .data across all_of everything mutate n_distinct
#'             reframe select where
#' @importFrom tidyr pivot_longer
#' @export
oblicz_wskazniki_pd <- function(p4, p3,
                                statystyki = list("średnia" = ~mean(., na.rm = TRUE)),
                                oddzielCzasOdWskaznika = TRUE,
                                format = c("długi", "szeroki"),
                                miesiace = NULL) {
  stopifnot(is.data.frame(p3),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p3)),
            !anyNA(p3$id_abs), !anyNA(p3$rok_abs), !anyNA(p3$mies_od_ukoncz),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "id_szk") %in% names(p4)),
            !anyNA(p4$id_abs), !anyNA(p4$rok_abs), !anyNA(p4$id_szk),
            is.list(statystyki), length(statystyki) > 0,
            is.logical(oddzielCzasOdWskaznika),
            length(oddzielCzasOdWskaznika) == 1,
            oddzielCzasOdWskaznika %in% c(TRUE, FALSE),
            is.null(miesiace) | is.numeric(miesiace))
  if (!is.null(miesiace)) {
    stopifnot(!anyNA(miesiace), !any(duplicated(miesiace)))
  }
  format <- match.arg(format)

  if (nrow(p3) == 0L) { # sztukowanie miesięcy, gdy brak danych w grupie
    if (is.null(miesiace)) {
      miesiace <- grep("[^[:digit:]][[:digit:]]+$", names(p4), value = TRUE)
      miesiace <- miesiace[-grep("_r[[:digit:]]+$", miesiace)]
      miesiace <- sub("^.*[^[:digit:]]([[:digit:]]+)$", "\\1", miesiace) %>%
        unique() %>%
        as.integer()
    }
    p3 <- p3 %>%
      reframe(across(where(is.factor),
                     ~factor(rep(NA, length(miesiace)), levels(.))),
              across(c(everything(),
                       -where(is.factor), -all_of("mies_od_ukoncz")),
                     function(x) {
                       m <- mode(x)
                       x <- rep(NA, length(miesiace))
                       mode(x) <- m
                       return(x)
                     }),
              mies_od_ukoncz = miesiace) %>%
      select(all_of(names(p3)))
  }
  p3 <- p3 %>%
    pivot_wider(id_cols = c("id_abs", "rok_abs"),
                names_from = "mies_od_ukoncz",
                values_from =
                  setdiff(names(p3),
                          c("id_abs", "rok_abs",
                            "mies_od_ukoncz", "rok", "miesiac",
                            "okres")),
                names_sep = "")

  p4 <- left_join(p4, p3,
                  by = c("id_abs", "rok_abs")) %>%
    summarise(across(where(is.matrix) & (where(is.numeric) | where(is.logical)) &
                       where(~length(.x) > 0) &
                       (matches("[^[:digit:]][[:digit:]]+$") |
                          matches("_r[[:digit:]]+(|_[[:alpha:]]+)$")),
                     ~list(structure(colSums(.[, colSums(!is.na(.)) != 0,
                                               drop = FALSE],
                                             na.rm = TRUE),
                                     lAbs = nrow(.) -
                                       sum(colSums(!is.na(.)) == 0 &
                                             colSums(!is.na(.)) > 0),
                                     lSzk = n_distinct(.data$id_szk[colSums(!is.na(.)) > 0]),
                                     lZadenZWymienionych =
                                       sum(colSums(., na.rm = TRUE) == 0 &
                                             colSums(!is.na(.)) > 0),
                                     lNieDotyczy = sum(colSums(!is.na(.)) == 0)))),
              # gdy brak danych, wskaźniki macierzowe wymagają innego potraktowania
              across(where(is.matrix) & where(~length(.x) == 0) &
                       (matches("[^[:digit:]][[:digit:]]+$") |
                          matches("_r[[:digit:]]+(|_[[:alpha:]]+)$")),
                     ~list(structure(colSums(.),
                                     lAbs = 0,
                                     lSzk = 0,
                                     lZadenZWymienionych = 0,
                                     lNieDotyczy = 0))),
              across((where(is.factor) | where(is.character)) &
                       (matches("[^[:digit:]][[:digit:]]+$") |
                          matches("_r[[:digit:]]+(|_[[:alpha:]]+)$")),
                     ~list(structure(table(.),
                                     lAbs = length(.) - sum(is.na(.)),
                                     lSzk = n_distinct(.data$id_szk[!is.na(.)]),
                                     lNieDotyczy = sum(is.na(.))))),
              across((where(is.numeric) | where(is.logical)) &
                       (matches("[^[:digit:]][[:digit:]]+$") |
                          matches("_r[[:digit:]]+(|_[[:alpha:]]+)$")),
                     ~list(structure(unlist(lapply(
                       statystyki,
                       function(x, dane) {
                         return(pull(reframe(data.frame(dane),
                                             across("dane", x)),
                                     "dane"))},
                       dane = .)),
                       lAbs = length(.) - sum(is.na(.)),
                       lSzk = n_distinct(.data$id_szk[!is.na(.)]),
                       lNieDotyczy = sum(is.na(.))))),
              .groups = "drop") %>%
    pivot_longer(cols = everything(),
                 names_to = "wskaznik",
                 values_to = "wartosc")
  if (oddzielCzasOdWskaznika) {
    p4 <- p4 %>%
      mutate(czyMiesiac = !grepl("_(r[[:digit:]]+)(|_[[:alpha:]]+)$",
                                 .data$wskaznik),
             czas = ifelse(.data$czyMiesiac,
                           sub("^.*[^[:digit:]]([[:digit:]]+)$", "\\1",
                               .data$wskaznik),
                           sub("^.*_(r[[:digit:]]+)(|_[[:alpha:]]+)$", "\\1\\2",
                               .data$wskaznik)),
             wskaznik = ifelse(.data$czyMiesiac,
                               sub("^(.*[^[:digit:]])[[:digit:]]+$", "\\1",
                                   .data$wskaznik),
                               sub("^(.*)_r[[:digit:]]+(|_[[:alpha:]]+)$", "\\1",
                                   .data$wskaznik))) %>%
      select(everything(), -c("wartosc", "czyMiesiac"), "wartosc")
  }
  if (format == "szeroki") {
    p4 <- p4 %>%
      pivot_wider(names_from = "wskaznik", values_from = "wartosc")
  }
  return(p4)
}
