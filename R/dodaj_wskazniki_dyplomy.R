#' @title Obliczanie wskaznikow wykorzystywanych w raportach
#' @description
#' Na podstawie tabeli *pośredniej* P1 funkcja oblicza wskaźniki opisujące
#' uzyskiwanie dyplomów: zdanie matury oraz uzyskanie dyplomów zawodowych
#' (świadectw czeladniczych, certyfikatów kwalifikacji).
#' @param p4 ramka danych z tabelą *pośrednią* P4 (lub jej odfiltrowanym
#' podzbiorem), musi zawierać co najmniej kolumny `id_abs`, `rok_abs`
#' i `kod_zaw`
#' @param p1 ramka danych z tabelą *pośrednią* P1 (lub jej odfiltrowanym
#' podzbiorem)
#' @param maksMiesOdUkoncz liczba całkowita opisująca okres, który będzie brany
#' pod uwagę: **jeśli jest to liczba nieujemna**, będzie to maksymalna wartość
#' zmiennej `mies_od_ukoncz` w ramce danych przekazanej argumentem `p1`;
#' **jeśli jest to liczba ujemna**, konieczne jest również podanie wartości
#' argumentu `rokMonitoringu` a brany pod uwagę okres zostanie ograniczony
#' z góry do czerwca `rokMonitoringu` plus (ujemna) `maksMiesOdUkoncz` (czyli
#' np. wartość -3 wskazuje na maksymalnie marzec roku prowadzenia monitoringu)
#' @param rokMonitoringu opcjonalnie liczba całkowita - rok (edycja)
#' monitoringu; musi być podana, jeśli `maksMiesOdUkoncz` jest liczbą ujemną
#' (w przeciwnym wypadku wartość tego argumentu nie jest w żaden sposób
#' wykorzystywana)
#' @returns Ramka danych przekazana argumentem `p4` z dodanymi kolumnami:
#'
#' -    `matura_zdana` - liczba całkowita: 0 - nie, 1 - tak,
#' -    `dyplom_zaw` - czynnik o poziomach: "Świadectwo czeladnicze",
#'      "Dyplom zawodowy", "Tylko certyfikat kwalifikacji",
#'       "Brak certyfikatów i dyplomu".
#' @details
#' W przypadku `dyplom_zaw`:
#'
#' -    Pod uwagę brane jest tylko uzyskanie dyplomu/certyfikatu/świadectwa
#'      w wyuczonym zawodzie;
#' -    W przypadku dyplomów zawodowych dodatkowo brane są pod uwagę tylko te
#'      uzyskane nie wcześniej niż w roku ukończenia szkoły;
#' -    Uzyskanie dyplomu zawodowego nadpisuje wszystkie inne
#'      certyfikaty/świadectwa, a uzyskanie tytułu czeladnika nadpisuje
#'      uzyskanie certyfikatu kwalifikacji.
#' -    Osoby, które nie kształciły się zawodowo mają przypisany brak danych.
#'
#' W przypadku `matura_zdana` wartości obliczane są dla wszystkich rekordów
#' **bez względu na typ ukończonej szkoły**.
#' @examples
#' \dontrun{
#'   p4 <- dodaj_wskazniki_dyplomy(p4, p1,
#'                                 maksMiesOdUkoncz = -3L, rokMonitoringu = 2025)
#'   table(p4$matura_zdana)
#'   table(p4$dyplom_zaw)
#'   p4 <- dodaj_wskazniki_dyplomy(p4, p1, maksMiesOdUkoncz = 21L)
#' }
#' @importFrom dplyr %>% .data all_of arrange case_match coalesce distinct
#'                   filter if_else left_join mutate select
#' @export
dodaj_wskazniki_dyplomy <- function(p4, p1, maksMiesOdUkoncz,
                                    rokMonitoringu = NULL) {
  stopifnot(is.data.frame(p1),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz", "rok",
                  "rodzaj_dyplomu", "kod_zaw", "kod_zaw_dyplom") %in% names(p1)),
            !anyNA(p1$id_abs), !anyNA(p1$rok_abs),
            !any(is.na(p1$mies_od_ukoncz) & p1$rodzaj_dyplomu != "tytuł czeladnika"),
            !any(is.na(p1$rok) & p1$rodzaj_dyplomu != "tytuł czeladnika"),
            is.numeric(maksMiesOdUkoncz), length(maksMiesOdUkoncz) == 1L,
            !is.na(maksMiesOdUkoncz),
            as.integer(maksMiesOdUkoncz) == maksMiesOdUkoncz,
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "kod_zaw") %in% names(p4)),
            mode(p1$id_abs) == mode(p4$id_abs),
            mode(p1$rok_abs) == mode(p4$rok_abs),
            !anyNA(p4$id_abs), !anyNA(p4$rok_abs),
            is.null(rokMonitoringu) | is.numeric(rokMonitoringu))
  if (!is.null(rokMonitoringu)) {
    stopifnot(length(rokMonitoringu) == 1L, !is.na(rokMonitoringu),
              as.integer(rokMonitoringu) == rokMonitoringu,
              rokMonitoringu >= 2019)
  }
  if (maksMiesOdUkoncz < 0) {
    stopifnot("Jeśli argument `maksMiesOdUkoncz` został podany jako liczba ujemna, konieczne jest podanie również wartości argumentu `rokMonitoringu`." =
                !is.null(rokMonitoringu))
    p1 <- p1 %>%
      mutate(maksMiesOdUkoncz = (rokMonitoringu - .data$rok_abs)*12) %>%
      filter(.data$mies_od_ukoncz <= .data$maksMiesOdUkoncz |
               .data$rodzaj_dyplomu == "tytuł czeladnika") %>%
      select(-"maksMiesOdUkoncz")
  } else {
    p1 <- p1 %>%
      filter(.data$mies_od_ukoncz <= maksMiesOdUkoncz |
               .data$rodzaj_dyplomu == "tytuł czeladnika")
  }
  zmienneDoUsuniecia <- intersect(c("matura_zdana", "dyplom_zaw"), names(p4))
  if (length(zmienneDoUsuniecia) > 0L) {
    warning("Zmienne: `", paste(zmienneDoUsuniecia, collapse = "`, `"),
            ", które już istnieją w danych przekazanych argumentem `p4` zostaną usunięte i będą utworzone na nowo.",
            immediate. = TRUE)
    p4 <- select(p4, -all_of(zmienneDoUsuniecia))
  }

  matura <- p1 %>%
    filter(.data$rodzaj_dyplomu == "matura") %>%
    select("id_abs", "rok_abs") %>%
    distinct() %>%
    mutate(matura_zdana = 1L)

  dyplomZaw <- p1 %>%
    filter(.data$rodzaj_dyplomu == "dyplom zawodowy",
           .data$kod_zaw == .data$kod_zaw_dyplom,
           .data$rok >= .data$rok_abs) %>%
    select("id_abs", "rok_abs", "kod_zaw", dyplomZaw = "rodzaj_dyplomu") %>%
    distinct()
  tytulCzel <- p1 %>%
    filter(.data$rodzaj_dyplomu == "tytuł czeladnika",
           .data$kod_zaw == .data$kod_zaw_dyplom) %>%
    select("id_abs", "rok_abs", "kod_zaw", tytulCzel = "rodzaj_dyplomu") %>%
    distinct()
  certyfikatKwal <- p1 %>%
    filter(.data$rodzaj_dyplomu == "certyfikat kwalifikacji",
           .data$kod_zaw == .data$kod_zaw_dyplom) %>%
    select("id_abs", "rok_abs", "kod_zaw", certyfikatKwal = "rodzaj_dyplomu") %>%
    distinct()
  dyplomy <- dyplomZaw %>%
    full_join(tytulCzel, by = c("id_abs", "rok_abs", "kod_zaw")) %>%
    full_join(certyfikatKwal, by = c("id_abs", "rok_abs", "kod_zaw")) %>%
    mutate(dyplom_zaw = coalesce(dyplomZaw, tytulCzel, certyfikatKwal)) %>%
    select("id_abs", "rok_abs", "kod_zaw", "dyplom_zaw")
  rm(dyplomZaw, tytulCzel, certyfikatKwal)

  p4 <- p4 %>%
    left_join(matura,
              by = c("id_abs", "rok_abs")) %>%
    left_join(dyplomy,
              by = c("id_abs", "rok_abs", "kod_zaw")) %>%
    mutate(matura_zdana = if_else(is.na(.data$matura_zdana),
                                  0L, .data$matura_zdana),
           dyplom_zaw = if_else(is.na(.data$kod_zaw), NA_character_,
                                if_else(is.na(.data$dyplom_zaw),
                                        "Brak certyfikatów i dyplomu",
                                        .data$dyplom_zaw)) %>%
             factor(c("tytuł czeladnika", "dyplom zawodowy",
                      "certyfikat kwalifikacji",
                      "Brak certyfikatów i dyplomu"),
                    c("Świadectwo czeladnicze", "Dyplom zawodowy",
                      "Tylko certyfikat kwalifikacji",
                      "Brak certyfikatów i dyplomu")))
  return(p4)
}
