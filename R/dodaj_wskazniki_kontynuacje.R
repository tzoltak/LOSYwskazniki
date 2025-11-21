#' @title Obliczanie wskaznikow wykorzystywanych w raportach
#' @description
#' Na podstawie tabeli *pośredniej* P2 funkcja oblicza wskaźniki opisujące
#' formy kontynuowania nauki.
#' @param p4 ramka danych z tabelą *pośrednią* P4 (lub jej odfiltrowanym
#' podzbiorem), musi zawierać co najmniej kolumny `id_abs`, `rok_abs`
#' i `typ_szk`
#' @param p2 ramka danych z tabelą *pośrednią* P2 (lub jej odfiltrowanym
#' podzbiorem)
#' @param miesOdUkoncz wektor liczb całkowitych - wartości zmiennej
#' `mies_od_ukoncz`, dla których mają zostać obliczone wskaźniki
#' @param tylkoLegalne wartość logiczna - czy usunąć z danych przekazanych `p2`
#' kontynuacje, które nominalnie powinny być niemożliwe (ze względu na typ
#' szkoły ukończonej przez absolwenta, por. [legalneKontynuacje])?
#' @returns Ramka danych przekazana argumentem `p4` z dodanymi kolumnami:
#'
#' -    `typ_szk_kont` - formy kontynuacji nauki,
#' -    `branza_kont_bsii` - branża kontynuacji nauki w BS II przez absolwentów BS I,
#' -    `dziedzina_kont` - dziedzina kontynuacji nauki na studiach,
#' -    `dyscyplina_kont` - dyscyplina kontynuacji nauki na studiach.
#' @details
#' Każda z tworzonych kolumn jest macierzą binarną, w której wartość 1 opisuje
#' kontynuowanie nauki w danej formie/branży/dziedzinie/dyscyplinie,
#' 0 niekontynuowanie, a brak danych, że dana kolumna nie ma sensu dla danego
#' absolwenta ze względu na typ ukończonej przez niego szkoły (choć jeśli
#' argument `tylkoLegalne` został ustawiony na `FALSE`, to podjęcie nauki
#' w danej formie itp. zostanie odnotowane wartością 1, nawet jeśli nominalnie
#' taka sytuacja nie powinna się zdarzyć).
#' @seealso [oblicz_wskaznik_macierz()]- *koń roboczy* odpowiedzialny za
#' obliczanie wskaźników w ramach `dodaj_wskazniki_kontynuacje()`
#' @importFrom dplyr %>% .data bind_rows distinct filter full_join mutate select
#'                   semi_join ungroup
#' @importFrom tidyr pivot_wider
#' @export
dodaj_wskazniki_kontynuacje <- function(p4, p2, miesOdUkoncz,
                                        tylkoLegalne = TRUE) {
  stopifnot(is.data.frame(p2),
            all(c("id_abs", "rok_abs", "mies_od_ukoncz") %in% names(p2)),
            !anyNA(p2$id_abs), !anyNA(p2$rok_abs), !anyNA(p2$mies_od_ukoncz),
            is.numeric(miesOdUkoncz), length(miesOdUkoncz) > 0L,
            !anyNA(miesOdUkoncz),
            all(as.integer(miesOdUkoncz) == miesOdUkoncz),
            is.data.frame(p4),
            all(c("id_abs", "rok_abs", "typ_szk") %in% names(p4)),
            mode(p2$id_abs) == mode(p4$id_abs),
            mode(p2$rok_abs) == mode(p4$rok_abs),
            !anyNA(p4$id_abs), !anyNA(p4$rok_abs),
            is.logical(tylkoLegalne), length(tylkoLegalne) == 1L,
            !is.na(tylkoLegalne))
  brakujaceMiesiace <- split(p2$mies_od_ukoncz, p2$rok_abs) %>%
    lapply(function(x, miesOdUkoncz) {return(setdiff(miesOdUkoncz, unique(x)))},
           miesOdUkoncz = miesOdUkoncz)
  brakujaceMiesiace <- brakujaceMiesiace[sapply(brakujaceMiesiace, length) > 0L]
  if (length(brakujaceMiesiace) > 0L) {
    message("Dla niektórych roczników absolwentów nie występują niektóre z wartości kolumny `mies_od_ukoncz` podane argumentem `miesOdUkoncz`:\n- ",
            paste(mapply(function(x, nm) {return(paste0("rok abs ", nm, ": ",
                                                        paste(x, collapse = ", "),
                                                        ";"))},
                         brakujaceMiesiace, names(brakujaceMiesiace)),
                  collapse = "\n- "))
  }

  p2$typ_szk_kont[p2$typ_szk_kont == "studia"] <- "Studia" # dla zgodności ze starszymi wersjami tabel pośrednich
  typySzkol <- c("Studia", "Szkoła policealna", "Liceum dla dorosłych",
                 "Branżowa szkoła II stopnia", "KKZ", "KUZ")
  p2 <- p2 %>%
    filter(.data$mies_od_ukoncz %in% miesOdUkoncz) %>%
    left_join(p4 %>%
                select("id_abs", "rok_abs", "typ_szk") %>%
                distinct(),
              by = c("id_abs", "rok_abs"), relationship = "many-to-many") %>%
    group_by(.data$id_abs, .data$rok_abs, .data$mies_od_ukoncz,
             .data$id_szk_kont, .data$typ_szk_kont, .data$kod_zaw_kont) %>%
    mutate(KKZwBSII = .data$typ_szk_kont == "Branżowa szkoła II stopnia" &
             .data$forma_kont == "KKZ" &
             all(c("uczeń", "KKZ") %in% .data$forma_kont)) %>%
    ungroup() %>%
    filter(!.data$KKZwBSII) %>%
    mutate(typ_szk_kont = factor(if_else(.data$forma_kont %in% typySzkol,
                                         as.character(.data$forma_kont),
                                         as.character(.data$typ_szk_kont)),
                                 typySzkol))
  if (tylkoLegalne) {
    p2 <- p2 %>%
      semi_join(LOSYwskazniki::legalneKontynuacje,
                by = c("typ_szk", "typ_szk_kont"))
  }
  miesRokAbs <- split(p2$mies_od_ukoncz, p2$rok_abs) %>%
    lapply(
      function(x, miesOdUkoncz) {
        return(data.frame(mies_od_ukoncz = intersect(miesOdUkoncz, unique(x))))},
      miesOdUkoncz = miesOdUkoncz) %>%
    bind_rows(.id = "rok_abs") %>%
    mutate(rok_abs = as.integer(.data$rok_abs))
  wszystkieObs <- p4 %>%
    select("id_abs", "rok_abs", "typ_szk") %>%
    distinct() %>%
    full_join(miesRokAbs,
              by = "rok_abs", relationship = "many-to-many")
  p2 <- p2 %>%
    semi_join(wszystkieObs, by = c("id_abs", "rok_abs"))

  szkoly <- p2 %>%
    filter(.data$typ_szk_kont %in% typySzkol |
             .data$forma_kont %in% typySzkol) %>%
    select("id_abs", "rok_abs", "mies_od_ukoncz", "typ_szk", "typ_szk_kont") %>%
    oblicz_wskaznik_macierz(zm = "typ_szk_kont", zestawWartosci = typySzkol,
                            wszystkieObs = wszystkieObs)
  for (typ in unique(LOSYwskazniki::legalneKontynuacje$typ_szk)) {
    nielegalneTypy <-
      setdiff(colnames(szkoly$typ_szk_kont),
              LOSYwskazniki::legalneKontynuacje$typ_szk_kont[
                LOSYwskazniki::legalneKontynuacje$typ_szk == typ])
    szkoly$typ_szk_kont[szkoly$typ_szk == typ, nielegalneTypy] <-
      ifelse(szkoly$typ_szk_kont[szkoly$typ_szk == typ, nielegalneTypy] == 1L,
             szkoly$typ_szk_kont[szkoly$typ_szk == typ, nielegalneTypy],
             NA_integer_)
  }
  szkoly <- szkoly %>%
    pivot_wider(names_from = "mies_od_ukoncz", names_prefix = "typ_szk_kont",
                values_from = "typ_szk_kont")

  branzeBS2 <- p2 %>%
    filter(.data$typ_szk == "Branżowa szkoła I stopnia",
           .data$typ_szk_kont %in% "Branżowa szkoła II stopnia",
           !is.na(.data$branza_kont)) %>%
    select("id_abs", "rok_abs", "typ_szk", "mies_od_ukoncz",
           branza_kont_bsii = "branza_kont") %>%
    oblicz_wskaznik_macierz(zm = "branza_kont_bsii",
                            wszystkieObs = wszystkieObs %>%
                              filter(.data$typ_szk == "Branżowa szkoła I stopnia")) %>%
    pivot_wider(names_from = "mies_od_ukoncz", names_prefix = "branza_kont_bsii",
                values_from = "branza_kont_bsii")

  dziedziny <- p2 %>%
    filter(.data$typ_szk %in% c("Liceum ogólnokształcące",
                                "Liceum dla dorosłych",
                                "Technikum", "Branżowa szkoła II stopnia",
                                "Szkoła policealna"),
           .data$typ_szk_kont %in% "Studia",
           !is.na(.data$dziedzina_kont)) %>%
    select("id_abs", "rok_abs", "typ_szk", "mies_od_ukoncz", "dziedzina_kont") %>%
    oblicz_wskaznik_macierz(zm = "dziedzina_kont",
                            wszystkieObs = wszystkieObs %>%
                              filter(.data$typ_szk %in% c("Liceum ogólnokształcące",
                                                          "Liceum dla dorosłych",
                                                          "Technikum",
                                                          "Branżowa szkoła II stopnia",
                                                          "Szkoła policealna")))
  colnames(dziedziny$dziedzina_kont) <- sub("^Dziedzina ", "",
                                            colnames(dziedziny$dziedzina_kont))
  dziedziny <- dziedziny %>%
    pivot_wider(names_from = "mies_od_ukoncz", names_prefix = "dziedzina_kont",
                values_from = "dziedzina_kont")

  dyscypliny <- p2 %>%
    filter(.data$typ_szk %in% c("Liceum ogólnokształcące",
                                "Liceum dla dorosłych",
                                "Technikum", "Branżowa szkoła II stopnia",
                                "Szkoła policealna"),
           .data$typ_szk_kont %in% "Studia",
           !is.na(.data$dyscyplina_wiodaca_kont)) %>%
    select("id_abs", "rok_abs", "typ_szk", "mies_od_ukoncz",
           dyscyplina_kont = "dyscyplina_wiodaca_kont") %>%
    oblicz_wskaznik_macierz(zm = "dyscyplina_kont",
                            wszystkieObs = wszystkieObs %>%
                              filter(.data$typ_szk %in% c("Liceum ogólnokształcące",
                                                          "Liceum dla dorosłych",
                                                          "Technikum",
                                                          "Branżowa szkoła II stopnia",
                                                          "Szkoła policealna"))) %>%
    pivot_wider(names_from = "mies_od_ukoncz", names_prefix = "dyscyplina_kont",
                values_from = "dyscyplina_kont")

  p4 <- p4 %>%
    left_join(szkoly,
              by = c("id_abs", "rok_abs", "typ_szk")) %>%
    left_join(branzeBS2,
              by = c("id_abs", "rok_abs", "typ_szk")) %>%
    left_join(dziedziny,
              by = c("id_abs", "rok_abs", "typ_szk")) %>%
    left_join(dyscypliny,
              by = c("id_abs", "rok_abs", "typ_szk"))
  return(p4)
}
