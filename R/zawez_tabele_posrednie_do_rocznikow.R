#' @title Przygotowanie tabel posrednich do dalszych analiz
#' @description
#' Funkcja pozwala zawęzić (odfiltrować) dane w tabelach *pośrednich*
#' (przygotowanych wcześniej przy pomocy [MLASdaneAdm::przygotuj_tabele_posrednie()])
#' do wskazanych roczników absolwentów i ew. ze względu na inne cechy
#' absolwentów (lub ich szkół - w praktyce, wartości kolumn występujących
#' w tabeli *pośredniej* p4). Domyślnie funkcja próbuje odszukać obiekty
#' przechowujące tabele *pośrednie* w środowisku, z którego została wywołana
#' i zmodyfikować je w tym środowisku. Argument `sufiks` pozwala zapisać
#' zawężone tabele *pośrednie* do nowych obiektów, zamiast nadpisywać
#' istniejące. Alternatywnie, tabele *pośrednie* można przekazać w formie listy
#' ramek danych, z wykorzystaniem argumentu `tabelePosrednie`.
#' @param lataAbs wektor liczb całkowitych - lata ukończenia szkoły, które mają
#' zostać pozostawione w zawężonych danych
#' @param ... opcjonalnie <[`wyrażenia`][rlang::args_data_masking]> zwracające
#' wartości logiczne, **odwołujące się do kolumn w tabeli *pośredniej* p4**
#' (zwykle innych niż `rok_abs`), które definiuje cechy absolwentów, do których
#' mają zostać zawężone dane w tabelach *pośrednich* - zostaną przekazane, razem
#' z tabelą *pośrednią* p4, do funkcji [dplyr::filter()]
#' @param sufiks opcjonalnie ciąg znaków, który zostanie dodany do nazw obiektów
#' zawierających tabele *pośrednie* po dokonaniu zawężenia (ignorowany, jeśli
#' tabele *pośrednie* są przekazywane argumentem `tabelePosrednie`)
#' @param tabelePosrednie opcjonalnie lista ramek danych zawierających tabele
#' *pośrednie*
#' @details
#' Funkcja w specyficzny sposób podchodzi do filtrowania po roku ukończenia
#' szkoły (tj. wartościach kolumny `rok_abs`) - wymagając zawsze zdefiniowania
#' go *explicite* i wykonując diagnostykę przyrównującą podane wartości
#' argumentu `lataAbs` do tych, które występują w tabeli *pośredniej* p4.
#'
#' Ew. dodatkowe wyrażenia filtrujące (przekazane `...`) nie są w żaden sposób
#' weryfikowane, w szczególności pod kątem tego, czy podawane w nich wartości
#' występują w danych (za to próba odwołania się do kolumny nie występującej
#' w tabeli *pośredniej* p4 doprowadzi oczywiście do błędu).
#' @returns Lista zawężonych (odfiltrowanych) tabel pośrednich albo, przy
#' wywołaniu z argumentem `tabelePosrednie=NULL`, niewidocznie
#' (tj. z wykorzystaniem `invisible()`) wektor nazw obiektów, na których
#' dokonano zawężenia.
#' @importFrom dplyr %>% filter semi_join
#' @export
zawez_tabele_posrednie_do_rocznikow <- function(lataAbs, ...,
                                                sufiks = '',
                                                tabelePosrednie = NULL) {
  stopifnot(is.numeric(lataAbs), !anyNA(lataAbs),
            !any(duplicated(lataAbs)), all(lataAbs >= 2019L),
            all(lataAbs == as.integer(lataAbs)),
            is.character(sufiks), length(sufiks) == 1L, !is.na(sufiks),
            is.null(tabelePosrednie) | is.list(tabelePosrednie))
  if (is.null(tabelePosrednie)) {
    srodowisko <- parent.frame()
    tabelePosrednie <- ls(srodowisko)
    tabelePosrednie <- tabelePosrednie[grep("^p[[:digit:]]+$", tabelePosrednie)]
    tabelePosrednie <-
      tabelePosrednie[sapply(tabelePosrednie,
                             function(x, srodowisko) {
                               is.data.frame(get(x, envir = srodowisko))
                             },
                             srodowisko = srodowisko)]
    message("W środowisku zidentyfikowano następujące obiekty, które zapewne zawierają tabele pośrednie:\n",
            paste(tabelePosrednie, collapse = ", "), ".")
    stopifnot("Nie zidentyfikowano obiektu zawierającego tabelę pośrednią 'p4'." =
                "p4" %in% tabelePosrednie,
              "rok_abs" %in% names(get("p4", envir = srodowisko)),
              is.numeric(get("p4", envir = srodowisko)$rok_abs),
              !anyNA(get("p4", envir = srodowisko)$rok_abs))
    lataAbsWDanych = intersect(lataAbs, get("p4", envir = srodowisko)$rok_abs)
    if (length(lataAbsWDanych) == 0L) {
      stop("W tabeli 'p4' nie ma rekordów opisujących podane roczniki absolwentów.")
    }
    if (length(lataAbsWDanych) < length(lataAbs)) {
      warning("W tabeli 'p4' nie występują wartości `rok_abs` równe: ",
              paste(sort(setdiff(lataAbs, lataAbsWDanych)), collapse = ","), ".")
    }
    tabeleDoZawezenia <-
      sapply(tabelePosrednie,
             function(x, srodowisko) {
               all(c("id_abs", "rok_abs") %in% names(get(x, envir = srodowisko)))
             },
             srodowisko = srodowisko) &
      tabelePosrednie != "p4"
    tabelePosrednie <- c("p4", tabelePosrednie[tabeleDoZawezenia])
    message("Zaweżone zostaną dane w następujących tabelach pośrednich (w których występują kolumny `id_abs` i `rok_abs`):\n",
            paste(tabelePosrednie, collapse = ", "), ".")
    lRekordowPrzed <- sapply(tabelePosrednie,
                             function(x, srodowisko) {
                               nrow(get(x, envir = srodowisko))
                             },
                             srodowisko = srodowisko)
    tryCatch(
      assign(paste0("p4", sufiks),
             get("p4", envir = srodowisko) %>%
               filter(.data$rok_abs %in% lataAbs,
                      ...),
             envir = srodowisko),
      error = function(e) stop("Przy próbie odfiltrowania rekordów w tabeli pośredniej p4:\n", e,
                               call. = FALSE))
    for (tp in setdiff(tabelePosrednie, "p4")) {
      assign(paste0(tp, sufiks),
             get(tp, envir = srodowisko) %>%
               semi_join(get("p4", envir = srodowisko),
                         by = c("id_abs", "rok_abs")),
             envir = srodowisko)
    }
    lRekordowPo <- sapply(tabelePosrednie,
                          function(x, srodowisko) {
                            nrow(get(x, envir = srodowisko))
                          },
                          srodowisko = srodowisko)
    message("W wyniku zawężenia do wskazanych roczników (",
            paste(lataAbs, collapse = ", "),
            ") liczba rekordów w tabelach zmieniła się w następujący sposób:\n",
            paste0("- ", tabelePosrednie, ": ", format(lRekordowPrzed, big.mark = " "),
                   " -> ", format(lRekordowPo, big.mark = " "),
                   collapse = ";\n"), ".")
    if (sufiks != '') message("Zaweżone dane zostały zapisane w obiektach:\n",
                              paste0(tabelePosrednie, sufiks, collapse = ", "), ".")
    invisible(paste0(tabelePosrednie, sufiks))
  } else {
    stopifnot("p4" %in% names(tabelePosrednie),
              !any(duplicated(names(tabelePosrednie))),
              "rok_abs" %in% names(tabelePosrednie$p4),
              is.numeric(tabelePosrednie$p4$rok_abs),
              !anyNA(tabelePosrednie$p4$rok_abs),
              "id_abs" %in% names(tabelePosrednie$p4), !anyNA(tabelePosrednie$p4$id_abs),
              all(sapply(tabelePosrednie, is.data.frame)))
    lataAbsWDanych = intersect(lataAbs, tabelePosrednie$p4$rok_abs)
    if (length(lataAbsWDanych) == 0L) {
      stop("W przekazanych danych, w tabeli 'p4' nie ma rekordów opisujących podane roczniki absolwentów.")
    }
    if (length(lataAbsWDanych) < length(lataAbs)) {
      warning("W przekazanych danych, w tabeli 'p4' nie występują wartości `rok_abs` równe: ",
              paste(sort(setdiff(lataAbs, lataAbsWDanych)), collapse = ","), ".")
    }
    tabeleDoZawezenia <-
      sapply(tabelePosrednie,
             function(x) all(c("id_abs", "rok_abs") %in% names(x))) &
      names(tabelePosrednie) != "p4"
    message("Zaweżone zostaną dane w następujących tabelach pośrednich (w których występują kolumny `id_abs` i `rok_abs`):\n",
            paste(c("p4", names(tabelePosrednie)[tabeleDoZawezenia]), collapse = ", "), ".")
    lRekordowPrzed <- sapply(tabelePosrednie, nrow)
    tabelePosrednie$p4 <- tryCatch(
      tabelePosrednie$p4 %>%
        filter(.data$rok_abs %in% lataAbs,
               ...),
      error = function(e) stop("Przy próbie odfiltrowania rekordów w tabeli pośredniej p4:\n", e,
                               call. = FALSE))
    tabelePosrednie[tabeleDoZawezenia] <- lapply(tabelePosrednie[tabeleDoZawezenia],
                                                 semi_join,
                                                 y = tabelePosrednie$p4,
                                                 by = c("id_abs", "rok_abs"))
    lRekordowPo <- sapply(tabelePosrednie, nrow)
    message("W wyniku zawężenia do wskazanych roczników (",
            paste(lataAbs, collapse = ", "),
            ") liczba rekordów w tabelach zmieniła się w następujący sposób:\n",
            paste0("- ", names(tabelePosrednie), ": ", format(lRekordowPrzed, big.mark = " "),
                   " -> ", format(lRekordowPo, big.mark = " "),
                   collapse = ";\n"), ".")
    return(tabelePosrednie)
  }
}
