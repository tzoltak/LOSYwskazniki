#' @title Przeksztalcanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala *spłaszczyć* strukturę, w jakiej przechowywane są wskaźniki
#' zagregowane do postaci (potencjalnie bardzo) szerokiej ramki danych.
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @param dopiszAtrybuty wektor ciągów znaków podający nazwy atrybutów
#' wskaźników, które mają zostać dodane jako dodatkowe kolumny do tworzonej
#' *płaskiej* reprezentacji lub "nie" aby wskazać, że takie kolumny nie mają
#' być tworzone
#' @returns Ramka danych przekazana argumentem `x`, w której wszystkie wskaźniki
#' zostały *rozwinięte* w kolumny.
#' @details
#' Funkcja przekształca **wszystko, co *wyglądają jej na zagregowany wskaźnik
#' przeznaczony do publicznej prezentacji***, co w praktyce oznacza, że
#' sprawdzane są elementy kolumn-list w ramce danych przekazanej argumentem `x`.
#'
#' Jeśli argument `dopiszZadenZWymienionych=TRUE`, przed dokonaniem opisanego
#' wyżej przekształcenia, wartość atrybutu `lZadenZWymienionych`
#' (o ile istnieje) zostanie dopisana jako dodatkowy element na końcu wskaźnika.
#' @importFrom methods is
#' @importFrom utils hasName
#' @importFrom dplyr %>% across all_of bind_rows mutate n_distinct
#' @importFrom tidyr pivot_wider
#' @export
splaszcz_wskazniki_pd <- function(x, dopiszAtrybuty = c("nie", "lAbs", "lSzk",
                                                        "lZadenZWymienionych",
                                                        "lNieDotyczy")) {
  dopiszAtrybuty <- match.arg(dopiszAtrybuty, several.ok = TRUE)
  if (any(dopiszAtrybuty == "nie")) {
    if (length(dopiszAtrybuty) > 0L) {
      warning("Elementy przekazane argumentem `dopiszAtrybuty` inne niż 'nie' zostały zignorowane.")
    }
    dopiszAtrybuty <- vector(mode = "character", length = 0L)
  }
  stopifnot(is.data.frame(x))


  if (all(c("czas", "wskaznik", "wartosc") %in% names(x))) {
    x <- x %>%
      mutate(wskaznik = paste0(.data$wskaznik,
                               ifelse(grepl("^[^[:digit:]-]", .data$czas),
                                      "_", ""), .data$czas)) %>%
      select(-"czas")
  } else if (hasName(x, "czas")) {
    x$czas <- ifelse(grepl("^[^[:digit:]-]", x$czas),
                     paste0("_", x$czas), x$czas)
  }
  if (all(c("wskaznik", "wartosc") %in% names(x)) & !hasName(x, "czas")) {
    kolumnyWskazniki <- unique(x$wskaznik)
    x <- pivot_wider(x, names_from = "wskaznik", values_from = "wartosc")
  }
  kolumnyWskazniki <- get0("kolumnyWskazniki",
                           ifnotfound = names(x)[sapply(x, is.list)])
  if (hasName(x, "czas")) {
    kolumnyWskazniki2 <- levels(interaction(kolumnyWskazniki, unique(x$czas),
                                            sep = ""))
    x <- pivot_wider(x, names_from = "czas", names_glue = "{.value}{czas}",
                      values_from = all_of(kolumnyWskazniki)) %>%
      select(-where(~all(sapply(., is.null))))
    kolumnyWskazniki <- intersect(names(x), kolumnyWskazniki2)
  }
  x <- x %>%
    mutate(across(all_of(kolumnyWskazniki),
                  ~bind_rows(lapply(
                    .,
                    \(w, dopiszAtrybuty) {
                      a <-  attributes(w)[intersect(dopiszAtrybuty,
                                                    names(attributes(w)))]
                      if (is.array(w)) {
                        w <- as.data.frame(as.table(w))
                        n <- do.call(paste,
                                     args = w[, rev(seq_len(ncol(w) - 1L)),
                                              drop = FALSE])
                        w <- w[, ncol(w)]
                        names(w) <- n
                      }
                      if (is(w, "vector")) { # is.vector nie działa, bo `w` co do zasady ma atrybuty
                        w <- c(w, unlist(a))
                        return(as.data.frame(as.list(w),
                                             check.names = FALSE))
                      } else {
                        print(str(w))
                        warning("Nie wiadomo jak dokonać spłaszczenia.")
                        return(data.frame(x = NA)[, c(), drop = FALSE]) # ramka danych z zerową liczbą kolumn oraz 1 wierszem
                      }
                    },
                    dopiszAtrybuty = dopiszAtrybuty))
                  ))
  for (k in kolumnyWskazniki) {
    names(x[[k]]) <- paste(k, names(x[[k]]), sep = ".")
  }
  return(unnest(x, kolumnyWskazniki))
}
