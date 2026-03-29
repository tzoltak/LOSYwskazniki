#' @title Zapisywanie na dysku wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja zapisuje na dysku pliki z wartościami wskaźników obliczonych przez
#' [oblicz_wskazniki_pd_jst()]) - obecnie jedynym odstępnym formatem są pliki
#' JSON.
#' @param x ramka danych zwrócona przez funkcję [oblicz_wskazniki_pd_jst()]
#' @param katalogZapis opcjonalnie ciąg znaków z nazwą katalogu, do którego
#' mają zostać zapisane pliki; domyślnie bieżący katalog roboczy
#' @param format opcjonalnie ciąg znaków wskazujący format zapisu plików -
#' obecnie jedyna możliwa wartość to "JSON"
#' @inheritParams przygotuj_wskazniki_pd_toJSON
#' @param usunKombinacjeBezAbs wartość logiczna - czy przed zapisem do plików
#' z danych powinny zostać usunięte wiersze opisujące kombinacje wartości
#' zmiennych grupujących, dla których nie wystąpił ani jeden abolwent
#' @param zmTERYT opcjonalnie nazwa zmiennej zawierającej identyfikatory JST
#' (kody TERYT); jeśli zostanie tu podany wektor zerowej długości, funkcja
#' przyjmie, że przekazane dane to wyniki ogólnopolskie
#' @param przeliczNaProcenty opcjonalnie wartość logiczna - czy przeliczać
#' wartości wskaźników na procenty? (domyślnie `TRUE`)
#' @param wykluczZprzeliczaniaNaProcenty opcjonalnie wektor ciągów znaków
#' z nazwami elementów wartości wskaźników, które wskazują, że dana wartość
#' **nie** powinna zostać przeliczona na procenty (domyślnie "średnia",
#' "liczba" i "suma")
#' @param pretty opcjonalnie wartość logiczna - czy zapisywane pliki JSON
#' mają być sformatowane w sposób ułatwiający ich czytanie przez człowieka,
#' tzn. z łamaniem do wielu linii i wcięciami? (domyślnie `FALSE`, w celu
#' zmniejszenia wielkości zapisywanych plików)
#' @param wyswietlPostep opcjonalnie wartość logiczna - czy wyświetlać pasek
#' postępu obrazujący postęp zapisu? (domyślnie `TRUE`)
#' @returns Funkcja zwraca *niewidocznie* wektor nazw zapisanych plików.
#' @details
#' Funkcja została napisana tak, aby miała możliwie niewielki ślad pamięciowy,
#' gdyż zestawienia przygotowane przy pomocy [oblicz_wskazniki_pd_jst()] mogą
#' być ekstremalnie duże (jeśli tylko uzyje się nieco szerszego zestawu
#' zmiennych grupujących).
#'
#' Wewnątrz wykorzystywana jest funkcje [przygotuj_wskazniki_pd_toJSON()],
#' a następnie [jsonlite::toJSON()].
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr %>% .data any_of filter select
#' @export
zapisz_wskazniki_pd_jst <- function(x, katalogZapis = "./", format = "JSON",
                                    komunikatCenzura = eval(formals(przygotuj_wskazniki_pd_toJSON)$komunikatCenzura),
                                    usunKombinacjeBezAbs = TRUE,
                                    komunikatBrakDanych = eval(formals(przygotuj_wskazniki_pd_toJSON)$komunikatBrakDanych),
                                    zmTERYT = intersect(c("teryt_woj_szk", "teryt_pow_szk"),
                                                        names(x)),
                                    przeliczNaProcenty = TRUE,
                                    wykluczZprzeliczaniaNaProcenty = c("średnia", "liczba", "suma"),
                                    pretty = FALSE, wyswietlPostep = TRUE) {
  format = match.arg(format, several.ok = FALSE)
  stopifnot(is.data.frame(x), "wskaznik" %in% names(x), "wartosc" %in% names(x),
            is.character(katalogZapis), length(katalogZapis) == 1L,
            !anyNA(katalogZapis),
            is.character(komunikatBrakDanych), length(komunikatBrakDanych) == 1L,
            is.character(komunikatCenzura), length(komunikatCenzura) == 1L,
            is.logical(usunKombinacjeBezAbs), length(usunKombinacjeBezAbs) == 1L,
            usunKombinacjeBezAbs %in% c(FALSE, TRUE),
            is.logical(przeliczNaProcenty), length(przeliczNaProcenty) == 1L,
            przeliczNaProcenty %in% c(FALSE, TRUE),
            is.character(wykluczZprzeliczaniaNaProcenty),
            !anyNA(wykluczZprzeliczaniaNaProcenty),
            is.logical(pretty), length(pretty) == 1L,
            pretty %in% c(FALSE, TRUE),
            is.logical(wyswietlPostep), length(wyswietlPostep) == 1L,
            wyswietlPostep %in% c(FALSE, TRUE),
            is.character(zmTERYT), length(zmTERYT) <= 1L, !anyNA(zmTERYT))
  katalogZapis <- paste0(sub("[/\\\\]$", "", katalogZapis), "/")
  if (!dir.exists(katalogZapis)) dir.create(katalogZapis, recursive = TRUE)
  if (length(zmTERYT) == 0L) {
    wszystkieJST <- "000000"
  } else {
    wszystkieJST <- unique(x[[zmTERYT]])
  }
  wszystkieWsk <- unique(x$wskaznik)
  wszystkieKombinacje <- expand.grid(jst = wszystkieJST, wsk = wszystkieWsk)
  wszystkieKombinacje$jstZapis <- gsub(" ", "0", format(wszystkieKombinacje$jst))
  wszystkieKombinacje$jstZapis <-
    do.call(paste0,
            append(list(wszystkieKombinacje$jstZapis),
                   rep(list("0"),
                       max(0, 6 - max(nchar(wszystkieKombinacje$jstZapis))))))

  if (wyswietlPostep) pb <- txtProgressBar(0, nrow(wszystkieKombinacje),
                                           style = 3)
  for (i in seq_len(nrow(wszystkieKombinacje))) {
    temp <- x %>%
      filter(.data$wskaznik == wszystkieKombinacje$wsk[i])
    if (length(zmTERYT) == 1L) {
      temp <- temp %>%
        filter(.data[[zmTERYT]] == wszystkieKombinacje$jst[i])
    }
    if (usunKombinacjeBezAbs & nrow(temp) > 0) {
      temp <- temp |>
        filter(sapply(.data$wartosc, \(x) attributes(x)$lAbs != 0))
    }
    if (przeliczNaProcenty) {
      temp$wartosc <-
        lapply(temp$wartosc,
               function(x, wykluczZprzeliczaniaNaProcenty) {
                 x[!(names(x) %in% wykluczZprzeliczaniaNaProcenty)] <-
                   x[!(names(x) %in% wykluczZprzeliczaniaNaProcenty)] /
                   attributes(x)$lAbs
                 return(x)
               },
               wykluczZprzeliczaniaNaProcenty = wykluczZprzeliczaniaNaProcenty)
    }
    temp %>%
      select(-any_of(c("wskaznik", zmTERYT, "obszar", "nazwa_pow_szk",
                       "nazwa_woj_szk", "kod_zaw"))) |>
      przygotuj_wskazniki_pd_toJSON(komunikatCenzura = komunikatCenzura) %>%
      jsonlite::toJSON(dataframe = "rows", factor = "string", na = "null",
                       pretty = FALSE) %>%
      writeLines(paste0(katalogZapis, wszystkieKombinacje$jstZapis[i], "-",
                        wszystkieKombinacje$wsk[i], ".json"))
    if (wyswietlPostep) setTxtProgressBar(pb, i)
  }
  if (wyswietlPostep) close(pb)
  invisible(paste0(katalogZapis, wszystkieKombinacje$jstZapis, "-",
                   wszystkieKombinacje$wsk, ".json"))
}
