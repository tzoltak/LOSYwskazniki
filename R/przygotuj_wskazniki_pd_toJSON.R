#' @title Agregowanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja przekształca wskaźniki obliczone przez funkcję [oblicz_wskazniki_pd()]
#' (a więc również przez [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd_jst()]) do formatu, który będzie przyjazny zapisaniu
#' ich w formacie JSON przy pomocy funkcji `toJSON()` z pakietu *jsonlite*.
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @param komunikatBrakDanych opcjonalnie ciąg znaków - komunikat, który ma
#' zostać zapisany, jako element "brakDanych" (elementy "w" i "a" nie zostaną
#' wtedy zapisane), dla tych wskaźników, dla których nie było żadnych
#' absolwentów, na podstawie których można by obliczyć wskaźnik (tj. atrybut
#' `lAbs` równy jest równy zero lub atrybut `lSzk` jest równy zero);
#' jeśli taki atrybut nie ma być dopisywany, należy argumentem przekazać wartość
#' `NA_character_`
#' @param komunikatCenzura opcjonalnie ciąg znaków - komunikat, który ma
#' zostać zapisany, jako element "cenzura" (elementy "w" i "a" nie zostaną wtedy
#' zapisane), dla tych wskaźników, które zostały *ocenzurowane*/zanonimizowane
#' (tj. ich atrybut `lAbs` jest mniejszy od zera lub ich atrybut `lSzk` jest
#' mniejszy od zera);
#' podany ciag znaków może zawierać kody specjalne "\{lAbs\}" i "\{lSzk\}", za
#' które podstawione zostaną odpowiednie wartości progów użytych podczas
#' anonimizacji (odczytane z atrybutów `lAbs` i `lSzk`);
#' jeśli taki atrybut nie ma być dopisywany, należy argumentem przekazać wartość
#' `NA_character_`
#' @returns Ramka danych przekazana argumentem `x`, w której wskaźniki zostały
#' przekształcone na zagnieżdżone listy. Każdy element kolumny-listy
#' zawierającej wskaźniki jest listą zawierającą dwa elementy:
#'
#' -  `w`, zawierający ramkę danych z wartościami wskaźnika, utworzoną w ten
#'    sposób, że kolumna `label` zawiera nazwy elementów wejściowego wektora,
#'    a kolumna `value` ich wartości,
#' -  `a`, zawierający atrybuty wskaźnika.
#'
#' Jeśli jednak argumentami `komunikatBrakDanych` i `komunikatCenzura` nie
#' podano wartości `NA_character_` i dla danego wskaźnika jest spełniony
#' warunek braku danych lub zanonimizowania jego wartości, to dany element
#' będzie listą jednoelementową, zawierającą jedynie element odpowiednio
#' `brakDanych` albo `cenzura`, będących ciągiem znaków (nie będzie jednak
#' zawierał ani elementu `w`, ani elementu `a`).
#' @seealso [zanonimizuj_wskazniki_pd()]
#' @export
przygotuj_wskazniki_pd_toJSON <-
  function(x,
           komunikatBrakDanych = "W danych nie ma absolwentów pasujących do podanych kryteriów.",
           komunikatCenzura = "W danych było mniej niż {lAbs} absolwentów pasujących do podanych kryteriów lub ukończyli oni mniej niż {lSzk} różne szkoły, w związku z czym wyniki nie mogą zostać pokazane.") {
  stopifnot(is.data.frame(x),
            is.character(komunikatBrakDanych), length(komunikatBrakDanych) == 1L,
            is.character(komunikatCenzura), length(komunikatCenzura) == 1L)
  x <-
    mutate(x,
           across(where(is.list),
                  ~lapply(.,
                          function(x, komunikatBrakDanych, komunikatCenzura) {
                            a <- attributes(x)
                            a <- a[names(a) %in% c("lAbs", "lSzk",
                                                   "lNieDotyczy",
                                                   "lZadenZWymienionych")]
                            if (length(a) == 0L) return(x)

                            if (is(x, "vector")) {
                              if(!is.null(names(x))) {
                                x <- data.frame(label = names(x),
                                                value = unname(x))
                              }
                            } else if (is.array(x)) {
                              x <- as.data.frame(x, responseName = "value")
                              if (ncol(x) == 2L) names(x)[1] <- "label"
                            }

                            if (!is.null(a$lAbs) & !is.null(a$lSzk)) {
                              if ((a$lAbs == 0 | a$lSzk == 0) &
                                  !is.na(komunikatBrakDanych)) {
                                a$brakDanych = komunikatBrakDanych
                              } else if ((a$lAbs < 0 | a$lSzk < 0) &
                                         !is.na(komunikatCenzura)) {
                                a$cenzura =
                                  gsub("{lAbs}", -a$lAbs,
                                       gsub("{lSzk}", -a$lSzk, komunikatCenzura,
                                            fixed = TRUE),
                                       fixed = TRUE)
                              }
                              if (any(c("brakDanych", "cenzura") %in% names(a))) {
                                return(a[names(a) %in% c("brakDanych", "cenzura")])
                              }
                            }

                            return(list(w = x, a = a))
                          },
                          komunikatBrakDanych = komunikatBrakDanych,
                          komunikatCenzura = komunikatCenzura)))
  return(x)
}
