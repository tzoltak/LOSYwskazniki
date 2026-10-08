#' @title Przeksztalcanie wskaznikow w nierozlacznych podgrupach do publicznej prezentacji
#' @description
#' Funkcja pozwala zmienić przekształcić wskaźniki zagregowane będące rozkładami
#' liczebności poprzez dopisanie do nich rozkładu częstości.
#' @param x ramka danych zawierająca (między innymi) kolumny ze zagregowanymi
#' wskaźnikami przeznaczonymi do publicznej prezentacji, typowo zwrócona przez
#' funkcje [oblicz_wskazniki_pd_jst()], [oblicz_wskazniki_pd_grupy()] lub
#' [oblicz_wskazniki_pd()]
#' @param dopiszZadenZWymienionych wartość logiczna - czy jeśli wskaźnik
#' posiada atrybut `lZadenZWymienionych`, to powinien on zostać dodany do
#' rozkładu (z nazwą "Żaden z wymienionych")
#' @returns Ramka danych przekazana argumentem `x`, w której wskaźniki będące
#' rozkładami (a przynajmniej na takie wyglądające) zostały uzupełnione
#' o częstości. Co do zasady przekształcony wskaźnik ma dotychczasowe nazwy
#' w wierszach, a w kolumnach rodzaj rozkładu.
#' @details
#' Funkcja przekształca **wszystko, co *wyglądają jej na zagregowany wskaźnik
#' przeznaczony do publicznej prezentacji będący rozkładem liczebności***, co
#' w praktyce oznacza, że sprawdzane są elementy kolumn-list w ramce danych
#' przekazanej argumentem `x`. Jeśli element takiej kolumny-listy jest tablicą
#' (*table*) lub ma atrybut `lZadenZWymienionych`, to zostanie uznany za
#' kwalifikujący się do przekształcenia. Zostanie on przekształcony w tablicę
#' (*array*) z dodanym dodatkowym wymiarem o nazwie "rozklad", którego dwa
#' elementy będą miały nazwy "N" i "Pct", gdzie element "N" będzie przechowywał
#' dotychczasową postać wskażnika, a element "Pct" dotychczasowe wartości
#' podzielone przez wartość atrybutu `lAbs`. Atrybuty `lAbs`, `lSzk`,
#' `lZadenZWymienionych` i `lNieDotyczy` zostaną w przekształconym wskażniku
#' bez zmian.
#'
#' Jeśli argument `dopiszZadenZWymienionych=TRUE`, przed dokonaniem opisanego
#' wyżej przekształcenia, wartość atrybutu `lZadenZWymienionych`
#' (o ile istnieje) zostanie dopisana jako dodatkowy element na końcu wskaźnika.
#' @importFrom utils hasName
#' @importFrom dplyr %>% across mutate where
#' @export
dodaj_procenty_wskazniki_pd <- function(x, dopiszZadenZWymienionych = FALSE) {
  stopifnot(is.data.frame(x),
            is.logical(dopiszZadenZWymienionych),
            length(dopiszZadenZWymienionych) == 1L,
            dopiszZadenZWymienionych %in% c(FALSE, TRUE))

  return(
    x %>%
      mutate(across(where(is.list),
                    ~lapply(.,
                            \(w, dopiszZadenZWymienionych) {
                              if (!hasName(attributes(w), "lAbs") |
                                  (!is.table(w) &
                                   !hasName(attributes(w),
                                            "lZadenZWymienionych"))) {
                                return(w)
                              }
                              if (dopiszZadenZWymienionych &
                                  hasName(attributes(w), "lZadenZWymienionych")) {
                                # dla zachowania atrybutów w ten sposób
                                w[length(w) + 1L] <-
                                  attributes(w)$lZadenZWymienionych
                                names(w)[length(w)] <- "Żaden z wymienionych"
                              }
                              pct <- w / attributes(w)$lAbs
                              a <- attributes(w)
                              if (is.array(w)) {
                                if ("rozklad" %in% names(dimnames(w))) {
                                  warning("Wskaznik jest już tablicą (array), której wymiar ma nazwę 'rokzlad'. Prawdopodobnie nie powinien on być (kolejny raz) procentowany.")
                                }
                                w <- array(c(w, pct), dim = c(dim(w), 2),
                                           dimnames = c(dimnames(w),
                                                        rozklad = list(c("N",
                                                                         "Pct"))))
                              } else {
                                w <- array(c(w, pct), dim = c(length(w), 2),
                                           dimnames = list(names(w),
                                                           rozklad = c("N", "Pct")))
                              }
                              attributes(w) <-
                                append(attributes(w),
                                       a[names(a) %in%
                                           c("lAbs", "lSzk", "lZadenZWymienionych",
                                             "lNieDotyczy")])
                              return(w)
                            },
                            dopiszZadenZWymienionych = dopiszZadenZWymienionych)))
    )
}
