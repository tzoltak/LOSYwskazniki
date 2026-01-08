#' @title Funkcje pomocnicze
#' @description
#' Pozwala dodać do zestawu wartości danej zmiennej (wektora lub czynnika)
#' *wartość ogółem*.
#' @param x wektor lub czynnik
#' @param wartosc wartość, która ma zostać podmieniona w miejsce braków danych
#' (jednoelementowy wektor)
#' @param zawszeDodawaj opcjonalnie wartość logiczna - czy `wartosc` powinna
#' zostać dodana również wtedy, kiedy `x` ma tylko jedną unikalną wartość lub
#' jest czynnikiem o tylko jednym poziomie; domyślnie `TRUE`
#' @returns **Funkcja zawsze zwraca czynnik**, którego zestaw poziomów oprócz
#' wcześniej istniejących, lub - jeśli argumentem `x` został przekazany wektor -
#' zbiór wartości występujących w danych, został rozszerzony o wartość podaną
#' argumentem `wartosc` (chyba że `zawszeDodawaj=FALSE`).
#' @export
dodaj_wartosc_ogolem <- function(x, wartosc, zawszeDodawaj = TRUE) {
  stopifnot(is.vector(x) | is.factor(x),
            is.character(wartosc), length(wartosc) == 1L,
            !is.na(wartosc),
            is.logical(zawszeDodawaj), length(zawszeDodawaj) == 1L,
            zawszeDodawaj %in% c(FALSE, TRUE))
  if (is.factor(x)) {
    l <- levels(x)
  } else {
    l <- as.character(sort(unique(x)))
  }
  if (length(l) > 1L | zawszeDodawaj) {
    l <- unique(c(l, "Ogółem"))
  }
  return(factor(x, levels = l))
}
