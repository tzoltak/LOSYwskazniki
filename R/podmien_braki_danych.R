#' @title Funkcje pomocnicze
#' @description
#' Pozwala podmienić braki danych na podaną wartość. W odróżnieniu od wielu
#' innych podobnych funkcji **obsługuje również czynniki**.
#' @param x wektor lub czynnik
#' @param wartosc wartość, która ma zostać podmieniona w miejsce braków danych
#' (jednoelementowy wektor)
#' @param zawszeDodajPoziom opcjonalnie wartość logiczna - czy jeśli `x` jest
#' czynnikiem, to dodawać do zestawu jego poziomów `wartość` nawet wtedy, jeśli
#' w `x`, ani wśród jego poziomów, nie występują żadne braki danych? jeśli `x`
#' nie jest czynnikiem, argument jest ignorowany; domyślnie `TRUE`
#' @returns Obiekt przekazany argumentem `x` z podmienionymi brakami danych
#' (a w przypadku czynników również odpowiednio zmienionym zestawem poziomów
#' czynnika).
#' @export
podmien_braki_danych <- function(x, wartosc, zawszeDodajPoziom = TRUE) {
  stopifnot(is.vector(x) | is.factor(x),
            is.vector(wartosc), length(wartosc) == 1L, !is.na(wartosc),
            is.logical(zawszeDodajPoziom), length(zawszeDodajPoziom) == 1L,
            zawszeDodajPoziom %in% c(FALSE, TRUE))
  if (is.factor(x)) {
    l <- levels(x)
    if (anyNA(l)) {
      l[is.na(l)] <- wartosc
    } else if (zawszeDodajPoziom | anyNA(x)) {
      l <- c(l, wartosc)
    }
    x <- levels(x)[x]
    x[is.na(x)] <- wartosc
    x <- factor(x, unique(l))
  } else {
    x[is.na(x)] <- wartosc
  }
  return(x)
}
