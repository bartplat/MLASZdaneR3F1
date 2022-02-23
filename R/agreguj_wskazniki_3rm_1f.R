#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych z wynikami ankiety CAWI.
#' @param wskazniki ramka danych z wynikami 3. rundy monitoringu
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'   \item{\code{\link{dane_szkoly}},}
#'   \item{\code{\link{firma_bad}},}
#'   \item{\code{\link{l_abs}},}
#'   \item{\code{\link{l_abs_WT}},}
#'   \item{\code{\link{l_kobiet}},}
#'   \item{\code{\link{l_kobiet_WT}},}
#'   \item{\code{\link{formy}},}
#'   \item{\code{\link{zawod_licz}},}
#'   \item{\code{\link{zawod_licz_WT}},}
#'   \item{\code{\link{W1_pon_wyb}},}
#'   \item{\code{\link{W1_pon_wyb_WT}},}
#'   \item{\code{\link{W2_przyg_zaw}},}
#'   \item{\code{\link{W2_przyg_zaw_WT}},}
#'   \item{\code{\link{W2_zawod}},}
#'   \item{\code{\link{W2_zawod_WT}},}
#'   \item{\code{\link{NZ2_pnz_szk}},}
#'   \item{\code{\link{NZ2_pnz_szk_WT}},}
#'   \item{\code{\link{NZ2_pnz_prac}},}
#'   \item{\code{\link{NZ2_pnz_prac_WT}},}
#'   \item{\code{\link{PL1_plany_eduk}},}
#'   \item{\code{\link{PL1_plany_eduk_WT}},}
#'   \item{\code{\link{PL2_plany_praca}},}
#'   \item{\code{\link{PL2_plany_praca_WT}},}
#'   \item{\code{\link{PL5A_praca_branza}},}
#'   \item{\code{\link{PL5A_praca_branza_WT}},}
#'   \item{\code{\link{PL5B_praca_branza_plan}},}
#'   \item{\code{\link{PL5B_praca_branza_plan_WT}},}
#'   \item{\code{\link{PL1_poza}},}
#'   \item{\code{\link{PL1_poza_WT}}}
#' }
#' @export
#' @importFrom dplyr .data
agreguj_cawi_r3_f1 = function(wskazniki, grupy) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy))
  nazwy = c("id_rspo", "typ_szk", "woj_szk")
  sprawdz_nazwy(names(wskazniki), nazwy)

  wsk = agreguj_wskazniki(
    wskazniki, grupy,
    dane_szk = dane_szkoly(.data),
    firma = firma_bad(.data),
    l_ucz = l_abs(.data),
    l_ucz_WT = l_abs_WT(.data),
    l_kobiet = l_kobiet(.data),
    l_kobiet_WT = l_kobiet_WT(.data),
    formy = formy(.data),
    zawod_licz = zawod_licz(.data),
    zawod_licz_WT = zawod_licz_WT(.data),
    W1_pon_wyb = W1_pon_wyb(.data),
    W1_pon_wyb_WT = W1_pon_wyb_WT(.data),
    W2_przyg_zaw = W2_przyg_zaw(.data),
    W2_przyg_zaw_WT = W2_przyg_zaw_WT(.data),
    W2_zawod = W2_zawod(.data),
    W2_zawod_WT = W2_zawod_WT(.data),
    NZ2_pnz_szk = NZ2_pnz_szk(.data),
    NZ2_pnz_szk_WT = NZ2_pnz_szk_WT(.data),
    NZ2_pnz_prac = NZ2_pnz_prac(.data),
    NZ2_pnz_prac_WT = NZ2_pnz_prac_WT(.data),
    PL1_plany_eduk = PL1_plany_eduk(.data),
    PL1_plany_eduk_WT = PL1_plany_eduk_WT(.data),
    PL2_plany_praca = PL2_plany_praca(.data),
    PL2_plany_praca_WT = PL2_plany_praca_WT(.data),
    PL5A_praca_branza = PL5A_praca_branza(.data),
    PL5A_praca_branza_WT = PL5A_praca_branza_WT(.data),
    PL5B_praca_branza_plan = PL5B_praca_branza_plan(.data),
    PL5B_praca_branza_plan_WT = PL5B_praca_branza_plan_WT(.data),
    PL1_poza = PL1_poza(.data),
    PL1_poza_WT = PL1_poza_WT(.data)
  )

  return(wsk)
}
