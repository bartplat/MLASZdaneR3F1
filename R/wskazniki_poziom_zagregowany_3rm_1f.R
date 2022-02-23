#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja przechowująca nazwę i adres szkoły jako listę.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
dane_szkoly = function(x) {
  stopifnot(is.data.frame(x))

  list(szk_nazwa = unique(x$szk_nazwa),
       szk_adres = unique(x$szk_adres)) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja przechowująca informację o nazwie firmy realizującej
#' badanie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% summarise case_when n_distinct .data
firma_bad = function(x) {
  x = x %>%
    summarise(firma = case_when(n_distinct(.data$FIRMA) > 1 ~ "ndt.",
                                      all(.data$FIRMA %in% 1) ~ "PBS sp. z o.o.",
                                      all(.data$FIRMA %in% 2) ~ "Danae sp. z o.o."))
  return(x$firma)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych absolwentów
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% count pull
l_abs = function(x) {
  x %>%
    count(.data$plec) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza \strong{ważony} wskaźnik opisujący liczbę
#' zbadanych absolwentów w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% count pull
l_abs_WT = function(x) {
  x %>%
    count(.data$plec, wt = .data$waga) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych kobiet w
#' grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' #' @importFrom dplyr %>% filter count pull
l_kobiet = function(x) {
  x %>%
    filter(.data$plec %in% 1) %>%
    count(.data$plec) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza \strong{ważony} wskaźnik opisujący liczbę
#' zbadanych kobiet w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' #' @importFrom dplyr %>% filter count pull
l_kobiet_WT = function(x) {
  x %>%
    filter(.data$plec %in% 1) %>%
    count(.data$plec, wt = .data$waga) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja przechowująca listę form gramatycznych różnych słów lub
#' wyrażeń, które pojawiają się w raporcie w zależności od typu szkoły.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% .data case_when distinct mutate select
formy = function(x) {
  l_kobiet = x %>%
    filter(.data$plec %in% 1) %>%
    count(.data$plec) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    round()

  x %>%
    select(.data$woj_szk, .data$typ_szk) %>%
    distinct() %>%
    mutate(
      typ_szk_mian = case_when(
        .data$typ_szk %in% 1 ~ "branżowa szkoła pierwszego stopnia",
        .data$typ_szk %in% 2 ~ "technikum",
        .data$typ_szk %in% 3 ~ "szkoła policealna"),
      typ_szk_dop_lm = case_when(
        .data$typ_szk %in% 1 ~ "branżowych szkół pierwszego stopnia",
        .data$typ_szk %in% 2 ~ "techników",
        .data$typ_szk %in% 3 ~ "szkół policealnych"),
      woj_nazwa_dop = ifelse(length(unique(.data$woj_szk)) %in% 1,
                             paste0(.data$woj_szk, "go"),
                             "NA"),
      kobiet_a = case_when(
        l_kobiet %in% 0 ~ paste0("kobiet"),
        l_kobiet %in% 1 ~ paste0("kobieta"),
        l_kobiet %in% c(2:4) ~ paste0("kobiety"),
        l_kobiet %in% c(5:21) ~ paste0("kobiet"),
        l_kobiet > 21 & l_kobiet %% 10 %in% c(2:4) ~ paste0("kobiety"),
        l_kobiet > 21 & l_kobiet %% 10 %in% c(5:9, 0, 1) ~ paste0("kobiet")),
      byla_o = ifelse(.data$typ_szk %in% c(1, 3), "była", "było"),
      la_lo = ifelse(.data$typ_szk %in% c(1, 3), "ła", "ło")) %>%
    select(-c(.data$woj_szk, .data$typ_szk)) %>%
    distinct() %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja obliczająca liczbę absolwentów danego zawodu. Ponadto,
#' dodana jest informacja o branży, do której należy zawód.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% count
zawod_licz = function(x) {
  zaw = x %>%
    count(.data$zawod_nazwa) %>%
    as.list()

  return(zaw)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważoną }liczbę absolwentów danego
#' zawodu. Ponadto, dodana jest informacja o branży, do której należy zawód.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% count
zawod_licz_WT = function(x) {
  zaw = x %>%
    count(.data$zawod_nazwa, wt = .data$waga) %>%
    as.list()

  return(zaw)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{ponowny wybór szkoły}: "Gdyby
#' teraz mogła Pani/ mógł Pan wybrać szkołę, to czy ponownie wybrał(a)by
#' Pani/Pan tę samą szkołę, w której obecnie się Pani/Pan uczy?".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Raczej tak" + "Na pewno tak" - kod 4 + kod 5}
#'  \item{\code{nap_nie}: "Na pewno nie" - kod 1}
#'  \item{\code{r_nie}: "Raczej nie" - kod 2}
#'  \item{\code{r_tak}: "Raczej tak" - kod 4}
#'  \item{\code{nap_tak}: "Na pewno tak" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
W1_pon_wyb = function(x) {
  nka = sum(x$W1 %in% c(1:7), na.rm = TRUE)

  pon_wyb = list(
    n = nka,
    t2b = sum(x$W1 %in% c(4, 5), na.rm = TRUE) / nka,
    nap_nie = sum(x$W1 %in% 1, na.rm = TRUE) / nka,
    r_nie = sum(x$W1 %in% 2, na.rm = TRUE) / nka,
    r_tak = sum(x$W1 %in% 4, na.rm = TRUE) / nka,
    nap_tak = sum(x$W1 %in% 5, na.rm = TRUE) / nka,
    trud_pow = sum(x$W1 %in% 7, na.rm = TRUE) / nka)

  return(pon_wyb)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{ponowny wybór
#' szkoły}: "Gdyby teraz mogła Pani/ mógł Pan wybrać szkołę, to czy ponownie
#' wybrał(a)by Pani/Pan tę samą szkołę, w której obecnie się Pani/Pan uczy?".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Raczej tak" + "Na pewno tak" - kod 4 + kod 5}
#'  \item{\code{nap_nie}: "Na pewno nie" - kod 1}
#'  \item{\code{r_nie}: "Raczej nie" - kod 2}
#'  \item{\code{r_tak}: "Raczej tak" - kod 4}
#'  \item{\code{nap_tak}: "Na pewno tak" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
W1_pon_wyb_WT = function(x) {
  nka = sum(as.numeric(x$W1 %in% c(1:7)) * x$waga, na.rm = TRUE)

  pon_wyb = list(
    n = nka,
    t2b = sum(as.numeric(x$W1 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    nap_nie = sum(as.numeric(x$W1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    r_nie = sum(as.numeric(x$W1 %in% 2) * x$waga, na.rm = TRUE) / nka,
    r_tak = sum(as.numeric(x$W1 %in% 4) * x$waga, na.rm = TRUE) / nka,
    nap_tak = sum(as.numeric(x$W1 %in% 5) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$W1 %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(pon_wyb)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{ocena szkoły - porzygotowanie
#' do wykonywania zawodu}: "Jak  Pani/Pan uważa, na ile szkoła przygotowała
#' Panią/Pana do wykonywania zawodu, którego się Pani/Pan w niej uczy?".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{b2b}: "Nie przygotowała w ogóle" + "Przygotowała słabo" - kod 1
#'  + kod 2}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
W2_przyg_zaw = function(x) {
  nka = sum(x$W2 %in% c(1:5, 7), na.rm = TRUE)

  przyg_zaw = list(
    n = nka,
    t2b = sum(x$W2 %in% c(4, 5), na.rm = TRUE) / nka,
    b2b = sum(x$W2 %in% c(1, 2), na.rm = TRUE) / nka,
    wcale = sum(x$W2 %in% 1, na.rm = TRUE) / nka,
    slabo = sum(x$W2 %in% 2, na.rm = TRUE) / nka,
    srednio = sum(x$W2 %in% 3, na.rm = TRUE) / nka,
    dobrze = sum(x$W2 %in% 4, na.rm = TRUE) / nka,
    b_dob = sum(x$W2 %in% 5, na.rm = TRUE) / nka,
    trud_pow = sum(x$W2 %in% 7, na.rm = TRUE) / nka)

  return(przyg_zaw)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{ocena szkoły -
#' porzygotowanie do wykonywania zawodu}: "Jak  Pani/Pan uważa, na ile szkoła
#' przygotowała Panią/Pana do wykonywania zawodu, którego się Pani/Pan w niej
#' uczy?".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{b2b}: "Nie przygotowała w ogóle" + "Przygotowała słabo" - kod 1
#'  + kod 2}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
W2_przyg_zaw_WT = function(x) {
  nka = sum(as.numeric(x$W2 %in% c(1:5, 7)) * x$waga, na.rm = TRUE)

  przyg_zaw = list(
    n = nka,
    t2b = sum(as.numeric(x$W2 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$W2 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    wcale = sum(as.numeric(x$W2 %in% 1) * x$waga, na.rm = TRUE) / nka,
    slabo = sum(as.numeric(x$W2 %in% 2) * x$waga, na.rm = TRUE) / nka,
    srednio = sum(as.numeric(x$W2 %in% 3) * x$waga, na.rm = TRUE) / nka,
    dobrze = sum(as.numeric(x$W2 %in% 4) * x$waga, na.rm = TRUE) / nka,
    b_dob = sum(as.numeric(x$W2 %in% 5) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$W2 %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(przyg_zaw)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek uczniów oceniających, że szkoła
#' przygotowanła ich dobrze lub doskonale do wykonywania zawodu (\emph{W1}).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select filter count left_join
W2_zawod = function(x) {
  mapp = x %>%
    count(.data$zawod_nazwa, name = "mian")

  ocena = x %>%
    filter(.data$W2 %in% c(4, 5)) %>%
    count(.data$zawod_nazwa)

  zaw_tab = ocena %>%
    left_join(mapp) %>%
    mutate(ods = .data$n / .data$mian) %>%
    select(.data$zawod_nazwa, n = .data$mian, .data$ods)

  return(zaw_tab)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważony} odsetek uczniów
#' oceniających, że szkoła przygotowanła ich dobrze lub doskonale do wykonywania
#' zawodu (\emph{W1}).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select filter count left_join
W2_zawod_WT = function(x) {
  mapp = x %>%
    count(.data$zawod_nazwa, name = "mian", wt = .data$waga)

  ocena = x %>%
    filter(.data$W2 %in% c(4, 5)) %>%
    count(.data$zawod_nazwa, wt = .data$waga)

  zaw_tab = ocena %>%
    left_join(mapp) %>%
    mutate(ods = .data$n / .data$mian) %>%
    select(.data$zawod_nazwa, n = .data$mian, .data$ods) %>%
    mutate(across(.data$n, ~round(.)))

  return(zaw_tab)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{ocena praktycznej nauki zawodu
#' w szkole}: "Jak Pani / Pan ocenia, w jakim stopniu praktyczna nauka zawodu
#' przygotowała Panią/Pana do wykonywania pracy w zawodzie? - w szkole".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
NZ2_pnz_szk = function(x) {
  nka = sum(x$NZ2_3 %in% c(1:5, 7), na.rm = TRUE)

  pnz_szk = list(
    n = nka,
    t2b = sum(x$NZ2_3 %in% c(4, 5), na.rm = TRUE) / nka,
    wcale = sum(x$NZ2_3 %in% 1, na.rm = TRUE) / nka,
    slabo = sum(x$NZ2_3 %in% 2, na.rm = TRUE) / nka,
    srednio = sum(x$NZ2_3 %in% 3, na.rm = TRUE) / nka,
    dobrze = sum(x$NZ2_3 %in% 4, na.rm = TRUE) / nka,
    b_dob = sum(x$NZ2_3 %in% 5, na.rm = TRUE) / nka,
    trud_pow = sum(x$NZ2_3 %in% 7, na.rm = TRUE) / nka)

  return(pnz_szk)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{ocena
#' praktycznej nauki zawodu w szkole}: "Jak Pani / Pan ocenia, w jakim stopniu
#' praktyczna nauka zawodu przygotowała Panią/Pana do wykonywania pracy w
#' zawodzie? - w szkole".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
NZ2_pnz_szk_WT = function(x) {
  nka = sum(as.numeric(x$NZ2_3 %in% c(1:5, 7)) * x$waga, na.rm = TRUE)

  pnz_szk = list(
    n = nka,
    t2b = sum(as.numeric(x$NZ2_3 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    wcale = sum(as.numeric(x$NZ2_3 %in% 1) * x$waga, na.rm = TRUE) / nka,
    slabo = sum(as.numeric(x$NZ2_3 %in% 2) * x$waga, na.rm = TRUE) / nka,
    srednio = sum(as.numeric(x$NZ2_3 %in% 3) * x$waga, na.rm = TRUE) / nka,
    dobrze = sum(as.numeric(x$NZ2_3 %in% 4) * x$waga, na.rm = TRUE) / nka,
    b_dob = sum(as.numeric(x$NZ2_3 %in% 5) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$NZ2_3 %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(pnz_szk)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{ocena praktycznej nauki zawodu
#' w szkole}: "Jak Pani / Pan ocenia, w jakim stopniu praktyczna nauka zawodu
#' przygotowała Panią/Pana do wykonywania pracy w zawodzie? - u pracodawcy w
#' Polsce".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
NZ2_pnz_prac = function(x) {
  nka = sum(x$NZ2_1 %in% c(1:5, 7), na.rm = TRUE)

  pnz_prac = list(
    n = nka,
    t2b = sum(x$NZ2_1 %in% c(4, 5), na.rm = TRUE) / nka,
    wcale = sum(x$NZ2_1 %in% 1, na.rm = TRUE) / nka,
    slabo = sum(x$NZ2_1 %in% 2, na.rm = TRUE) / nka,
    srednio = sum(x$NZ2_1 %in% 3, na.rm = TRUE) / nka,
    dobrze = sum(x$NZ2_1 %in% 4, na.rm = TRUE) / nka,
    b_dob = sum(x$NZ2_1 %in% 5, na.rm = TRUE) / nka,
    trud_pow = sum(x$NZ2_1 %in% 7, na.rm = TRUE) / nka)

  return(pnz_prac)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{ocena
#' praktycznej nauki zawodu w szkole}: "Jak Pani / Pan ocenia, w jakim stopniu
#' praktyczna nauka zawodu przygotowała Panią/Pana do wykonywania pracy w
#' zawodzie? - u pracodawcy w Polsce".
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{\code{t2b}: "Przygotowała dobrze" + "Przygotowała bardzo dobrze" - kod
#'  4 + kod 5}
#'  \item{\code{wcale}: "Nie przygotowała w ogóle" - kod 1}
#'  \item{\code{slabo}: "Przygotowała słabo" - kod 2}
#'  \item{\code{srednio}: "Przygotowała średnio" - kod 3}
#'  \item{\code{dobrze}: "Przygotowała dobrze" - kod 4}
#'  \item{\code{b_dob}: "Przygotowała bardzo dobrze" - kod 5}
#'  \item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
NZ2_pnz_prac_WT = function(x) {
  nka = sum(as.numeric(x$NZ2_1 %in% c(1:5, 7)) * x$waga, na.rm = TRUE)

  pnz_prac = list(
    n = nka,
    t2b = sum(as.numeric(x$NZ2_1 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    wcale = sum(as.numeric(x$NZ2_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    slabo = sum(as.numeric(x$NZ2_1 %in% 2) * x$waga, na.rm = TRUE) / nka,
    srednio = sum(as.numeric(x$NZ2_1 %in% 3) * x$waga, na.rm = TRUE) / nka,
    dobrze = sum(as.numeric(x$NZ2_1 %in% 4) * x$waga, na.rm = TRUE) / nka,
    b_dob = sum(as.numeric(x$NZ2_1 %in% 5) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$NZ2_1 %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(pnz_prac)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{planowanie dalszej edukacji}
#' będący odsetkiem uczniów, którzy w pytaniu "PL1.: Proszę pomyśleć o swoich
#' planach na pierwszy rok po ukończeniu obecnej szkoły. Czy po zakończeniu
#' nauki w obecnej szkole planuje Pani/Pan:...", w podpunktach:
#' \itemize{
#'  \item{... uczyć się w szkole branżowej drugiego stopnia}
#'  \item{... uczyć się w liceum dla dorosłych}
#'  \item{... uczyć się w szkole policealnej}
#'  \item{... studiować}
#'  \item{... uczyć się na kwalifikacyjnym kursie zawodowym (KKZ), czyli kursie,
#'  który kończy się egzaminem zawodowym}
#' }
#' zaznaczyli chociaż jedną odpowiedź "Tak, taki mam plan" (kod 1.)
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL1_plany_eduk = function(x) {
  nka = sum(x$PL1_6 %in% c(1:3, 7), na.rm = TRUE)

  plany_eduk = list(
    n = nka,
    any1_5 = sum(
      x$PL1_1 %in% 1 |
        x$PL1_2 %in% 1 |
        x$PL1_3 %in% 1 |
        x$PL1_4 %in% 1 |
        x$PL1_5 %in% 1, na.rm = TRUE) / nka,
    bs2 = sum(x$PL1_1 %in% 1, na.rm = TRUE) / nka,
    lodd = sum(x$PL1_2 %in% 1, na.rm = TRUE) / nka,
    spolic = sum(x$PL1_3 %in% 1, na.rm = TRUE) / nka,
    stud = sum(x$PL1_4 %in% 1, na.rm = TRUE) / nka,
    kkz = sum(x$PL1_5 %in% 1, na.rm = TRUE) / nka,
    praca = sum(x$PL1_6 %in% 1, na.rm = TRUE) / nka)

  return(plany_eduk)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{planowanie
#' dalszej edukacji} będący odsetkiem uczniów, którzy w pytaniu "PL1.: Proszę
#' pomyśleć o swoich planach na pierwszy rok po ukończeniu obecnej szkoły. Czy
#' po zakończeniu nauki w obecnej szkole planuje Pani/Pan:...", w podpunktach:
#' \itemize{
#'  \item{... uczyć się w szkole branżowej drugiego stopnia}
#'  \item{... uczyć się w liceum dla dorosłych}
#'  \item{... uczyć się w szkole policealnej}
#'  \item{... studiować}
#'  \item{... uczyć się na kwalifikacyjnym kursie zawodowym (KKZ), czyli kursie,
#'  który kończy się egzaminem zawodowym}
#' }
#' zaznaczyli chociaż jedną odpowiedź "Tak, taki mam plan" (kod 1.)
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL1_plany_eduk_WT = function(x) {
  nka = sum(as.numeric(x$PL1_6 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  plany_eduk = list(
    n = nka,
    any1_5 = sum(as.numeric(
      x$PL1_1 %in% 1 |
        x$PL1_2 %in% 1 |
        x$PL1_3 %in% 1 |
        x$PL1_4 %in% 1 |
        x$PL1_5 %in% 1) * x$waga, na.rm = TRUE) / nka,
    bs2 = sum(as.numeric(x$PL1_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    lodd = sum(as.numeric(x$PL1_2 %in% 1) * x$waga, na.rm = TRUE) / nka,
    spolic = sum(as.numeric(x$PL1_3 %in% 1) * x$waga, na.rm = TRUE) / nka,
    stud = sum(as.numeric(x$PL1_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
    kkz = sum(as.numeric(x$PL1_5 %in% 1) * x$waga, na.rm = TRUE) / nka,
    praca = sum(as.numeric(x$PL1_6 %in% 1) * x$waga, na.rm = TRUE) / nka)

  return(plany_eduk)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{planowanie poszukiwania pracy}
#' będący odsetkiem uczniów, którzy w pytaniu "PL2.: Czy w pierwszym roku po
#' zakończeniu nauki w obecnej szkole planuje Pani/Pan poszukiwać pracy?".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL2_plany_praca = function(x) {
  nka = sum(x$PL2 %in% c(1:3) | x$PL1_6 %in% 3, na.rm = TRUE)

  plany_praca = list(
    n = nka,
    any = sum(
      x$PL2 %in% 1 |
        x$PL2 %in% 2 |
        x$PL2 %in% 3 |
        x$PL1_6 %in% 3, na.rm = TRUE) / nka,
    tak = sum(x$PL2 %in% 1, na.rm = TRUE) / nka,
    nie_praca = sum(x$PL2 %in% 2, na.rm = TRUE) / nka,
    nie_inne_plany = sum(x$PL2 %in% 3 | x$PL1_6 %in% 3, na.rm = TRUE) / nka)

  return(plany_praca)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{planowanie
#' poszukiwania pracy} będący odsetkiem uczniów, którzy w pytaniu "PL2.: Czy w
#' pierwszym roku po zakończeniu nauki w obecnej szkole planuje Pani/Pan
#' poszukiwać pracy?".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL2_plany_praca_WT = function(x) {
  nka = sum(as.numeric(x$PL2 %in% c(1:3) | x$PL1_6 %in% 3) * x$waga, na.rm = TRUE)

  plany_praca = list(
    n = nka,
    any = sum(
      as.numeric(x$PL2 %in% 1) |
        as.numeric(x$PL2 %in% 2) |
        as.numeric(x$PL2 %in% 3) |
        as.numeric(x$PL1_6 %in% 3) * x$waga, na.rm = TRUE) / nka,
    tak = sum(as.numeric(x$PL2 %in% 1) * x$waga, na.rm = TRUE) / nka,
    nie_praca = sum(as.numeric(x$PL2 %in% 2) * x$waga, na.rm = TRUE) / nka,
    nie_inne_plany = sum(as.numeric(x$PL2 %in% 3 | x$PL1_6 %in% 3) * x$waga, na.rm = TRUE) / nka)

  return(plany_praca)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{planowanie zatrudnienia w
#' branży}, czyli odsetek uczniów, którzy podejmą pracę w branży związanej
#' z wyuczonym zawodem w ciągu roku od ukończenia nauki - pytanie PL5A kod 1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL5A_praca_branza = function(x) {
  nka = sum(x$PL5A %in% c(1:3, 7), na.rm = TRUE)

  praca_branza = list(
    n = sum(x$PL5A %in% c(1:3, 7), na.rm = TRUE),
    praca_branza = sum(x$PL5A %in% 1, na.rm = TRUE) / nka,
    praca_zwiazana_branza = sum(x$PL5A %in% 2, na.rm = TRUE) / nka,
    inna_branza = sum(x$PL5A %in% 3, na.rm = TRUE) / nka,
    trud_pow = sum(x$PL5A %in% 7, na.rm = TRUE) / nka)

  return(praca_branza)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{planowanie
#' zatrudnienia w branży}, czyli odsetek uczniów, którzy podejmą pracę w branży
#' związanej z wyuczonym zawodem w ciągu roku od ukończenia nauki - pytanie PL5A
#' kod 1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL5A_praca_branza_WT = function(x) {
  nka = sum(as.numeric(x$PL5A %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  praca_branza = list(
    n = sum(x$PL5A %in% c(1:3, 7), na.rm = TRUE),
    praca_branza = sum(as.numeric(x$PL5A %in% 1) * x$waga, na.rm = TRUE) / nka,
    praca_zwiazana_branza = sum(as.numeric(x$PL5A %in% 2) * x$waga, na.rm = TRUE) / nka,
    inna_branza = sum(as.numeric(x$PL5A %in% 3) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PL5A %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(praca_branza)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{planowanie zatrudnienia w
#' branży}, czyli odsetek uczniów, którzy planują podjąć pracę w branży
#' związanej z wyuczonym zawodem w ciągu roku od ukończenia nauki - pytanie
#' PL5B_1 kod 1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL5B_praca_branza_plan = function(x) {
  nka = sum(x$PL5B_1 %in% c(1:3, 7), na.rm = TRUE)

  praca_branza_plan = list(
    n = nka,
    praca_branza_plan = sum(x$PL5B_1 %in% 1, na.rm = TRUE) / nka,
    rozwaza = sum(x$PL5B_1 %in% 2, na.rm = TRUE) / nka,
    nie = sum(x$PL5B_1 %in% 3, na.rm = TRUE) / nka,
    trud_pow = sum(x$PL5B_1 %in% 7, na.rm = TRUE) / nka)

  return(praca_branza_plan)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{planowanie
#' zatrudnienia w branży}, czyli odsetek uczniów, którzy planują podjąć pracę w
#' branży związanej z wyuczonym zawodem w ciągu roku od ukończenia nauki -
#' pytanie PL5B_1 kod 1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL5B_praca_branza_plan_WT = function(x) {
  nka = sum(as.numeric(x$PL5B_1 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  praca_branza_plan = list(
    n = nka,
    praca_branza_plan = sum(as.numeric(x$PL5B_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    rozwaza = sum(as.numeric(x$PL5B_1 %in% 2) * x$waga, na.rm = TRUE) / nka,
    nie = sum(as.numeric(x$PL5B_1 %in% 3) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PL5B_1 %in% 7) * x$waga, na.rm = TRUE) / nka)

  return(praca_branza_plan)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik \strong{pozostawanie poza edukacją i
#' rynkiem pracy}, czyli odsetki i liczebności uczniów, którzy pozostają poza
#' edukacją i rynkiem pracy - na podstawie pytania PL1
#' (\code{\link{PL1_plany_eduk_WT}})
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL1_poza = function(x) {
  nka = sum(x$PL1_6 %in% c(1:3, 7), na.rm = TRUE)

  poza = list(
    n = nka,
    n_poza = sum(
      x$PL1_1 %in% c(3, 7) &
        x$PL1_2 %in% c(3, 7) &
        x$PL1_3 %in% c(3, 7) &
        x$PL1_4 %in% c(3, 7) &
        x$PL1_5 %in% c(3, 7) &
        x$PL1_6 %in% c(3, 7), na.rm = TRUE),
    ods_poza = sum(
      x$PL1_1 %in% c(3, 7) &
        x$PL1_2 %in% c(3, 7) &
        x$PL1_3 %in% c(3, 7) &
        x$PL1_4 %in% c(3, 7) &
        x$PL1_5 %in% c(3, 7) &
        x$PL1_6 %in% c(3, 7), na.rm = TRUE) / nka)

  return(poza)
}
#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja licząca \strong{ważony} wskaźnik \strong{pozostawanie
#' poza edukacją i rynkiem pracy}, czyli odsetki i liczebności uczniów, którzy
#' pozostają poza edukacją i rynkiem pracy - na podstawie pytania PL1
#' (\code{\link{PL1_plany_eduk_WT}})
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
PL1_poza_WT = function(x) {
  nka = sum(as.numeric(x$PL1_6 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  poza = list(
    n = sum(x$PL1_6 %in% c(1:3, 7), na.rm = TRUE),
    n_poza = sum(
      as.numeric(x$PL1_1 %in% c(3, 7)) &
        as.numeric(x$PL1_2 %in% c(3, 7)) &
        as.numeric(x$PL1_3 %in% c(3, 7)) &
        as.numeric(x$PL1_4 %in% c(3, 7)) &
        as.numeric(x$PL1_5 %in% c(3, 7)) &
        as.numeric(x$PL1_6 %in% c(3, 7)) * x$waga, na.rm = TRUE),
    ods_poza = sum(
      as.numeric(x$PL1_1 %in% c(3, 7)) &
        as.numeric(x$PL1_2 %in% c(3, 7)) &
        as.numeric(x$PL1_3 %in% c(3, 7)) &
        as.numeric(x$PL1_4 %in% c(3, 7)) &
        as.numeric(x$PL1_5 %in% c(3, 7)) &
        as.numeric(x$PL1_6 %in% c(3, 7)) * x$waga, na.rm = TRUE) / nka)

  return(poza)
}
