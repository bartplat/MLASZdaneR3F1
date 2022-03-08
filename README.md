![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

# MLASZdaneR3F1

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój.

Pakiet `MLASZdaneR3F1` zawiera zbiór funkcji służących do obliczania wskaźników dla 1. fali 3. rundy Monitoringu Losów Absolwentów. Runda ta została zrealizowana za pomocą badania sondażowego metodą CAWI. Przy pomocy tego pakietu oraz drugiego, będącego silnikiem agregacji - [`MLASZdane`](https://github.com/bartplat/MLASZdane), można tworzyć zbiory wskaźników na zadanym poziomie agregacji.

# Instalacja / aktualizacja

Pakiet nie jest dostępny na CRAN, więc trzeba instalować go ze źródeł.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('bartplat/MLASZdaneR3F1', build_opts = c("--no-resave-data"))
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

Pakiet `MLASZdaneR3F1` jest zależny od pakietu `MLASZdane`, ale nie ma potrzeby go dodatkowo instalować, ponieważ dzieje się to podczas instalacji pakietu `MLASZdaneR3F1`.

# Użycie

Jako, że pakiet `MLASZdaneR3F1` zawiera zbiór funkcji służących do liczenia wskaźników dla specyficznych danych w specyficzny sposób, poniżej opisano kolejne kroki, które należy przejść, aby otrzymać zbiór wskaźników zagregowanych.

## Definicja grup odniesienia

Do funkcji agregujących, jako jeden z argumentów, należy podać ramkę danych będącą definicją grupy oraz grupy odniesienia, czyli de facto poziomy agregacji dla grup prezentowanych w raportach oraz ich grup odniesienia. Można to osiągnąć na 2 sposoby: albo za pomocą funkcji `utworz_grupowanie_ze_zmiennej()` z pakietu [`MLASZdane`](https://github.com/bartplat/MLASZdane) albo samodzielnie. Istotne jest, aby wynikiem była ramka danych zawierająca kolumny o wymaganych nazwach. Pierwsza opcja ma jednak pewne ograniczenia: grupę odniesienia można definiować tylko za pomocą jednej nazwy zmiennej. W przypadku potrzeby użycia więcej niż jednej zmiennej ramkę danych zawierającą definicje grup i grup odniesienia należy przygotować samodzielnie.

Dodatkowo, ze względu na specyfikę danych, należy za pomocą funkcji `zap_labels()` z pakietu `haven` usunąć etykiety danych.

```r
library(dplyr)
library(haven)
grupy_szkoly = cawi %>%
  select(id_rspo, typ_szk, woj_szk) %>%
  mutate(across(everything(), ~zap_labels(.))) %>% 
  mutate(grupa = paste0("id_rspo %in% ", id_rspo),
         odniesienie = paste0("!(id_rspo %in% ", id_rspo, ") & ",
                              "(woj_szk %in% \"", woj_szk, "\") & ",
                              "(typ_szk %in% ", typ_szk, ")")) %>% 
  distinct()
```

Powyższy kod tworzy dwie definicje grup:

1. Prezentowaną grupę - w tym przypadku jest to szkoła (określona numerem RSPO) oraz
2. Grupę odniesienia będącą **pozostałymi** (czyli z wykluczeniem z grupy odniesienia prezentowanej szkoły) szkołami z danego województwa, dla danego typu szkoły

## Obliczanie wskaźników na poziomie zagregowanym

Ostatnim krokiem, mając już ramkę danych z definicja grup odniesienia, jest wygenerowanie wskaźników na danym poziomie agregacji.

```r
library(MLASZdaneR3F1)
wskazniki = agreguj_cawi_r3_f1(cawi, grupy_szkoly)

szk = wskazniki$grupy
woj = wskazniki$grupyOdniesienia
```

Wynikiem działania funkcji `agreguj_cawi_r3_f1()` jest lista o dwóch elementach: ramkach danych zagregwanych dla prezentowanej grupy oraz dla jej grupy odniesienia.
