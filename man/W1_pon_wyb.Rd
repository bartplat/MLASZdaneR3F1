% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_poziom_zagregowany_3rm_1f.R
\name{W1_pon_wyb}
\alias{W1_pon_wyb}
\title{Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym}
\usage{
W1_pon_wyb(x)
}
\arguments{
\item{x}{ramka danych ze wskaźnikami na poziomie indywidualnym}
}
\value{
lista
}
\description{
Funkcja licząca wskaźnik \strong{ponowny wybór szkoły}: "Gdyby
teraz mogła Pani/ mógł Pan wybrać szkołę, to czy ponownie wybrał(a)by
Pani/Pan tę samą szkołę, w której obecnie się Pani/Pan uczy?".
Wskaźnik jest rozkładem odpowiedzi:
\itemize{
\item{\code{t2b}: "Raczej tak" + "Na pewno tak" - kod 4 + kod 5}
\item{\code{nap_nie}: "Na pewno nie" - kod 1}
\item{\code{r_nie}: "Raczej nie" - kod 2}
\item{\code{r_tak}: "Raczej tak" - kod 4}
\item{\code{nap_tak}: "Na pewno tak" - kod 5}
\item{\code{trud_pow}: "Trudno powiedzieć" - kod 7}
}
}
