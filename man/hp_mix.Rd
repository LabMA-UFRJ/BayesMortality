% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hp_mix.R
\name{hp_mix}
\alias{hp_mix}
\title{HP Model mixture}
\usage{
hp_mix (fit, mu_post, weights = NULL, mix_age,
 x0_prior, x0_post, max_age)
}
\arguments{
\item{fit}{Object of the class 'HP' fitted by the hp() function.}

\item{mu_post}{Vector with mortality rates used in the mix.}

\item{weights}{Positive vector specifying the weights used in the mix. The default value is NULL.}

\item{mix_age}{Positive vector specifying the age range in the mixture.}

\item{x0_prior}{Nonnegative number indicating the initial age of the fitted HP model.}

\item{x0_post}{Nonnegative number indicating the initial age of the mortality table provided by the user.}

\item{max_age}{Positive number indicating the final age in the mixture.}
}
\value{
Return new chains for qx.
}
\description{
This function mixes the fitted mortality table of a HP model with another mortality
table provided by the user.
}
\examples{
## Importing mortality data from USA
data(USA)

## Selecting the exposure and death count of the 2010 and 2013 male populations ranging from 0 to 90 years old
USA2010 = USA[USA$Year == 2010,]
x = 0:90
Ex = USA2010$Ex.Male[x+1]
Dx = USA2010$Dx.Male[x+1]

USA2013 = USA[USA$Year == 2013,]
Ex2 = USA2013$Ex.Male[x+1]
Dx2 = USA2013$Dx.Male[x+1]

## Fitting HP model for 2010 data and calculating the mortality rates of 2013
fit = hp(x = x, Ex = Ex, Dx = Dx,
         M = 40000, bn = 10000, thin = 30)
tx_2013 = 1 - exp(-Dx2/Ex2)

## Mixing fitted model and mortality rates of 2013:
mix <- hp_mix(fit, tx_2013, x0_prior = 0, x0_post = 0, mix_age = c(50,90))

## Getting the new estimated mortality table (after mixture):
qx_mix<- apply(mix, 2, median, na.rm = T)
qx_mix

}
