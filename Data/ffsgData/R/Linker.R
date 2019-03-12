#' Vaughn Johnson
#' 2019-02-19
#' Record linker
#'
#' This package always assumes you're in
#' the top level directory for the package
#' ie, where ffsg.Rproj is kept

library(dplyr)
library(here)
library(fastLink)

path_to_src = here::here(file.path('R'))
load(file.path(path_to_src,
               "HarmonizedFiles",
               "HarmonizedDataSets.RData"))

#' First, we deduplicate

## Not run:
## -------------
## Run on subset
## -------------
dfA.s <- dfA[sample(1:nrow(dfA), 50),]
dfB.s <- dfB[sample(1:nrow(dfB), 50),]

## Calculate gammas
g1 <- gammaCKpar(dfA.s$firstname, dfB.s$firstname)
g2 <- gammaCKpar(dfA.s$middlename, dfB.s$middlename)
g3 <- gammaCKpar(dfA.s$lastname, dfB.s$lastname)
g4 <- gammaKpar(dfA.s$birthyear, dfB.s$birthyear)

## Run tableCounts
tc <- tableCounts(list(g1, g2, g3, g4), nobs.a = nrow(dfA.s), nobs.b = nrow(dfB.s))
## Run EM
em <- emlinkMAR(tc, nobs.a = nrow(dfA.s), nobs.b = nrow(dfB.s))
## ------------------
## Apply to full data
## ------------------
## Calculate gammas
g1 <- gammaCKpar(dfA$firstname, dfB$firstname)
g2 <- gammaCKpar(dfA$middlename, dfB$middlename)
g3 <- gammaCKpar(dfA$lastname, dfB$lastname)
g4 <- gammaKpar(dfA$birthyear, dfB$birthyear)
## Run tableCounts
tc <- tableCounts(list(g1, g2, g3, g4), nobs.a = nrow(dfA), nobs.b = nrow(dfB))
em.full <- emlinkRS(tc, em, nrow(dfA), nrow(dfB)
                    ## End(Not run)

