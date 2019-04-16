#' Vaughn Johnson
#' 2019-02-19
#' Record linker
#'
#' This package always assumes you're in
#' the top level directory for the package
#' ie, where ffsg.Rproj is kept

library(dplyr)
library(here)
library(RecordLinkage)

path_to_src = here::here(file.path('R'))
load(file.path(path_to_src,
               "HarmonizedFiles",
               "HarmonizedDataSets.RData"))

compare.dedup(kbp_harmonized, phonetic='name')
kbp_harmonized
