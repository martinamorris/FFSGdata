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

#
common_cols = intersect(colnames(kbp_harmonized),
                        colnames(wapo_harmonized))

kbp_link = kbp_harmonized %>% select(common_cols)
wapo_link = wapo_harmonized %>% select(common_cols)

pairs = compare.linkage(kbp_link, wapo_link, blockfld = c('state'), phonetic = F)
results = emWeights(pairs)$pairs # Non match weights are all NA
results[!is.na(results$is_match), ]


common_names = intersect( kbp_harmonized$name,
                        wapo_harmonized$name)

 kbp_harmonized %>% filter(name == "Terry Lee Cockrell")
wapo_harmonized %>% filter(name == "Terry Lee Cockrell")
