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

test_idx = sample(1:(nrow(kbp_harmonized)/3), 250, replace=T)
table(test_idx)
matches = compare.dedup(kbp_harmonized[test_idx, ], identity=test_idx, strcmp = T)
results = classifySupv(trainSupv(matches, method='svm'), matches)
truth = results$pairs$is_match == 1
pred  = results$prediction == 'L'
sum(xor(truth, pred))
