#' Vaughn Johnson
#' 2019-02-19
#' Clerical Review System
#'
#' This package always assumes you're in
#' the top level directory for the package
#' ie, where ffsg.Rproj is kept

library(dplyr)
library(plyr)
library(here)
library(RecordLinkage)

path_to_src = here::here(file.path('R'))
# source(file.path(path_to_src, "Harmonizer.R"))

load(file.path(path_to_src,
               "HarmonizedFiles",
               "HarmonizedDataSets.RData"))



common_cols = Reduce(intersect, list(colnames(kbp_harmonized),
                                     colnames(fe_harmonized),
                                     colnames(mpv_harmonized),
                                     colnames(wapo_harmonized)))


combined_harmonized = plyr::rbind.fill(kbp_harmonized,
                                       fe_harmonized,
                                       mpv_harmonized,
                                       wapo_harmonized) %>%
                                       mutate("uid" = 1:nrow(.)) %>% sample_n(1200)

combined_link = combined_harmonized %>%
                select(c(common_cols, 'uid'))

colinear = c("aka", 'name', 'date', 'str_age', 'source', 'uid')

blank_dedup_object = compare.dedup(combined_link,
                                   exclude = colinear,
                                   blockfld = c('state', 'year'),
                                   phonetic = c("firstname", "lastname", "middlename"),
                                   strcmp = c("firstname", "lastname", "middlename"))

EM_dedup_object = emWeights(blank_dedup_object)

# Threshold determined from clerical review
threshold = 6

classification = emClassify(EM_dedup_object, threshold.upper = 6)

save(classification, file=file.path(path_to_src,
                                    "FinalClassification",
                                    "sample_classification.RData"))
