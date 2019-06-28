#' Vaughn Johnson
#' 2019-02-19
#' Clerical Review System
#'
#' This package always assumes you're in
#' the top level directory for the package
#' ie, where ffsg.Rproj is kept

library(plyr)
library(dplyr)
library(here)
library(RecordLinkage)

path_to_src = here::here(file.path('R', 'Harmonizing'))
source(file.path(path_to_src, "Harmonizer.R"))

load(file.path(path_to_src,
               "HarmonizedFiles",
               "HarmonizedDataSets.RData"))

common_cols = Reduce(intersect, list(colnames(kbp_harmonized),
                                     colnames(fe_harmonized),
                                     colnames(mpv_harmonized),
                                     colnames(wapo_harmonized)))

colinear = c("aka", 'name', 'date', 'str_age')

combined_harmonized = plyr::rbind.fill(kbp_harmonized,
                                       fe_harmonized,
                                       mpv_harmonized,
                                       wapo_harmonized) %>%
                                       select(common_cols) %>%
                                       select(-colinear)

display_matches = function(classifyObject) {
    links = classifyObject$prediction == 'L'
    num_matches = sum(links)
    ncols = ncol(classifyObject$data)
    df = data.frame(matrix(nrow = 3 * num_matches, ncol= ncols))
    colnames(df) = colnames(classifyObject$data)
    left  = classifyObject$pairs[links, 'id1']
    right = classifyObject$pairs[links, 'id2']
    idx1 = 3 * (0:(num_matches -1))
    idx2  = idx1 + 1
    blank = idx1 + 2

    df[idx1  + 1, ] = classifyObject$data[left, ]
    df[idx2  + 1, ] = classifyObject$data[right, ]
    df[blank + 1, ] = c(rep("", ncols))
    return(df)
}

k = 10
search_space = 10
n_matches      = matrix(ncol=search_space, nrow=k)
n_true_matches = matrix(ncol=search_space, nrow=k)

for (iteration in 1:k) {
    comb_sample = combined_harmonized %>% sample_n(500)

    blank_dedup_object = compare.dedup(comb_sample,
                                       #blockfld = c('state'),
                                       phonetic = T,
                                       strcmp = T)

    EM_dedup_object = emWeights(blank_dedup_object)

    for (i in 1:search_space) {
        threshold = 3*i - 15
        classify  = emClassify(EM_dedup_object, threshold.upper=threshold)
        print.data.frame(display_matches(classify))
        n_matches[iteration, i] = sum(classify$prediction == 'L')

        if (n_matches[iteration, i] > 100) {
            n_true_matches[iteration, i] = NA
        } else {
            n_true_matches[iteration, i] = readline(prompt="How many true matches: ")
        }
    }
}

save.image("love")
