library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(venn)
library(igraph)
library(stringr)

path_to_src  = file.path(here::here(), 'Data')
#path_to_link = file.path(here::here(), 'Data')
path = file.path(here::here(), 'R', 'Merging')
# source(file.path(path_to_src, 'Linker.R'))

load(file=file.path(path_to_src,
                    "FinalClassification",
                    "full_classification.RData"))

load(file=file.path(path_to_src,
                    "FinalClassification",
                    "full_combined_harmonized.RData"))

load(file= file.path(path_to_src,
                     "HarmonizedFiles",
                     "HarmonizedDataSets.RData"))

link_idx = classification$prediction == 'L'
links = classification$pairs[link_idx, c('id1', 'id2')]
link_graph = graph_from_edgelist(as.matrix(links), directed=F)
linked_ids = groups(components(link_graph, mode="weak"))
linked_ids = linked_ids[unlist(lapply(
    linked_ids, function(x) length(x) > 1))]

linked_sources = lapply(linked_ids,
                        function(x)  combined_harmonized[x, 'source'])

dup_link_idx = unlist(lapply(linked_sources, function(x) any(duplicated(x))))

dup_links      =linked_ids[ dup_link_idx]
unique_links   = linked_ids[!dup_link_idx]
unique_sources = linked_sources[!dup_link_idx]

sources = unique(combined_harmonized[, 'source'])

for (source in sources) {
    col_name = paste0("in_", source)
    combined_harmonized[col_name] = ifelse(combined_harmonized[, 'source'] == source, TRUE, NA)
}

collaps_vals = function(x) {
    return(x[!is.na(x)][1])
}


# Pad end components
max_comp = length(components(link_graph, mode="weak")$membership)
max_id   = nrow(combined_harmonized['uid'])
end_cap = 1:(max_id - max_comp) + max_comp
combined_harmonized['person'] = c(components(link_graph, mode="weak")$membership,
                                  end_cap)

# function to standardize the outputs
to_camel = function(x) {
    x =gsub("_", " ", x, perl=T)
    x = str_to_title(x)
    x = gsub("\\s", "\\U\\1", x , perl = T)
    return(x)
}


final_merged = combined_harmonized %>%
                group_by(person) %>%
                summarise_all(collaps_vals) %>%
                mutate(in_mpv  = ifelse(is.na(in_mpv),  0, 1)) %>%
                mutate(in_fe   = ifelse(is.na(in_fe),   0, 1)) %>%
                mutate(in_kbp  = ifelse(is.na(in_kbp),  0, 1)) %>%
                mutate(in_wapo = ifelse(is.na(in_wapo), 0, 1)) %>%
                select(-City, -County, -Source) # getting rid of duplicate



# # status after linkage
# combined  = combined_harmonized %>%
#     group_by(person) %>%
#     select( person, race, in_fe, in_kbp, in_wapo, in_mpv)
# 
# final = final_merged %>%
#     select( person, race, in_fe, in_kbp, in_wapo, in_mpv) %>%
#     mutate(status = ifelse(is.na(race), "missing", "not missing"))
# 
# 
# join = inner_join(combined, final, by = "person") %>%
#     select(person, race.x, race.y, status) %>%
#     rename(beforeLinkage = race.x, afterLinkage = race.y)

final_merged = final_merged %>%  rename_all(to_camel)
save_dir = file.path(path_to_src, 'Merged')

if(!dir.exists(save_dir)) {
    dir.create(save_dir, recursive=T)
}

save(final_merged,
     file = file.path(save_dir, "final_merged.RData"))


# breakdown of final_merged
fe <- sort(colnames(fe_harmonized))
kbp <- sort(colnames(kbp_harmonized))
wapo <- sort(colnames(wapo_harmonized))
mpv <- sort(colnames(mpv_harmonized))
#common
common <- Reduce(intersect, list(fe, kbp, wapo, mpv))

#unique
nfe <-  fe[-pmatch(common, fe)]
nkbp<-  kbp[-pmatch(common, kbp)]
nwapo <-  wapo[-pmatch(common, wapo)]
nmpv <-  mpv[-pmatch(common, mpv)]

#shared
int_fe_wapo <- intersect(nfe, nwapo)
int_fe_mpv <- intersect(nfe,nmpv )









