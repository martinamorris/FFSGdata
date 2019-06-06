library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(venn)
library(igraph)

path_to_src = file.path(here::here(), 'R', 'Linking')
# source(file.path(path_to_src, 'Linker.R'))


load(file=file.path(path_to_src,
                    "FinalClassification",
                    "full_classification.RData"))

load(file=file.path(path_to_src,
                    "FinalClassification",
                    "full_combined_harmonized.RData"))

link_idx = classification$prediction == 'L'
links = classification$pairs[link_idx, c('id1', 'id2')]
link_graph = graph_from_edgelist(as.matrix(links), directed=F)
linked_ids = groups(components(link_graph, mode="weak"))
linked_ids = linked_ids[unlist(lapply(
    linked_ids, function(x) length(x) > 1))]

linked_sources = lapply(linked_ids,
                        function(x)  combined_harmonized[x, 'source'])

dup_link_idx = unlist(lapply(linked_sources, function(x) any(duplicated(x))))

dup_links      =    linked_ids[ dup_link_idx]
unique_links   = linked_ids[!dup_link_idx]
unique_sources = linked_sources[!dup_link_idx]

sources = unique(combined_harmonized[, 'source'])

for (source in sources) {
    col_name = paste0("in_", source)
    combined_harmonized[col_name] = combined_harmonized[, 'source'] == source
}

collaps_vals = function(x) {
    return(x[!is.na(x)][1])
}

# This would break if the max row wasn't linked to anything
combined_harmonized['person'] = components(link_graph, mode="weak")$membership

final_merged = combined_harmonized %>%
                group_by(person) %>%
                spread(source, source) %>%
                summarise_all(collaps_vals)
