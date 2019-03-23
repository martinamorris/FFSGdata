
create.trans <- function(name, ops = c("A", "O"), currOps = c(), tempOps = c(), indexApplied = c(), graph = list()) {
  if (length(name) > 0) {
    for (i in 1:length(ops)) {
      for (j in  1:length(name)) {
        mod.name <- name
        if (!(j %in% indexApplied)) {
          if (ops[i] == "S") {
            #print(mod.name)
            prm <- gtools::permutations(n=length(unique(mod.name)), r=length(unique(mod.name)), v=mod.name)
            #print(prm)
            if (rlang::is_empty(currOps)) {
              currOps <- c(paste(name, collapse = " "))
            }
            p <- c(currOps, paste(ops[i], get.count(tempOps, ops[i]) + 1, sep = ""))
            for (k in 1:nrow(prm)) {
              print(prm[k, ])
              graph <- add.to.graph(graph, p, list("name" = paste(prm[k, ], collapse = " "), "match_format" = get.match(prm[k, ])))
            }
          } else {
            if (ops[i] == "A") {
              mod.name[j] <- substr(mod.name[j], 1, 1)
            } else if (ops[i] == "O") {
              mod.name <- mod.name[-j]
            }
            #print(mod.name)
            if (!rlang::is_empty(mod.name)) {
              if (rlang::is_empty(currOps)) {
                currOps <- c(paste(name, collapse = " "))
              }
              p <- c(currOps, paste(ops[i], get.count(tempOps, ops[i]) + 1, sep = ""))
              graph <- add.to.graph(graph, p, list("name" = paste(mod.name, collapse = " "), "match_format" = get.match(mod.name)))
              graph <- create.trans(mod.name, ops, p, c(tempOps, ops[i]), c(indexApplied, j), graph)
            }
          }
        }
      }
    }
  }
  return(graph)
}

get.match <- function(arr) {
  res <-c()
  for (i in 1:length(arr)) {
    if (nchar(arr[i]) > 1) {
      res <- c(res, "N")
    } else {
      res <- c(res, "I")
    }
  }
  return(res)
}

add.to.graph <- function(graph, path, dest) {
  if (length(path) > 1) {
    if (is.null(graph[[path[1]]])) {
      graph[[path[1]]] <- list()
    }
    graph[[path[1]]] <- unique(c(graph[[path[1]]], list(path[2])))
    graph <- add.to.graph(graph, tail(path,-1), dest)
  } else if (length(path) == 1) {
    if (is.null(graph[[path[1]]])) {
      graph[[path[1]]] <- list()
    }
    graph[[path[1]]] <- unique(c(graph[[path[1]]], list(dest)))
  }
  return(graph)
}

get.count <- function(arr, ops) {
  a <- table(arr)
  return(a[ops == names(a)])
}

g1 <- create.trans(c("Jainul", "N", "Vaghasia", "K", "L", "M"))
g2 <- create.trans(c("J", "Naresh", "Vaghasia"))

merge.graphs <- function(graph1, graph2) {
}

