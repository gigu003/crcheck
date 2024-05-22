classify_childhood <- function(topo, morp, type = "sub") {
  iccc3_morp <- iccc3_2005[[1]]
  iccc3_topo <- iccc3_2005[[2]]
  topo_default <- function(x) {
    if (is.null(x)) gen_topo(0:809)
    else x
  }
  iccc3_topo <- lapply(iccc3_topo, topo_default)
  id_groups <- function(x) {
    morp %in% iccc3_morp[[x]] & topo %in% iccc3_topo[[x]]
  }
  find_pos <- function(x) {
    which(x == TRUE, arr.ind = TRUE)[1]
  }
  result <- lapply(1:69, id_groups)
  res_matrix <- do.call(rbind, result)
  groups <- apply(res_matrix, 2, find_pos)
  groups <- ifelse(is.na(groups), 0, groups)
  recodes <- names(iccc3_morp)[groups]
  sub <- as.numeric(recodes)
  main <- floor(sub / 10)
  if (type == "sub") {
    return(sub)
  } else if (type == "main") {
    return(main)
  }
}
