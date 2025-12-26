#' Convert ICDO3 code to ICD10 code.
#'
#' @param topo Topography code in ICDO3.
#' @param morp Morphology code in ICDO3.
#' @param beha Behavior code in ICDO3.
#'
#' @return Vector of ICD10 codes.
#' @export
#'
ICDO3toICD10 <- function(topo, morp, beha) {
  com <- lapply(1:length(topo), function(x){
    c(paste(morp[[x]], beha[[x]], sep="/"), topo[[x]])
  })
  icd10 <- unlist(lapply(com, function(x){
    o3to10(x[1], x[2])
  }))
  return(icd10)
}

o3to10 <- function(morp_beha, topo){
  rule1 <- SEER_ICDO3toICD10Rule$morp_map1
  rule2 <- SEER_ICDO3toICD10Rule$morp_map2
  rule3 <- SEER_ICDO3toICD10Rule$morp_map3
  toporule <- SEER_ICDO3toICD10Rule$topo_map
  if (morp_beha %in% unlist(rule1$morp)) {
    icd10 <-   unlist(lapply(1:80, function(i) {
      if (morp_beha %in% rule1$morp[[i]]) {
        return(rule1$icd10[[i]]) }
      }))
  } else if (morp_beha %in% unlist(rule2$morp)) {
    top_loc <- unlist(lapply(1:9, function(i) {
      if (morp_beha %in% rule2$morp[[i]]) {
        return(rule2$loc[[i]])
      }
    }))
    icd10 <- NA
    if (topo %in% rownames(toporule)) {
      icd10 <- toporule[topo, top_loc]
      }
    
    check <- check_topo(topo, quiet = TRUE)$check
    
  } else {
    check <- check_morp_beha(substr(morp_beha, 1, 4),
                               substr(morp_beha, 6, 6),
                               quiet = TRUE)$check
    if (!check){
      icd10 <- NA
    } else {
      icd10 <- "rules not found"
    }
    
  }
  return(icd10)
}
