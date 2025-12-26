#' Check the combination of topography and morphology
#'
#' Validates combinations of topography (ICD-O-3 site) and morphology (ICD-O-3
#' histology) codes using standard tumor coding rules. Supports IARC (2005)
#' standards for assessing whether a given topography/morphology
#' pairing is considered valid, invalid, or discouraged.
#'
#' @template topo
#' @template morp
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' topo <- c("C15.1", "C06.9", "C16.2", "C34.9")
#' morp <- c(8140, 8500, 8500, 8600)
#' res <- check_topo_morp(topo, morp)
#' print(res)
check_topo_morp <- function(topo,
                            morp,
                            std = "iacr",
                            quiet = FALSE
                            ) {
  topo <- add_decimal(topo)
  iacr_tm <- function(x) {
    if (x <= 64) {
      morp %in% IARC_TopoMorpRule[[1]][[x]] &
        topo %nin% IARC_TopoMorpRule[[2]][[x]]
    } else {
      morp %in% IARC_TopoMorpRule[[1]][[x]] &
        topo %in% IARC_TopoMorpRule[[2]][[x]]
    }
  }
  
  encr_tm <- function(x, type = "allowed") {
    if (type == "allowed") {
      morp %in% ENCR_TopoMorpRule$allowed[[1]][[x]] &
        topo %nin% ENCR_TopoMorpRule$allowed[[2]][[x]]
    } else if (type == "nallowed") {
      morp %in% ENCR_TopoMorpRule$nallowed[[1]][[x]] &
        topo %in% ENCR_TopoMorpRule$nallowed[[2]][[x]]
    }
  }
  
  if (std == "iacr") {
    result <- lapply(1:71, iacr_tm)
    res_matrix <- do.call(rbind, result)
    groups <- apply(res_matrix, 2, function(x) {
      loc <- which(x == TRUE, arr.ind = TRUE)
      paste(loc + 300, collapse = ",")
    })
    groups <- ifelse(is.na(groups), 0, groups) 
    check <- !Reduce(`|`, result)
  } else if (std == "encr") {
    result1 <- lapply(1:105, encr_tm, type = "allowed")
    result2 <- lapply(1:10, encr_tm, type = "nallowed")
    res_matrix <- do.call(rbind, c(result1, result2))
    groups <- apply(res_matrix, 2, function(x) {
      loc <- which(x == TRUE, arr.ind = TRUE)[1]
      paste(loc + 300, collapse = ",")
    })
    groups <- ifelse(is.na(groups), 0, groups)
    check <- !Reduce(`|`, c(result1, result2))
  }
  
  
  # generate result data
  data <- list(topo = topo, morp = morp)
  type <- ifelse(check, 1, 2)
  error_code <- ifelse(!check, "W-MOTO", "C")
  error_desc <- groups
  # output result
  res <- list()
  class(res) <- c("check", "topo/morp")
  res$check_item <- "Topography/Morphology"
  res$check_standard <- std
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- type
  res$error_code <- error_code
  res$error_desc <- error_desc
  if (!quiet) {
    summary(res)  
  }
  invisible(res)
}
