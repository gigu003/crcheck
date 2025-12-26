#' Check consistency between data items of age, site, and morp
#'
#' @template age
#' @template topo
#' @template morp
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' age <- c(30)
#' topo <- c("C61.9")
#' morp <- c(8140)
#' res <- check_age_topo_morp(age, topo, morp)
#' print(res)
check_age_topo_morp <- function(age,
                                topo,
                                morp,
                                std = "iacr",
                                quiet = FALSE) {
  topo <- add_decimal(topo)
  un_age_morp <- function(x, std = "encr") {
    if (std == "encr"){
      agerule <- ENCR_AgeTopoMorpRule
    } else if (std == "iacr") {
      agerule <- IARC_AgeTopoMorpRule
    }
    un_age <- agerule[[1]][[x]]
    morp_list <- agerule[[2]][[x]]
    topo_list <- agerule[[3]][[x]]
    if (length(topo_list) == 0) {
      topo_list <- gen_topo(0:809)
    }
    age %in% un_age & morp %in% morp_list & topo %in% topo_list
  }
  
  if (std == "encr") {
    result <- lapply(1:29, un_age_morp, std = "encr")
    res_matrix <- do.call(rbind, result)
    groups <- apply(res_matrix, 2, function(x) {
      loc <- which(x == TRUE, arr.ind = TRUE)
      paste(loc + 110, collapse = ",")
    })
    groups <- ifelse(is.na(groups), 0, groups)
    check <- !Reduce(`|`, result)
  } else if (std == "iacr") {
    result <- lapply(1:36, un_age_morp, std = "iacr")
    res_matrix <- do.call(rbind, result)
    groups <- apply(res_matrix, 2, function(x) {
      loc <- which(x == TRUE, arr.ind = TRUE)
      paste(loc + 140, collapse = ",")
    })
    groups <- ifelse(is.na(groups), 0, groups)
    check <- !Reduce(`|`, result)
  }

  #generate result data
  data <- list(age = age, topo = topo, morp = morp)
  type <- ifelse(check, 1, 2)
  error_code <- ifelse(!check, "W-AGMT", "C")
  error_desc <- groups
    
  res <- list()
  class(res) <- c("check", "age/site/morp")
  res$check_item <- "Age/Site/Morp"
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
