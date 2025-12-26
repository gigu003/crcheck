#' Check consistency between grade and morphology
#'
#' @template morp
#' @template beha
#' @template grad
#' @template std
#' @template quiet
#' 
#' @template return
#' @export
#'
#' @examples
#' grad <- c(3, 5)
#' morp <- c(9705, 9714)
#' beha <- c(3, 3)
#' res <- check_morp_grad(morp, beha, grad)
check_morp_grad <- function(morp,
                            beha,
                            grad,
                            std = "iacr",
                            quiet = FALSE) {
  morp <- gsub("[^0-9]", "", morp)
  morp <- as.integer(morp)
  grad <- as.integer(grad)
  if (std == "iacr") {
    beha_grad <- beha < 3 & grad < 9
    result <- lapply(1:11, function(x) {
      grad %in% IARC_GradMorpRule[[1]][[x]] &
        morp %in% IARC_GradMorpRule[[2]][[x]]
    })
    res_matrix <- do.call(rbind, append(list(beha_grad), result))
    groups <- apply(res_matrix, 2, function(x) {
      loc <- which(x == TRUE, arr.ind = TRUE)[1]
      paste(loc +210, collapse = ",")
    })
    groups <- ifelse(is.na(groups), 0, groups)
    check <- !Reduce(`|`, append(list(beha_grad), result))
    error_code <- ifelse(beha_grad, "W-BEGR",
                         ifelse(!check, "W-MOGR", "C"))
    error_desc <- groups
  } else if (std == "encr") {
    # Rule check for Heamato
    rule_heamato <- ENCR_GradMorpRule[[1]]
    heamato <- lapply(1:70, function(x) {
      morp %in% rule_heamato[[1]][[x]] & grad %nin% rule_heamato[[2]][[x]]
    })
    t61_logi <- grad %in% (5:8) & morp %nin% gen_morp(9590:9993)
    t62_logi <- grad %in% c(1:4) & morp %in% gen_morp(c(9590:9800,
                                                        9802:9993))
    heamato <- Reduce('|', c(heamato, t61_logi, t62_logi))
    
    # Rule check for CNS
    rule_cns <- ENCR_GradMorpRule[[2]]
    cns <- lapply(1:65, function(x) {
      morp %in% rule_cns[[1]][[x]] & beha %in% rule_cns[[2]][[x]] &
        grad %nin% rule_cns[[3]][[x]]
    })
    cns <- Reduce('|', cns)
    
    # Rule check for Solid tumor
    rule_solid <- ENCR_GradMorpRule[[3]]
    solid <- lapply(1:19, function(x) {
      morp %in% rule_solid[[1]][[x]] & beha %in% rule_solid[[2]][[x]] &
        grad %nin% rule_solid[[3]][[x]]
    })
    solid <- Reduce('|', solid)
    
    # Rule check for general rule
    begr <- beha < 3 & grad %in% c(1:8)
    
    # Combine the Rules check result
    result <- list(begr, heamato, cns, solid)
    check <- !Reduce('|', result)
    error_code <- ifelse(heamato | cns | solid, "W-MOGR",
                         ifelse(begr, "W-BEGR", "C"))
    error_desc <- ifelse(heamato, 211,
                         ifelse(cns, 212,
                                ifelse(solid, 213,
                                       ifelse(begr, 214, 0))))
    }
  
  #generate result data
  data <- list(morp = morp, beha = beha, grad = grad)
  type <- ifelse(check, 1, 2)
  #output the result
  res <- list()
  class(res) <- c("check", "morp/beha/grad")
  res$check_item <- "Morp/Beha/Grad"
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
