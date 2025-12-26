#' Check consistency between basis of diagnosis and morphology code
#'
#' @template basi
#' @template morp
#' @template topo
#' @template beha
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#'  basi <- c(1, 1, 1, 1, 5, 7, 2)
#'  beha <- c(3, 3, 3, 3, 3, 3, 3)
#'  morp <- c(8000, 8720, 9120, 9510, 8023, 8001, 9180)
#'  topo <- c("C15.2","C15.2", "C44.0", "C69.1", "C42.1", "C15.2", "C15.2")
#'  res <- check_basi_morp(basi, morp, topo, beha)
#'  print(res)
check_basi_morp <- function(basi,
                            morp,
                            topo = NULL,
                            beha = NULL,
                            std = "iacr",
                            quiet = FALSE) {
  topo <- add_decimal(topo)
  if (std == "iacr") {
    morp_allowed <- c(8000, gen_morp(8150:8154), 8170, gen_morp(8270:8281),
                      8800, 8960, 9100, 9140, 9380, 9384, 9500, 9510,
                      gen_morp(9530:9539), 9590, 9732, 9761, 9800)
    basi_logi <- basi %in% c(0, 1:4, 8, 9) & morp %nin% morp_allowed
    #result <- lapply(1:2, function(x) {
    #  basi %in% IARC_BasiMorpRule[[1]][[x]] &
    #    !(morp %in% IARC_BasiMorpRule[[2]][[x]] &
    #       topo %in% IARC_BasiMorpRule[[2]][[x]])
    #})
    #rr <- basi %in% c(5:7) & morp %in% (8000)
    #res_matrix <- do.call(rbind, append(result, list(rr)))
    #groups <- apply(res_matrix, 2, function(x) {
    #  loc <- which(x == TRUE, arr.ind = TRUE)
    #  paste(loc + 210, collapse = ",")
    #})
    #groups <- ifelse(is.na(groups)|groups=="", 0, groups)
    #check <- !Reduce(`|`, append(list(rr), result))
    check <- !basi_logi
    groups <- ifelse(!check, 211, 0)
    error_code <- ifelse(!check, "W-BDMO", "C")
    error_desc <- groups
  } else if (std == "encr") {
    # define a function 
    check_basi <- function(x, bs, rule = rule1) {
      morp_list <- rule[[1]][[x]]
      beha_list <- rule[[2]][[x]]
      topo_list <- rule[[3]][[x]]
      if (length(topo_list)==0) {topo_list <- gen_topo(0:809)}
      morp_logi <- morp %in% morp_list
      beha_logi <- beha %in% beha_list
      topo_logi <- topo %nin% topo_list
      basi == bs & morp_logi & beha_logi & topo_logi
    }
    # check allowed morphology code when basi code equals 0
    basi0 <- basi == 0 & beha == 3 & morp %nin% unlist(ENCR_BasiMorpRule$basi0[[1]])
    # check allowed morphology and behaviour code when basi equals 1
    rule1 <- ENCR_BasiMorpRule[[2]]
    rule1[[3]][[1]] <- gen_topo(0:809)
    result <- lapply(1:5, check_basi, bs =1)
    basi1 <- Reduce(`|`, result)
    # check allowed morphology and behaviour code when basi equals 2
    rule2 <- ENCR_BasiMorpRule[[3]]
    rule2[[3]][[1]] <- gen_topo(0:809)
    result <- lapply(1:65, check_basi, bs = 2, rule = rule2)
    basi2 <- Reduce(`|`, result)
    # check allowed morphology and behaviour code when basi equals 4
    rule4 <- ENCR_BasiMorpRule[[4]]
    result <- lapply(1:20, check_basi, bs = 4, rule = rule4)
    basi4 <- Reduce(`|`, result)
    # check allowed morphology and behaviour code when basi equals 5
    morp_list <- ENCR_BasiMorpRule[[5]][[1]][[2]]
    basi5 <- basi == 5 & beha == 3 & morp %in% morp_list
    # check allowed morphology and behaviour code when basi equals 6
    basi6 <- basi == 6 & beha %in% c(0, 1, 2)
    # check allowed morphology and behaviour cocde when basi equals 7
    morp_list <- ENCR_BasiMorpRule[[7]][[1]][[1]]
    basi7 <- basi == 7 & beha == 3 & morp %in% morp_list
    # check allowed morphology and behaviour cocde when basi equals 8
    morp_list <- ENCR_BasiMorpRule[[8]]
    basi8 <- basi == 8 & morp %nin% morp_list
    
    # generate output data
    result <- list(basi0, basi1, basi2, basi4,
                   basi5, basi6, basi7, basi8)
    check <- !Reduce('|', result)

    res_matrix <- do.call(rbind, result)
    groups <- apply(res_matrix, 2, function(x) {
      which(x == TRUE, arr.ind = TRUE)[1]
    })
    groups <- ifelse(is.na(groups), 0, groups + 240)
    error_code <- ifelse(basi5|basi7|basi8, "W-BDMS",
                         ifelse(basi0|basi1|basi2|basi4, "W-BDMO", "C"))
    error_desc <- groups
  }
  data <- list(basi = basi, morp = morp, beha = beha, topo = topo)
  type <- ifelse(check, 1, 2)
  
  # output result
  res <- list()
  class(res) <- c("check", "basi/morp")
  res$check_item <- "Basi/Morphology"
  res$check_standard <- std
  res$check_version <- "ICDO3.2/ENCR"
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
