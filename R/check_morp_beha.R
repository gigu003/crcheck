#' Check the morphology and behavior code in ICDO3
#'
#'
#' @template morp
#' @template beha
#' @template std
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' morp <- c(8000, 8010, 8140, 8533, 9000)
#' beha <- c(3, 3, 3, 3, 1)
#' res <- check_morp_beha(morp, beha)
#' print(res)
check_morp_beha <- function(morp,
                            beha,
                            std = "iacr",
                            quiet = FALSE) {
  morp <- gsub("[^0-9]", "", morp)
  morp_logi <- morp %nin% ValueRange$iarc$morp
  beha_logi <- beha %nin% ValueRange$iarc$beha
  if (std == "encr") {
    morp_beha_logi <- paste0(morp, "/", beha) %nin% ENCR_BehaMorpRule[[1]]
  } else if (std == "iacr") {
    morp_beha_logi <- paste0(morp, "/", beha) %nin% IARC_BehaMorpRule[[1]]
  }
  
  # generate output data.
  data <- list(morp = morp, beha = beha)
  check <- !(morp_logi | beha_logi | morp_beha_logi)
  type <- ifelse(morp_logi | beha_logi, 3,
                 ifelse(morp_beha_logi, 2, 1))
  error_code <- ifelse(is.na(beha) | is.na(morp), "E-MISS",
                       ifelse(beha_logi | morp_logi, "E-OUTR",
                              ifelse(morp_beha_logi, "W-MOBE", "C"))
                       )
  error_desc <- ifelse(is.na(beha) | is.na(morp), 11,
                       ifelse(beha_logi, 12,
                              ifelse(morp_logi, 13,
                                     ifelse(morp_beha_logi, 14, 0)))
                       )
  # output result
  res <- list()
  class(res) <- c("check", "morp/beha")
  res$check_item <- "Morphology/Behaviour"
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

