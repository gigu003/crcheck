#' Check the morphology and behavior code in ICDO3
#'
#' @param morp Morphology codes. 
#' @param beha Behavior codes.
#'
#' @return check_morp returns an object of class of 'check'.
#' @export
#'
#' @examples
#' morp <- c(8000, 8010, 8140, 8533, 9000)
#' beha <- c(3, 3, 3, 3, 1)
check_morp_beha <- function(morp, beha){
  data <- list(morp = morp, beha = beha)
  morp <- gsub("[^0-9]", "", morp)
  morp_logi <- morp %in% morp_dict
  beha_logi <- beha %in% c("0","1","2","3","6","9")
  morp_beha_logi <- paste0("M", morp, "/", beha) %in% morp_beha_dict
  check <- morp_logi & morp_beha_logi & beha_logi
  type <- ifelse(!morp_logi | !beha_logi,
                 3,
                 ifelse(!morp_beha_logi, 2, 1))
  message <- ifelse(!morp_logi, 6,
                ifelse(!beha_logi, 7,
                       ifelse(!morp_beha_logi, 8, 0)))
  ress <- ifelse(!morp_logi, NA, morp)
  res <- list()
  class(res) <- c("check","morp/beha")
  res$check_item <- "Morphology/Behaviour"
  res$check_version <- "ICDO3.2"
  res$data <- data
  res$check <- check
  res$type <- factor(type,
                     levels = c(1:3),
                     labels = c("correct", "warning", "error"))
  res$message <- message
  res$correct <- ress
  print(res)
  invisible(res)
}

gen_morp <- function(x, na.drop = TRUE) {
  res <- ifelse(x %nin% morp_dict | x < 8000 | x > 9993, NA, x)
  if(na.drop){res <- res[!is.na(res)]}
  return(res)
}