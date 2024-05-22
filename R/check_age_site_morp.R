#' Check consistency between data items of age, site, and morp
#'
#' @param age Age
#' @param topo Cancer site
#' @param morp Morphology
#'
#' @return Class of 'check'
#' @export
#'
#' @examples
#' age <- c(30)
#' topo <- c("C61.9")
#' morp <- c(8140)
check_age_site_morp <- function(age, topo, morp) {
  data <- list(age = age, topo = topo, morp = morp)
  sub <- classify_childhood(topo, morp)
  un_age <- list(
    0:2,
    10:14,
    6:14,
    9:14,
    0:8,
    6:14,
    0:8,
    0:5,
    0:5,
    0:3,
    8:14,
    0:14,
    0:5,
    0:5,
    0:4,
    0:4
  )
  un_sub <- list(
    21, 41, 50, 61, 62, 71, 72, 81, 82, 83,
    102, 104, 112, 113, 115, 116
  )
  age_sub <- function(x) {
    sub %in% un_sub[[x]] & age %in% un_age[[x]]
  }
  result <- lapply(1:14, age_sub)
  result[[17]] <- age >= 0 & age <= 14 & substr(morp, 1, 3) == "905"

  # check conditions age≥15
  result[[18]] <- age >= 15 & age < 40 &
    topo %in% gen_topo(610:619) & substr(morp, 1, 3) == "814"
  result[[19]] <- age >= 15 & age < 20 &
    topo %in% gen_topo(c(150:159, 190:249, 384, 500:509, 530:559))
  result[[20]] <- age >= 15 &
    age < 20 & topo %in% gen_topo(170:179) & morp < 9590
  result[[21]] <- age >= 15 &
    age < 20 & topo %in% gen_topo(c(180:189, 330:349)) &
    (!substr(morp, 1, 3) == "824")
  result[[22]] <- age > 45 &
    topo %in% gen_topo(580:589) & morp == 9100
  result[[23]] <- age >= 15 & age <= 25 & morp %in% c(9732, 9823)
  result[[24]] <- age >= 15 &
    (morp %in% c(8910, 8960, 8970, 8981, 8991, 9072, 9470, 9687) |
       substr(morp, 1, 3) == "951")

  res_matrix <- do.call(rbind, result)
  groups <- apply(res_matrix, 2, function(x) {
    which(x == TRUE, arr.ind = TRUE)[1]
  })
  groups <- groups + 20
  check <- !Reduce(`|`, result)
  type <- ifelse(check, 1, 2)
  res <- list()
  class(res) <- c("check", "age/site/morp")
  res$check_item <- "Age/Site/Morp"
  res$check_version <- "IARC Check Rules 2005"
  res$data <- data
  res$check <- check
  res$type <- factor(type,
    levels = c(1:3),
    labels = c("correct", "warning", "error")
  )
  res$message <- groups
  print(res)
  invisible(res)
}
