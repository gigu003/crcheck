#' Check consistency between sex and site code
#'
#' @template sex
#' @template topo
#' @template morp
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' sex <- c(1, 1, 1, 2, 2, 2)
#' topo <- c("C51.0", "C53.0", "C57.4", "C60.0", "C60.9", "C63.9")
#' morp <- c(8950, 9061, 8600, 9300, 8500, 8600)
#' res <- check_sex_topo_morp(sex, topo, morp)
#' print(res)
check_sex_topo_morp <- function(sex, topo, morp, quiet = FALSE) {
  check_equal_length(sex, topo, morp, arg_names = c("sex", "topo", "morp"))
  check_flag(quiet, "quiet")

  sex <- as.integer(sex)
  topo <- add_decimal(topo)
  morp <- as.integer(gsub("[^0-9]", "", morp))

  ## Rule 1 check
  topo_male <- which(sex == 1 & topo %in% gen_topo(510:589))
  topo_female <- which(sex == 2 & topo %in% gen_topo(600:639))
  loc_topo <- unique(c(topo_male, topo_female))
  values1 <- paste0(sex[loc_topo], "/", topo[loc_topo])
  if (length(loc_topo) == 0) {
    values1 <- character(0)
  }
  check1 <- loc_topo
  types1 <- rep("E-SETO", length(check1))
  groups1 <- character(length(check1))
  groups1[match(topo_male, check1)] <- "G-01"
  groups1[match(topo_female, check1)] <- "G-02"

  ## Rule 2 check
  morp_male <- which(sex == 1 & morp %in% unlist(IARC_TopoMorpRule[[1]][23:27]))
  morp_female <- which(
    sex == 2 & morp %in% unlist(IARC_TopoMorpRule[[1]][28:29])
  )
  loc_morp <- unique(c(morp_male, morp_female))
  values2 <- paste0(sex[loc_morp], "/", morp[loc_morp])
  if (length(loc_morp) == 0) {
    values2 <- character(0)
  }
  check2 <- loc_morp
  types2 <- rep("E-SEMO", length(check2))
  groups2 <- character(length(check2))
  groups2[match(morp_male, check2)] <- "G-01"
  groups2[match(morp_female, check2)] <- "G-02"

  # Construct result
  res <- structure(
    list(
      check_item = c(
        rep("Sex/Topo", length(loc_topo)),
        rep("Sex/Morp", length(loc_morp))
      ),
      check = c(check1, check2),
      data = c(values1, values2),
      type = c(types1, types2),
      group = c(groups1, groups2)
    ),
    class = c("check", "sex/topo/morp")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
