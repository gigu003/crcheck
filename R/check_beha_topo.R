#' Check consistency between behavior and site code
#'
#' @template beha
#' @template topo
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#' beha <- c(2, 2, 2, 2)
#' site <- c("C40.9", "C49.1", "C70.0", "C72.9")
#' res <- check_beha_topo(beha, site)
#' print(res)
check_beha_topo <- function(beha, topo, quiet = FALSE) {
  check_equal_length(beha, topo, arg_names = c("beha", "topo"))
  check_flag(quiet, "quiet")

  beha <- as.integer(beha)
  topo <- add_decimal(topo)
  beha_site <- beha == 2 &
    topo %in%
      gen_topo(c(
        400:429,
        470:479,
        490:499,
        700:729
      ))
  # prepare result data
  loc_beha_site <- which(beha_site)
  values_beha_site <- paste0(beha[loc_beha_site], "/", topo[loc_beha_site])
  groups <- rep("G-01", length(loc_beha_site))

  # Construct result data
  res <- structure(
    list(
      check_item = "Beha/Topo",
      check = loc_beha_site,
      data = values_beha_site,
      type = "W-BETO",
      group = groups
    ),
    class = c("check", "beha/topo")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
