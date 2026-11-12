calc_registry_age <- function(start, end) {
  check_equal_length(start, end, arg_names = c("start", "end"))
  start <- parse_registry_date(start, arg = "start")
  end <- parse_registry_date(end, arg = "end")
  age <- rep(NA, length(start))
  v_dates <- !is.na(start) & !is.na(end)
  # Extract year, month, and day components for valid indices
  syear <- as.numeric(format(start[v_dates], "%Y"))
  eyear <- as.numeric(format(end[v_dates], "%Y"))
  smonth <- as.numeric(format(start[v_dates], "%m"))
  emonth <- as.numeric(format(end[v_dates], "%m"))
  sday <- as.numeric(format(start[v_dates], "%d"))
  eday <- as.numeric(format(end[v_dates], "%d"))
  # Calculate initial age for valid dates
  age[v_dates] <- eyear - syear
  adjustment <- (emonth < smonth) | (emonth == smonth & eday < sday)
  age[v_dates][adjustment] <- age[v_dates][adjustment] - 1
  return(age)
}


#' Check each Topo/Morp rule (internal)
#'
#' @noRd
#' @template topo
#' @template morp
#' @param x Rule number (index)
#' @param std Standard ("iacr" or "encr")
#' @param type For ENCR: "allowed" or "nallowed"
#'
#' @returns Logical vector of same length as topo/morp indicating violations
check_tm <- function(x, topo, morp) {
  if (x <= 64) {
    morp %in%
      IARC_TopoMorpRule[[1]][[x]] &
      topo %nin% IARC_TopoMorpRule[[2]][[x]]
  } else {
    morp %in%
      IARC_TopoMorpRule[[1]][[x]] &
      topo %in% IARC_TopoMorpRule[[2]][[x]]
  }
}

#' Expressions
#'
#' @noRd
#'
#' @returns Logical vector of same length as the input
"%nin%" <- function(chrvelements, chrvset) {
  !(chrvelements %in% chrvset)
}
