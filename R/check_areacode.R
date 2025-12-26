check_areacode <- function(areacode) {
  # Check if the province code existed.
  provs <- c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65, 71, 81, 82)
  prov_logi <- substr(areacode, 1, 2) %in% provs
  # Check if the district code is valid (6 digits)
  format_logi <- grepl("^\\d{6}$", areacode)
  check <- prov_logi & format_logi
}