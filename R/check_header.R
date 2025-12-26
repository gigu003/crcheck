check_header <- function(x, schema = "iarc") {
  cnames <- tolower(colnames(x))
  v_exist <- cnames %in% SchemaHeader$iarc
  all(v_exist)
  names(ValueRange$ncc)
}