check_followup <- function(basi,
                           status,
                           inciden,
                           deathda,
                           lastcontact) {
  c1 <- basi == 0 & !inciden == deathda
  c2 <- status == 2 & is.na(deathda)
  c3 <- !status == 2 & !is.na(deathda)
  c4 <- !is.na(deathda) & !deathda == lastcontact
  c5 <- !is.na(inciden) & !is.na(deathda) & inciden > deathda
}