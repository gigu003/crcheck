#' Conduct quality check for cancer registration data.
#'
#' @rdname cr_check
#' @param x Object contains cancer registration data
#' @inheritParams param_std
#' @param registr The record number of cancer case
#' @param sex The birth sex of cancer case
#' @param age The age of cancer case
#' @param birthda The birth date of cancer case
#' @param inciden The incidence date of cancer case
#' @param topo The cancer site code of cancer case coded as ICDO3.2
#' @param morp The Morphology code of cancer case, coded as ICDO3.2
#' @param beha The behaviour code of cancer case, coded as ICDO3.2
#' @param grad The grade code of cancer code, coded as ICDO3.2
#' @param icd10 The ICD10 code corresponding to ICD3.2
#' @param ... Other parameters
#'
#' @return A data frame contain the check result.
#' @export
#'
cr_check <- function(x,
                    std = "iacr",
                    registr = "registr",
                    sex = "sex",
                    age = "age",
                    birthda = "birthda",
                    inciden = "inciden",
                    topo = "topo",
                    morp = "morp",
                    beha = "beha",
                    grad = "grad",
                    icd10 = "icd10",
                    ...) {
  UseMethod("cr_check", x)
}

#' Conduct quality check for cancer registration data.
#'
#' @rdname cr_check
#' @method cr_check data.frame
#' @return A data frame contain the check result.
#' @export
#'
#'
cr_check.data.frame <- function(x,
                               std = "iacr",
                               registr = "registr",
                               sex = "sex",
                               age = "age",
                               birthda = "birthda",
                               inciden = "inciden",
                               topo = "topo",
                               morp = "morp",
                               beha = "beha",
                               grad = "grad",
                               icd10 = "icd10",
                               ...) {
  x <- x[!is.na(x$registr),]
  x$age <- as.numeric(x$age)
  results <- list(
    age = check_age_birth_inci(x$age, x$birthda, x$inciden, quiet = TRUE),
    sex = check_sex(x$sex, quiet = TRUE),
    topo = check_topo(x$topo, quiet = TRUE),
    grade = check_grad(x$grad, quiet = TRUE),
    beha_morp = check_morp_beha(x$morp, x$beha, std = std, quiet = TRUE),
    age_topo_morp = check_age_topo_morp(x$age, x$topo, x$morp, std = std,
                                        quiet = TRUE),
    topo_morp = check_topo_morp(x$topo, x$morp, std = std, quiet = TRUE),
    sex_topo = check_sex_topo(x$sex, x$topo, std = std, quiet = TRUE),
    sex_morp = check_sex_morp(x$sex, x$morp, std = std, quiet = TRUE),
    beha_topo = check_beha_topo(x$beha, x$topo, quiet = TRUE),
    grade_morp = check_morp_grad(x$morp, x$beha, x$grad, std= std, quiet = TRUE),
    basi_morp =check_basi_morp(x$basi, x$topo, x$morp, x$beha, std = std,
                               quiet = TRUE)
    )
  res1 <- lapply(results, print)
  res <- do.call(rbind, res1)
  res$id <- x$registr[res$id]
  rownames(res) <- NULL
  return(res)
}

#' Conduct quality check for cancer registration data.
#'
#' @rdname cr_check
#' @method cr_check canreg
#'
#' @return A data frame with check results.
#' @export
#'
cr_check.canreg <- function(x, ...) {
  fbcases <- x$FBcases
  res <- cr_check(fbcases, ...)
  return(res)
}
