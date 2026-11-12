#' Check consistency between grade, morphology, behaviour
#'
#' @template morp
#' @template beha
#' @template grad
#' @template quiet
#'
#' @template return
#' @export
#'
check_grad_morp_beha <- function(grad, morp, beha, quiet = FALSE) {
  if (length(grad) != length(morp) || length(grad) != length(beha)) {
    stop("`grad`, `morp`, and `beha` must have the same length.", call. = FALSE)
  }

  morp <- gsub("[^0-9]", "", morp)
  morp <- as.integer(morp)
  beha <- as.integer(beha)
  grad <- as.integer(grad)

  ## Rule 1 check
  result <- lapply(1:6, function(x) {
    !is.na(morp) &
      !is.na(beha) &
      beha %in% as.integer(names(IARC_BehaMorpRule)[x]) &
      morp %nin% IARC_BehaMorpRule[[x]]
  })
  loc_beha_morp <- lapply(result, which)
  check1 <- unique(unlist(loc_beha_morp))
  groups1 <- character(length(check1))
  values1 <- paste0(morp[check1], "/", beha[check1])
  if (length(check1) == 0) {
    values1 <- character(0)
  }
  items1 <- rep("Morp/Beha", length(check1))
  types1 <- rep("W-MOBE", length(check1))
  for (i in 1:6) {
    pos <- loc_beha_morp[[i]]
    idx <- match(pos, check1)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups1[idx] <- paste0(
      ifelse(groups1[idx] == "", "G-", paste0(groups1[idx], "/")),
      sprintf("%02d", i)
    )
  }

  ## Rule 2 check
  beha_grad <- beha < 3 & grad < 9 & !is.na(beha) & !is.na(grad)
  check2 <- which(beha_grad)
  groups2 <- rep("G-01", length(check2))
  values2 <- paste0(beha[check2], "/", grad[check2])
  if (length(check2) == 0) {
    values2 <- character(0)
  }
  items2 <- rep("Beha/Grad", length(check2))
  types2 <- rep("W-BEGR", length(check2))

  ## Rule 3 check
  result <- lapply(1:11, function(x) {
    grad %in%
      IARC_GradMorpRule[["grad"]][[x]] &
      morp %in% IARC_GradMorpRule[["morp"]][[x]]
  })
  loc_grad_morp <- lapply(result, which)
  check3 <- unique(unlist(loc_grad_morp))
  groups3 <- character(length(check3))
  values3 <- paste0(morp[check3], "/", grad[check3])
  if (length(check3) == 0) {
    values3 <- character(0)
  }
  items3 <- rep("Morp/Grad", length(check3))
  types3 <- rep("W-MOGR", length(check3))
  for (i in 1:11) {
    pos <- loc_grad_morp[[i]]
    idx <- match(pos, check3)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups3[idx] <- paste0(
      ifelse(groups3[idx] == "", "G-", paste0(groups3[idx], "/")),
      sprintf("%02d", i)
    )
  }

  # Construct result
  res <- structure(
    list(
      check_item = c(items1, items2, items3),
      check = c(check1, check2, check3),
      data = c(values1, values2, values3),
      type = c(types1, types2, types3),
      group = c(groups1, groups2, groups3)
    ),
    class = c("check", "grad/morp/beha")
  )

  if (!quiet) {
    summary(res)
  }
  invisible(res)
}
