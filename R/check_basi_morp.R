#' Check consistency between basis of diagnosis and morphology code
#'
#' @template basi
#' @template morp
#' @template topo
#' @template beha
#' @template quiet
#'
#' @template return
#' @export
#'
#' @examples
#'  basi <- c(1, 1, 1, 1, 5, 7, 2)
#'  beha <- c(3, 3, 3, 3, 3, 3, 3)
#'  morp <- c(8000, 8720, 9120, 9510, 8023, 8001, 9180)
#'  topo <- c("C15.2","C15.2", "C44.0", "C69.1", "C42.1", "C15.2", "C15.2")
#'  res <- check_basi_morp(basi, morp, topo, beha)
#'  print(res)
check_basi_morp <- function(
  basi,
  morp,
  topo = NULL,
  beha = NULL,
  quiet = FALSE
) {
  has_topo <- !is.null(topo)
  has_beha <- !is.null(beha)

  if (is.null(topo)) {
    topo <- rep(NA_character_, length(basi))
  }
  if (is.null(beha)) {
    beha <- rep(NA_integer_, length(basi))
  }

  check_equal_length(
    basi,
    morp,
    topo,
    beha,
    arg_names = c("basi", "morp", "topo", "beha")
  )
  check_flag(quiet, "quiet")

  basi <- as.integer(basi)
  morp <- as.integer(gsub("[^0-9]", "", morp))
  beha <- as.integer(beha)
  topo <- add_decimal(topo)
  topo <- gsub("[[:space:]]+", "", topo)
  result1 <- lapply(1:2, function(i) {
    if (i == 1) {
      basi_morp <- basi %in%
        IARC_BasiMorpRule[["basi"]][[i]] &
        morp %nin% IARC_BasiMorpRule[["morp"]][[i]]
    } else if (i == 2) {
      basi_morp <- basi %in%
        IARC_BasiMorpRule[["basi"]][[i]] &
        morp %in% IARC_BasiMorpRule[["morp"]][[i]]
      if (has_topo) {
        basi_morp <- basi_morp & (topo %nin% IARC_BasiMorpRule[["topo"]][[i]])
      }
    }
    basi_morp
  })

  # define a function
  check_basi <- function(x, bs, rule = rule1) {
    morp_list <- rule[[1]][[x]]
    beha_list <- rule[[2]][[x]]
    topo_list <- rule[[3]][[x]]
    if (length(topo_list) == 0) {
      topo_list <- gen_topo(0:809)
    }
    morp_logi <- morp %in% morp_list
    beha_logi <- beha %in% beha_list
    topo_logi <- if (has_topo) topo %nin% topo_list else rep(TRUE, length(basi))
    basi == bs & morp_logi & beha_logi & topo_logi
  }

  # check allowed morphology code when basi code equals 0
  basi0 <- basi == 0 &
    beha == 3 &
    morp %nin% unlist(ENCR_BasiMorpRule$basi0[[1]])

  # check allowed morphology and behaviour code when basi equals 1
  rule1 <- ENCR_BasiMorpRule[[2]]
  rule1[[3]][[1]] <- gen_topo(0:809)
  result <- lapply(1:5, check_basi, bs = 1)
  basi1 <- Reduce(`|`, result)

  # check allowed morphology and behaviour code when basi equals 2
  rule2 <- ENCR_BasiMorpRule[[3]]
  rule2[[3]][[1]] <- gen_topo(0:809)
  result <- lapply(1:65, check_basi, bs = 2, rule = rule2)
  basi2 <- Reduce(`|`, result)

  # check allowed morphology and behaviour code when basi equals 4
  rule4 <- ENCR_BasiMorpRule[[4]]
  result <- lapply(1:20, check_basi, bs = 4, rule = rule4)
  basi4 <- Reduce(`|`, result)

  # check allowed morphology and behaviour code when basi equals 5
  morp_list <- ENCR_BasiMorpRule[[5]][[1]][[2]]
  basi5 <- basi == 5 & beha == 3 & morp %in% morp_list

  # check allowed morphology and behaviour cocde when basi equals 7
  morp_list <- ENCR_BasiMorpRule[[7]][[1]][[1]]
  basi7 <- basi == 7 & beha == 3 & morp %in% morp_list

  # check allowed morphology and behaviour cocde when basi equals 8
  morp_list <- ENCR_BasiMorpRule[[8]]
  basi8 <- basi == 8 & morp %nin% morp_list

  #result2 <- list(basi0, basi1, basi2, basi4, basi5, basi7, basi8)
  result2 <- list(basi0, basi1)
  loc_basi_morp <- lapply(append(result1, result2), which)
  check <- unique(unlist(loc_basi_morp))
  groups <- character(length(check))
  values <- character(length(check))
  for (i in 1:4) {
    pos <- loc_basi_morp[[i]]
    idx <- match(pos, check)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      next
    }
    groups[idx] <- paste0(
      ifelse(groups[idx] == "", "G-", paste0(groups[idx], "/")),
      sprintf("%02d", i)
    )
    values[idx] <- paste0(
      basi[pos],
      "/",
      morp[pos],
      "/",
      topo[pos],
      "/",
      beha[pos]
    )
  }

  # output result
  res <- structure(
    list(
      check_item = "Basi/Morp/Topo/Beha",
      check = check,
      data = values,
      type = "W-BDMO",
      group = groups
    ),
    class = c("check", "basi/morp")
  )

  if (!quiet) {
    summary(res)
  }

  invisible(res)
}
