## code to prepare `ICDO3toICD10Rule` dataset goes here

icdo3_to_icd10_rule_dir <- rule_source_path("ICDO3toICD10Rule")

read_icdo3_to_icd10_rule <- function(file) {
  read.csv(
    file.path(icdo3_to_icd10_rule_dir, file),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

morp_beha <- read_icdo3_to_icd10_rule("rules_morp_beha.csv")
morp_beha <- unique(morp_beha[c("morp", "beha", "icd10")])

topo_beha <- read_icdo3_to_icd10_rule("rules_topo_beha.csv")
topo_beha <- topo_beha[c("topo", "beha", "icd10")]

topo_morp_beha <- read_icdo3_to_icd10_rule("rules_topo_morp_beha.csv")
topo_morp_beha <- topo_morp_beha[c("topo", "morp", "beha", "icd10")]

metadata <- read_icdo3_to_icd10_rule("rule_metadata.csv")

expand_icdo3_to_icd10_tmb <- function(rule) {
  out <- vector("list", nrow(rule))

  for (i in seq_len(nrow(rule))) {
    topo <- recode_topo(rule$topo[i], simplify = TRUE, sort_result = FALSE)
    morp <- recode_morp(rule$morp[i], simplify = TRUE, sort_result = FALSE)

    if (length(topo) == 0L || length(morp) == 0L || anyNA(morp)) {
      next
    }

    key <- as.vector(outer(topo, sprintf("%04d", morp), paste, sep = "|"))
    key <- paste(key, rule$beha[i], sep = "|")
    out[[i]] <- stats::setNames(rep(rule$icd10[i], length(key)), key)
  }

  res <- unlist(out, use.names = TRUE)
  res[!duplicated(names(res))]
}

expand_icdo3_to_icd10_mb <- function(rule) {
  out <- vector("list", nrow(rule))

  for (i in seq_len(nrow(rule))) {
    morp <- recode_morp(rule$morp[i], simplify = TRUE, sort_result = FALSE)

    if (length(morp) == 0L || anyNA(morp)) {
      next
    }

    key <- paste(sprintf("%04d", morp), rule$beha[i], sep = "|")
    out[[i]] <- stats::setNames(rep(rule$icd10[i], length(key)), key)
  }

  res <- unlist(out, use.names = TRUE)
  res[!duplicated(names(res))]
}

expand_icdo3_to_icd10_tb <- function(rule) {
  out <- vector("list", nrow(rule))

  for (i in seq_len(nrow(rule))) {
    topo <- recode_topo(rule$topo[i], simplify = TRUE, sort_result = FALSE)

    if (length(topo) == 0L) {
      next
    }

    key <- paste(topo, rule$beha[i], sep = "|")
    out[[i]] <- stats::setNames(rep(rule$icd10[i], length(key)), key)
  }

  res <- unlist(out, use.names = TRUE)
  res[!duplicated(names(res))]
}

icdo3_to_icd10_maps <- list(
  topo_morp_beha = expand_icdo3_to_icd10_tmb(topo_morp_beha),
  morp_beha = expand_icdo3_to_icd10_mb(morp_beha),
  topo_beha = expand_icdo3_to_icd10_tb(topo_beha)
)

ICDO3toICD10Rule <- list(
  topo_morp_beha = topo_morp_beha,
  morp_beha = morp_beha,
  topo_beha = topo_beha,
  metadata = metadata,
  maps = icdo3_to_icd10_maps
)

rm(
  icdo3_to_icd10_rule_dir,
  read_icdo3_to_icd10_rule,
  expand_icdo3_to_icd10_tmb,
  expand_icdo3_to_icd10_mb,
  expand_icdo3_to_icd10_tb,
  icdo3_to_icd10_maps,
  morp_beha,
  topo_beha,
  topo_morp_beha,
  metadata
)
