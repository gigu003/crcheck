## prepare the data used for Rules in Grade/Morphology
## Rules NO. 1

addr <- "/Users/qc0824/Documents/2_Areas/R_Packages/jrc-qcs-2.2.8/config/rules/encr2014/"
#convert cns data to list
cns <- read.csv(paste0(addr,"GradeMorphology_CNS.csv"),
                sep= ";", header = FALSE, stringsAsFactors = FALSE)
colnames(cns) <- c("morp", "beha", "grad")
cns <- list(
  morp = as.list(cns$morp),
  beha = as.list(cns$beha),
  grad = lapply(strsplit(cns$grad, ","), as.integer)
  )

#convert heamato data to list
heamato <- read.csv(paste0(addr,"GradeMorphology_Heamato.csv"),
                    sep= ";", header = FALSE, stringsAsFactors = FALSE)
colnames(heamato) <- c("morp", "grad")
heamato <- list(
  morp = lapply(heamato$morp, recode_morp),
  grad = lapply(strsplit(heamato$grad, ","), as.integer)
)

#convert solid data to list
solid <- read.csv(paste0(addr,"GradeMorphology_Solid.csv"),
                  sep= ";", header = FALSE, stringsAsFactors = FALSE)
colnames(solid) <- c("morp", "beha", "grad")
solid <- list(
  morp = lapply(solid$morp, recode_morp),
  beha = as.list(solid$beha),
  grad = lapply(strsplit(solid$grad, ","), as.integer)
  )

ENCR_GradMorpRule <- list(
  heamato = heamato,
  cns = cns,
  solid = solid
)
rm (cns, heamato, solid)
