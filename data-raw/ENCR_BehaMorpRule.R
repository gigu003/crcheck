## code to prepare `BehaviourMorphologyRule` dataset goes here
## Rule NO. 1
# Rule basis equals 0
morp_beha <- read.csv(paste0(addr,"BehaviourMorphologyRule.csv"),
                  sep= ";", header = FALSE)
ENCR_BehaMorpRule <- list(
  morp_beha = paste0(morp_beha$V1, "/", morp_beha$V2)
)
rm(morp_beha)
