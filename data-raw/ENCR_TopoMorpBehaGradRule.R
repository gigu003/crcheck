## code to prepare `TopographyMorphologyBehaviourGrade` dataset goes here

topo_morp_beha_grad <- 
  read.csv(paste0(addr,"TopographyMorphologyBehaviourGrade.csv"), sep = ";")
ENCR_TopoMorpBehaGradRule <- list(
  topo = lapply(strsplit(topo_morp_beha_grad$Topography, ","), recode_topo),
  morp = lapply(strsplit(topo_morp_beha_grad$Morphology, ","), recode_morp),
  beha = lapply(strsplit(topo_morp_beha_grad$Behaviour, ","), as.integer),
  grad = lapply(strsplit(topo_morp_beha_grad$Grade, ","), as.integer)
)
rm(topo_morp_beha_grad)