## code to prepare `TopographyMorphologyBehaviourGrade` dataset goes here

topo_morp_beha_grad <-
  read.csv(
    file.path(encr2014, "TopographyMorphologyBehaviourGrade.csv"),
    sep = ";"
  )
ENCR_TopoMorpBehaGradRule <- list(
  topo = recode_topo(topo_morp_beha_grad$Topography, sep = ","),
  morp = recode_morp(topo_morp_beha_grad$Morphology, sep = ","),
  beha = lapply(strsplit(topo_morp_beha_grad$Behaviour, ","), as.integer),
  grad = lapply(strsplit(topo_morp_beha_grad$Grade, ","), as.integer)
)
rm(topo_morp_beha_grad)
