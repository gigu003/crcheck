## code to prepare `IARC_BasiMorpRule` dataset goes here
IARC_BasiMorpRule <- list(
  basi <- list(c(1:4), c(1:4)),
  morp <- list(c(8000, gen_morp(8150:8154), 8170, gen_morp(8270:8281),8800,
                 8960, 9100, 9140, 9380, 9384, 9500, 9510, gen_morp(9530:9539),
                 9590, 9732, 9761, 9800),
               c(8720)),
   topo <- list(c(gen_topo(0:809)), c(gen_topo(c(690:699, 440:449))))
)
