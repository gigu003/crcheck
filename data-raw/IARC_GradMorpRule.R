## code to prepare `IARC_GradMorpRule` dataset goes here
IARC_GradMorpRule <- list(
  grad = list(r2 = 5:8,
              r3 = 1:4,
              r4 = c(1:4, 6:8),
              r5 = c(1:4, 6, 8),
              r6 = c(1:4, 6, 7),
              r7 = c(1:5, 7:8),
              r8 = c(1:7),
              r9 = 2:8, 
              r10 = c(1, 3:8),
              r11 = c(1:2, 4:8),
              r12 = c(1:3, 5:8)),
  morp = list(r2 = gen_morp(8000:9589),
              r3 = gen_morp(9590:9993),
              r4 = c(9702, 9705:9706, 9708:9709, 9717:9718, 9729, 9827, 9834, 9837),
              r5 = c(9714, 9831),
              r6 = c(9700, 9701, 9712, 9716, 9719),
              r7 = c(gen_morp(9670:9699), 9728, 9823, 9826, 9833, 9836),
              r8 = 9948,
              r9 = c(8331, 9187, 9511),
              r10 = c(8332, 8858, 9083, 9243, 9372),
              r11 = c(8631, 8634),
              r12 = c(8020, 8021, 8805, 9062, 9082, 9390, 9392, 9401, 9451,
                9505, 9512))
)

IARC_GradMorpRule <- list(
  grad = lapply(IARC_GradMorpRule$grad, as.integer),
  morp = lapply(IARC_GradMorpRule$morp, as.integer)
)
