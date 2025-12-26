## prepare the data for Rules in SexMorphologyRule
## Rules NO. 1
male <- read.csv(paste0(addr,"SexMorphologyRule1.csv"), header = FALSE)
female <- read.csv(paste0(addr,"SexMorphologyRule2.csv"), header = FALSE)

ENCR_SexMorpRule <- list(male = male$V1, female = female$V1)
rm(male, female)
