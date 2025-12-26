## prepare the data for Rules in SexTopographyRule
## Rules NO. 1
male <- read.csv(paste0(addr,"SexTopographyRule1.csv"),
                sep= ";", header = FALSE, stringsAsFactors = FALSE)
female <- read.csv(paste0(addr,"SexTopographyRule2.csv"),
                 sep= ";", header = FALSE, stringsAsFactors = FALSE)

ENCR_SexTopoRule <- list(male = male$V1, female = female$V1)
rm(male, female)