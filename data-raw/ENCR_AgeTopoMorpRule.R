## Prepare the data for AgeTumourRule
## Rule NO.
####################
#read AgeTumourRule
agetumour <- read.csv(paste0(addr,"AgeTumourRule.csv"),
                sep= ";", header = FALSE, stringsAsFactors = FALSE)
colnames(agetumour) <- c("age", "morp", "beha", "topo")
agerange <- strsplit(agetumour$age, "-")
min <- do.call(rbind, agerange)[,1]
max <- do.call(rbind, agerange)[,2]
ages <- lapply(1:29, function(x) min[[x]]:max[[x]])
#deal with morp data

morp <- strsplit(agetumour$morp, ",")
morp <- lapply(morp, recode_morp)
morp <- lapply(morp, as.integer)
#deal with topography data.
# create a function

topo <- strsplit(agetumour$topo, ",")
topo <- lapply(topo, recode_topo)

#generate list of AgeTumourRule
ENCR_AgeTopoMorpRule <- list(
  un_age = ages,
  morp = morp,
  topo = topo
)

rm(ages, morp,topo,agerange, agetumour, max, min)

