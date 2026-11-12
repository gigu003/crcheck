## code to prepare `ValueRange` dataset goes here

topo_to_icd10 <- read.csv(rule_source_path("topo.csv"))
topo <- gsub("\\.", "", topo_to_icd10$topo)
data <- read.csv(rule_source_path("morp.csv"))
morp <- unique(substr(data$morp, 1, 4))
## not exist codes in icd10
not_icd10 <- list(
  C = c(
    7,
    11:19,
    25:27,
    32:38,
    42:47,
    53:57,
    63:67,
    71:79,
    82:87,
    92:97,
    105:107,
    114:117,
    121:129,
    133:137,
    141,
    143:147,
    149,
    156,
    157,
    167,
    174:177,
    191:199,
    201:209,
    213:217,
    219,
    225,
    226,
    228,
    231:239,
    242:247,
    255,
    256,
    262:267,
    270:299,
    302:309,
    314:317,
    324:327,
    331:339,
    344:347,
    350:369,
    371:379,
    385:387,
    389,
    391:397,
    404:407,
    415:417,
    420:429,
    452:456,
    458,
    464:466,
    477,
    483:487,
    489,
    497,
    507,
    513:517,
    521:529,
    532:537,
    543:547,
    551:559,
    561:569,
    575,
    576,
    581:589,
    590:599,
    603:607,
    611:619,
    622:628,
    633:636,
    641:649,
    651:659,
    661:669,
    682:687,
    697,
    702:708,
    726:727,
    731:739,
    742:748,
    756:757,
    766,
    769,
    776,
    777,
    789,
    799,
    801:809,
    814,
    815,
    816,
    818,
    823:826,
    828,
    846:849,
    903:909,
    916,
    918,
    926,
    928,
    852:856,
    858,
    860:879,
    884:886,
    888,
    890:899,
    933:936,
    938,
    946,
    948,
    949,
    953:956,
    958,
    964:966,
    968,
    971:979
  ),
  D = c(
    3:9,
    16,
    18,
    25:29,
    52:56,
    58,
    62:66,
    68,
    77:79,
    80:89,
    94:96,
    98,
    108,
    111:116,
    118,
    138,
    145:149,
    153:156,
    158,
    178,
    182:189,
    192:196,
    198,
    202:209,
    218,
    219,
    228,
    238,
    241:249,
    253:259,
    262:266,
    268,
    271:279,
    283:286,
    288,
    295,
    296,
    298,
    305,
    306,
    308,
    317,
    318,
    322:328,
    335,
    336,
    338,
    341:349,
    362:366,
    368,
    378,
    387,
    388,
    389,
    393:396,
    398,
    402:406,
    408,
    415,
    416,
    418,
    422:428,
    435,
    436,
    438,
    451:459,
    465,
    466,
    468,
    474:476,
    478,
    488
  )
)

ValueRange <- list(
  not_icd10 = not_icd10,
  iarc = list(
    sex = c(1, 2),
    age = c(0:105),
    birthda = NA,
    iinciden = NA,
    topo = topo,
    morp = as.integer(morp),
    beha = c(0:3, 6, 9),
    grad = c(1:9),
    basi = c(0:2, 4, 5, 7:9)
  ),
  ncc = list(
    registr = NA,
    id = NA,
    sex = c(1, 2, 9),
    age = c(0:121),
    birthda = NA,
    inciden = NA,
    topo = topo,
    morp = as.integer(morp),
    beha = c(0:3),
    grad = c(1:9),
    basi = c(0:9),
    icd10 = NA,
    deathda = NA,
    mult = c(0:9),
    trib = sprintf("%02d", c(1:56, 97, 98, 99)),
    occu = c(11, 13, 17, 21, 24, 27, 31, 37, 51, 54, 70, 80, 90),
    marri = c(1:5, 9),
    lastcontact = NA,
    status = c(1:4, 9),
    caus = c(1, 2, 9)
  )
)

ValueRange <- list(
  id = list(
    form = "^\\d{17}[0-9X]$",
    range = NULL,
    std = "GB11643-1999",
    allow_missing = TRUE
  ),
  name = list(
    form = "^[\\u4e00-\\u9fa5]+$",
    range = NULL,
    std = "ncc",
    allow_missing = FALSE
  ),
  sex = list(
    form = "^[0-9]$",
    range = c(1L, 2L),
    std = "iacr",
    allow_missing = FALSE
  ),
  age = list(
    form = "^[0-9]{1,3}",
    range = c(0:105),
    std = "iacr",
    allow_missing = FALSE
  ),
  topo = list(
    form = "^C\\d{2}\\.\\d$",
    range = topo,
    std = "ICD-O-3.2",
    allow_missing = FALSE
  ),
  morp = list(
    form = "^[0-9]{4}",
    range = as.integer(morp),
    std = "ICD-O-3.2",
    allow_missing = FALSE
  ),
  beha = list(
    form = "^[0-9]$",
    range = c(0:3),
    std = "ICD-O-3.2",
    allow_missing = FALSE
  ),
  grad = list(
    form = "^[0-9]$",
    range = c(1:9),
    std = "ICD-O-3.2",
    allow_missing = FALSE
  ),
  basi = list(
    form = "^[0-9]$",
    range = c(0:2, 4:7, 9),
    std = "iacr",
    allow_missing = FALSE
  ),
  mult = list(
    form = "^[0-9]$",
    range = c(0, 1),
    std = "ncc",
    allow_missing = FALSE
  ),
  trib = list(
    form = "^[0-9]{2}",
    range = sprintf("%02d", c(1:56, 97, 98, 99)),
    std = "GB3304-91",
    allow_missing = TRUE
  ),
  occu = list(
    form = "^[0-9]{2}",
    range = c(11, 13, 17, 21, 24, 27, 31, 37, 51, 54, 70, 80, 90),
    std = "GB/T2261.4",
    allow_missing = TRUE
  ),
  marri = list(
    form = "^[0-9]{2}",
    range = c(1:5, 9),
    std = "CR4",
    allow_missing = TRUE
  ),
  status = list(
    form = "^[0-9]{2}",
    range = c(1:4, 9),
    std = "CR4",
    allow_missing = FALSE
  ),
  caus = list(
    form = "^[0-9]{2}",
    range = c(1:4, 9),
    std = "CR4",
    allow_missing = TRUE
  ),
  not_icd10 = not_icd10
)


ValueRanges <- list(
  COMM = list(
    id = list(form = "^\\d{17}[0-9X]$", range = NULL, allow_missing = TRUE),
    name = list(
      form = "^[\\u4e00-\\u9fa5]+$",
      range = NULL,
      allow_missing = FALSE
    )
  ),
  IACR = list(
    sex = list(form = "^[0-9]$", range = c(1L, 2L), allow_missing = FALSE),
    age = list(form = "^[0-9]{1,3}", range = c(0:105), allow_missing = FALSE),
    topo = list(form = "^C\\d{2}\\.\\d$", range = topo, allow_missing = FALSE),
    morp = list(
      form = "^[0-9]{4}",
      range = as.integer(morp),
      allow_missing = FALSE
    ),
    beha = list(form = "^[0-9]$", range = c(0:3), allow_missing = FALSE),
    grad = list(form = "^[0-9]$", range = c(1:9), allow_missing = FALSE),
    basi = list(form = "^[0-9]$", range = c(0:2, 4:7, 9), allow_missing = FALSE)
  ),
  NCC = list(
    mult = list(form = "^[0-9]$", range = c(0, 1), allow_missing = FALSE),
    trib = list(
      form = "^[0-9]{2}",
      range = sprintf("%02d", c(1:56, 97, 98, 99)),
      allow_missing = TRUE
    ),
    occu = list(
      form = "^[0-9]{2}",
      range = c(11, 13, 17, 21, 24, 27, 31, 37, 51, 54, 70, 80, 90),
      allow_missing = TRUE
    ),
    marri = list(
      form = "^[0-9]{2}",
      range = c(10, 20:23, 30, 40, 90),
      allow_missing = TRUE
    ),
    status = list(form = "^[0-9]{2}", range = c(1:4, 9), allow_missing = FALSE),
    caus = list(form = "^[0-9]{2}", range = c(1:4, 9), allow_missing = TRUE)
  ),
  CR4 = list(
    mult = list(form = "^[0-9]$", range = c(0, 1), allow_missing = FALSE),
    marri = list(form = "^[0-9]{1}", range = c(1:5, 9), allow_missing = TRUE),
    status = list(form = "^[0-9]{2}", range = c(1:4, 9), allow_missing = FALSE),
    caus = list(form = "^[0-9]{2}", range = c(1:4, 9), allow_missing = TRUE)
  ),
  not_icd10 = not_icd10
)


## 生成 topo 词典
maincode <- unique(substr(gen_topo(0:809), 1, 3))
minteger <- as.integer(gsub("C", "", maincode))
dict_topo <- lapply(minteger * 10, function(x) gen_topo(x:(x + 9)))
names(dict_topo) <- maincode

## 生成 ICD10 词典
maincode <- unique(c(
  substr(gen_icd10(0:979), 1, 3),
  substr(gen_icd10(0:489, "D"), 1, 3)
))
dict_icd10 <- lapply(maincode, function(x) {
  init_letter <- substr(x, 1, 1)
  minteger <- as.integer(gsub("[^0-9]", "", x)) * 10
  gen_icd10(minteger:(minteger + 9), init = init_letter)
})
names(dict_icd10) <- maincode


rm(topo_to_icd10, topo, data, morp, not_icd10, maincode, minteger)
