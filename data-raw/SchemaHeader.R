## code to prepare `Schema_header` dataset goes here
SchemaHeader <- list(
  ncc = list(
    man = c(
      "registr",
      "id",
      "name",
      "sex",
      "age",
      "birthda",
      "inciden",
      "topo",
      "morp",
      "beha",
      "grad",
      "icd10",
      "deathda",
      "basi",
      "trib",
      "marri",
      "mult",
      "lastcontact",
      "status",
      "caus"
    ),
    suggest = c(
      "clinicalextent",
      "tnm_t",
      "tnm_n",
      "tnm_m",
      "tnm_group",
      "tnm_edit"
    )
  ),
  iacr = list(
    man = c(
      "registr",
      "sex",
      "age",
      "birthda",
      "inciden",
      "topo",
      "morp",
      "beha",
      "grad",
      "basi",
      "icd10"
    ),
    suggest = c(
      "id",
      "name",
      "deathda",
      "mult",
      "lastcontact",
      "status",
      "caus"
    )
  )
)
