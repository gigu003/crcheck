## code to prepare `req_vars_func` dataset goes here
Req_Vars_Func <- list(
  list(
    name = "age",
    req_vars = c("age", "birthda", "inciden"),
    func = "check_age_birth_inci"
  ),
  list(
    name = "age_topo_morp",
    req_vars = c("age", "topo", "morp"),
    func = "check_age_topo_morp"
  ),
  list(
    name = "topo_morp",
    req_vars = c("topo", "morp"),
    func = "check_topo_morp"
  ),
  list(
    name = "sex_topo_morp",
    req_vars = c("sex", "topo", "morp"),
    func = "check_sex_topo_morp"
  ),
  list(
    name = "beha_topo",
    req_vars = c("beha", "topo"),
    func = "check_beha_topo"
  ),
  list(
    name = "basi_morp",
    req_vars = c("basi", "morp", "topo", "beha"),
    func = "check_basi_morp"
  ),
  list(
    name = "grad_morp",
    req_vars = c("grad", "morp", "beha"),
    func = "check_grad_morp_beha"
  ),
  list(
    name = "grad_topo_morp_beha",
    req_vars = c("grad", "topo", "morp", "beha"),
    func = "check_grad_topo_morp_beha"
  ),
  list(
    name = "icd10_topo_morp_beha",
    req_vars = c("topo", "morp", "beha", "icd10"),
    func = "check_icd10"
  ),
  list(
    name = "birthda",
    req_vars = c("birthda", "inciden", "lastcontact"),
    func = "check_birth_inci_last"
  ),
  list(
    name = "followup",
    req_vars = c("basi", "status", "inciden", "deathda", "lastcontact"),
    func = "check_followup"
  )
)
