## Code to prepare `IARC_MultiplePrimaryRule` internal dataset.
##
## Source:
## IARC/IACR. International Rules for Multiple Primary Cancers (ICD-O Third
## Edition), July 2004. Multiple Primary Neoplasms.

multiple_topo <- list(
  g1 = gen_topo(10:29),
  g2 = gen_topo(c(0:9, 30:69)),
  g3 = gen_topo(c(90:109, 120:149)),
  g4 = gen_topo(190:209),
  g5 = gen_topo(230:249),
  g6 = gen_topo(330:349),
  g7 = gen_topo(400:419),
  g8 = gen_topo(650:689)
)

multiple_topo_summary <- data.frame(
  group = sprintf("T-%02d", seq_along(multiple_topo)),
  site = c(
    "C01-C02",
    "C00,C03-C06",
    "C09-C10,C12-C14",
    "C19-C20",
    "C23-C24",
    "C33-C34",
    "C40-C41",
    "C65-C68"
  ),
  label = c(
    "Base of tongue; other and unspecified parts of tongue",
    "Lip; gum; floor of mouth; palate; other and unspecified parts of mouth",
    "Tonsil; oropharynx; pyriform sinus; hypopharynx; other and ill-defined sites in lip, oral cavity and pharynx",
    "Rectosigmoid junction; rectum",
    "Gallbladder; other and unspecified parts of biliary tract",
    "Trachea; bronchus and lung",
    "Bones, joints and articular cartilage",
    "Renal pelvis; ureter; bladder; other and unspecified urinary organs"
  ),
  simultaneous_code = c(
    "C02.9",
    "C06.9",
    "C14.0",
    "C20.9",
    "C24.9",
    "C34.9",
    "C41.9",
    "C68.9"
  ),
  stringsAsFactors = FALSE
)

multiple_morp_text <- c(
  "8051-8084,8120-8131",
  "8090-8110",
  "8140-8149,8160-8162,8190-8221,8260-8337,8350-8551,8570-8576,8940-8941",
  "8030-8046,8150-8157,8170-8180,8230-8255,8340-8347,8560-8562,8580-8671",
  "8010-8015,8020-8022,8050",
  "8680-8713,8800-8921,8990-8991,9040-9044,9120-9125,9130-9136,9141-9252,9370-9373,9540-9582",
  "9050-9055",
  "9840,9861-9931,9945-9946,9950,9961-9964,9980-9987",
  "9670-9699,9728,9731-9734,9761-9767,9769,9823-9826,9833,9836,9940",
  "9700-9719,9729,9768,9827-9831,9834,9837,9948",
  "9650-9667",
  "9740-9742",
  "9750-9758",
  "9590-9591,9596,9727,9760,9800-9801,9805,9820,9832,9835,9860,9960,9970,9975,9989",
  "9140",
  "8720-8790,8930-8936,8950-8983,9000-9030,9060-9110,9260-9365,9380-9539",
  "8000-8005"
)

multiple_morp <- lapply(multiple_morp_text, recode_morp)
names(multiple_morp) <- sprintf("M-%02d", seq_along(multiple_morp))

multiple_morp_summary <- data.frame(
  group = names(multiple_morp),
  label = c(
    "Squamous and transitional cell carcinoma",
    "Basal cell carcinomas",
    "Adenocarcinomas",
    "Other specific carcinomas",
    "Unspecified carcinomas (NOS)",
    "Sarcomas and soft tissue tumours",
    "Mesothelioma",
    "Myeloid",
    "B-cell neoplasms",
    "T-cell and NK-cell neoplasms",
    "Hodgkin lymphoma",
    "Mast-cell tumours",
    "Histiocytes and accessory lymphoid cells",
    "Unspecified haematopoietic and lymphoid types",
    "Kaposi sarcoma",
    "Other specified types of cancer",
    "Unspecified types of cancer"
  ),
  morp = multiple_morp_text,
  nonspecific = seq_along(multiple_morp_text) %in% c(5L, 14L, 17L),
  systemic = seq_along(multiple_morp_text) %in% c(8L:14L, 15L),
  stringsAsFactors = FALSE
)

IARC_MultiplePrimaryRule <- list(
  topo = multiple_topo,
  topo_summary = multiple_topo_summary,
  morp = multiple_morp,
  morp_summary = multiple_morp_summary,
  nonspecific_morp_groups = sprintf("M-%02d", c(5L, 14L, 17L)),
  systemic_morp_groups = sprintf("M-%02d", c(8L:14L, 15L))
)

rm(
  multiple_topo,
  multiple_topo_summary,
  multiple_morp_text,
  multiple_morp,
  multiple_morp_summary
)
