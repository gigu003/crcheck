# crcheck 0.1.0.9000

## Breaking changes

* Public API in 0.1.0 now consolidates legacy item checks and rule displays under the current interfaces; see the migration guide for replacements (#noissue).
* `rule_info()`, rule metadata attributes, and `check_version` are removed; internal dictionaries are rebuilt directly from the maintained generation scripts (#noissue).

## New features

* `check_duplicate_person()` now identifies two or more records belonging to the same person, reports pair-level probabilities, and forms strict duplicate groups (#noissue).
* `check_icd10()` now checks whether an existing ICD-10 code is consistent with the code converted from topography, morphology, and behaviour, and is included in `cr_check()` (#noissue).
* `check_multiple_primary()` now detects possible multiple primary cancers using the IARC/IACR 2004 rules (#noissue).
* `icd10_to_topo()` converts ICD-10 codes to possible ICD-O-3 topography codes (#noissue).
* `label_message()` now adds Chinese or English labels to check outputs using the internal output message dictionary (#noissue).
* `std_icd10()` now standardizes raw ICD-10 codes by extracting the first 5 characters, removing decimal suffixes, and generating valid codes via `gen_icd10()`. It supports 2-digit code auto-expansion and fallback for invalid 3-digit codes (#noissue).
* `output-message.csv` now provides complete Chinese and English labels for all current check code and group combinations (#noissue).
