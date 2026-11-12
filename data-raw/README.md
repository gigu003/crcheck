# Rebuilding internal rule data

Keep the private dictionary inputs in the ignored `data-raw/private/`
directory. From an R or RStudio session whose working directory is the package
root, run:

```r
source("data-raw/create_CheckRules.R")
```

The script sources the maintained rule generators and immediately overwrites
`R/sysdata.rda` with the 22 runtime dictionary objects. It uses
`usethis::use_data()` with `internal = TRUE`, `overwrite = TRUE`, xz compression,
and serialization version 3.

Private inputs are not downloaded or added to version control. The build does
not maintain a source manifest, provenance metadata, or checksums.
