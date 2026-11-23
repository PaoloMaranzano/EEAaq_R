# EEAaq - notes for coding agents

R package (root of this repository = package root). Material that is not
part of the built package lives in `dev/` (maintenance scripts),
`archive/` (past releases and the original 2023 repository) and
`Support_files/` (run-time metadata); all three are in `.Rbuildignore`.

## Working rules

- **Work directly on `main`, no branches** (the maintainer's choice since
  2026-11-22; the branches served only the reorganisation of November 2026):
  commit on `main` and push to `origin main` when the maintainer asks.
- **`Support_files/` is a public API.** Every installed version of EEAaq
  downloads `LAU.rds`, `NUTS.rds`, `stations.rds`, `pollutant.rds` and
  `missing_cities.rds` from `raw/refs/heads/main/Support_files/`, so a push
  to `main` changes them for every user at once. Never move or rename them,
  and never change their columns without checking first that the released
  versions still work. Refresh them with `Rscript dev/update-support-files.R`.
- **No secrets in the repository.** A GitHub token was committed in
  `Support_files/upload_git.Rd` and `upload_on_GH.txt` until November 2026
  and is still in the history: treat it as compromised. Tokens belong in
  the git credential store (read by `gitcreds`) or in `~/.Renviron`, never
  in a file of the repository.
- **CRAN and the network.** The package was archived in August 2026 because
  the vignette downloaded data while CRAN built it. Nothing that runs during
  `R CMD check` may use the internet: examples that need it go in
  `\dontrun{}`, and the vignette is precomputed. Edit
  `vignettes/vignettes.Rmd.orig`, never `vignettes.Rmd`, then run
  `Rscript dev/precompute-vignette.R` (needs network) and commit the source,
  the result and `vignettes/figures/` together.
- `R CMD check` must run from a folder that is not synchronized by
  OneDrive/Google Drive: `Rscript dev/check-package.R` copies the sources to
  the temporary directory, builds and checks with `--as-cran`; online it
  also runs the CRAN incoming checks on every URL and file link, as CRAN
  does (R-hub skips them); add `--offline` to reproduce a CRAN machine
  without network. R-hub:
  `Rscript dev/run-rhub-checks.R` (checks the copy on GitHub, so push first;
  needs a PAT with the workflow scope, `--dry-run` to test without starting).
- `README.md` and `NEWS.md` are part of the built package and CRAN checks
  their links: a link to something that is only on GitHub (`dev/`,
  `Support_files/`, `archive/`) must be a full `https://github.com/...` URL,
  never a relative path. Do not link `dd.eionet.europa.eu`: it timed out
  from the CRAN check machine (November 2026).
- Temporary files: always inside `tempfile()`/`tempdir()`, and remove only
  the folder you created (never `unlink(tempdir())`).
- The `vignettes/function-map.Rmd` vignette runs no R code beyond
  `knitr::include_graphics()`, so it is built at check time; only
  `vignettes.Rmd` is precomputed.
- `NAMESPACE` and `man/` are generated: edit the roxygen comments and run
  `roxygen2::roxygenise()`.
- `dev/maintainer-workflow.R` is the maintainer's step-by-step script in
  RStudio (change, debug, check, push, CRAN release); keep it in line with
  the other scripts of `dev/` when they change.
- Map of the package: after adding, removing or renaming a function, or
  changing who calls whom, run `Rscript dev/make-function-map.R`: it scans
  `R/`, stops if a function is missing from its `layers` table or if the
  layout is not clean, and writes `inst/extdata/EEAaq_function_map.*`,
  `man/figures/function-map.svg` and `vignettes/function-map.svg`.
- Logo: `Rscript dev/make-logo.R [A-C]`; README figures:
  `Rscript dev/readme-figures.R` (both offline, from `Support_files/`).
- History: the layout before November 2026 is the tag
  `legacy-layout-2026-11-20`; `archive/README.md` maps what is where.
