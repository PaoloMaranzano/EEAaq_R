# EEAaq - notes for coding agents

R package (root of this repository = package root). Material that is not
part of the built package lives in `dev/` (maintenance scripts),
`archive/` (past releases and the original 2023 repository) and
`Support_files/` (run-time metadata); all three are in `.Rbuildignore`.

## Working rules

- **`Support_files/` is a public API.** Every installed version of EEAaq
  downloads `LAU.rds`, `NUTS.rds`, `stations.rds`, `pollutant.rds` and
  `missing_cities.rds` from `raw/refs/heads/main/Support_files/`. Never move
  or rename them, and never change their columns on `main` without checking
  that the released versions still work. Refresh them with
  `Rscript dev/update-support-files.R` on a branch.
- **No secrets in the repository.** A GitHub token was committed in
  `Support_files/upload_git.Rd` and `upload_on_GH.txt` until November 2026
  and is still in the history: treat it as compromised. Tokens belong in
  `~/.Renviron`.
- **CRAN and the network.** The package was archived in August 2026 because
  the vignette downloaded data while CRAN built it. Nothing that runs during
  `R CMD check` may use the internet: examples that need it go in
  `\dontrun{}`, and the vignette is precomputed. Edit
  `vignettes/vignettes.Rmd.orig`, never `vignettes.Rmd`, then run
  `Rscript dev/precompute-vignette.R` (needs network) and commit the source,
  the result and `vignettes/figures/` together.
- `R CMD check` must run from a folder that is not synchronized by
  OneDrive/Google Drive: `Rscript dev/check-package.R` copies the sources to
  the temporary directory, builds and checks with `--as-cran`; add
  `--offline` to reproduce a CRAN machine without network.
- Temporary files: always inside `tempfile()`/`tempdir()`, and remove only
  the folder you created (never `unlink(tempdir())`).
- `NAMESPACE` and `man/` are generated: edit the roxygen comments and run
  `roxygen2::roxygenise()`.
- Logo: `Rscript dev/make-logo.R [A-C]`; README figures:
  `Rscript dev/readme-figures.R` (both offline, from `Support_files/`).
- History: the layout before November 2026 is the tag
  `legacy-layout-2026-11-20`; `archive/README.md` maps what is where.
