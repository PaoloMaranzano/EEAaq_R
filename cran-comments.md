## Before submitting (delete this section)

- [ ] The vignette still needs the internet: `EEAaq_summary()`,
      `EEAaq_map_stations()` and `EEAaq_idw_map()` download the metadata
      from GitHub (`Support_files/`) and stop when `curl::has_internet()` is
      FALSE. On a CRAN machine without network the vignette fails as in 1.0.3.
      Either ship what the vignette needs, evaluate those chunks only when the
      resources are reachable, or precompute the vignette.
- [ ] `EEAaq_get_stations()` has a `\donttest{}` example that downloads from
      GitHub; CRAN runs `\donttest{}` examples in its incoming checks.
- [ ] Run `Rscript dev/check-package.R`, R-hub (`.github/workflows/rhub.yaml`)
      and win-builder (R-devel), then fill in the results below.

## Resubmission of an archived package

EEAaq was archived on 2026-08-26. The only problem in the CRAN checks of
1.0.3 was an ERROR on r-patched-linux-x86_64 while re-building the vignette:
the vignette downloaded data from the EEA Air Quality Download Service, which
did not answer the check machine ("No valid parquet file found in the
answer").

In this version:

* the vignette reads a small example dataset shipped with the package
  (`inst/extdata/vignette_data.rds`) instead of downloading it;
* the examples that need the EEA service are in `\dontrun{}`, because they
  require a working internet connection and an external service.

## Test environments

* local: Windows 11, R 4.6.1, `R CMD check --as-cran` (2026-11-20, with
  network access): 0 errors | 0 warnings | 0 notes
* R-hub: linux, windows, macos (to run)
* win-builder: R-devel (to run)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / package was archived on CRAN (expected on the incoming
  checks).
