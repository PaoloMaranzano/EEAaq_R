## Before submitting (delete this section)

- [x] Run R-hub (`Rscript dev/run-rhub-checks.R`) and fill in the results
      below.
- [ ] Run win-builder (R-devel) and fill in the result below.

## Resubmission of an archived package

EEAaq was archived on 2026-08-26. The only problem in the CRAN checks of
1.0.3 was an ERROR on r-patched-linux-x86_64 while re-building the vignette:
the vignette downloaded data from the EEA Air Quality Download Service, which
did not answer the check machine ("No valid parquet file found in the
answer").

In this version no code that accesses the internet runs during the checks:

* the vignette is precomputed: it is evaluated by the maintainers, and the
  package ships the R Markdown file with the results and the figures, so
  re-building it runs no R code;
* all the examples need an external service (the EEA Air Quality Download
  Service or the package metadata on GitHub) and are in `\dontrun{}`;
* internal functions used by the maintainers to rebuild the metadata no
  longer write outside `tempdir()` nor delete it.

The package was checked with every HTTP(S) request sent to an unreachable
proxy, to reproduce a check machine without network access.

## Test environments

* local: Windows 11, R 4.6.1, `R CMD check --as-cran` (2026-11-20):
  with network access 0 errors | 0 warnings | 0 notes; without network
  access 0 errors | 0 warnings | 1 note, "Unable to verify current time",
  raised by the check itself when it cannot reach its time server
* R-hub, `R CMD check --as-cran` on GitHub Actions (2026-11-20), with
  0 errors | 0 warnings | 0 notes on every platform:
  * linux: Ubuntu 26.04.1, R-devel (2026-11-19 r90939)
  * windows: Windows Server 2022, R-devel (2026-11-17 r90926 ucrt)
  * macos: macOS Sequoia 15.7.9 (x86_64), R-devel (2026-11-20 r90940)
  * macos-arm64: macOS Tahoe 26.7.2 (aarch64), R-devel (2026-11-20 r90940)
  * ubuntu-release: Ubuntu 26.04.1, R 4.6.2
  * ubuntu-next: Ubuntu 26.04.1, R 4.6.2 Patched (2026-11-17 r90935), the
    flavour on which the vignette of 1.0.3 failed
* win-builder: R-devel (to run)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / package was archived on CRAN (expected on the incoming
  checks).
