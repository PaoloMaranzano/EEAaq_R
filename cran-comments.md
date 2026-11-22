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

* local: Windows 11, R 4.6.1, `R CMD check --as-cran`: with network access
  0 errors | 0 warnings | 0 notes (2026-11-22, PDF manual included);
  without network access 0 errors | 0 warnings | 1 note, "Unable to verify
  current time", raised by the check itself when it cannot reach its time
  server (2026-11-20)
* R-hub, `R CMD check --as-cran` on GitHub Actions (2026-11-22, this
  version), with 0 errors | 0 warnings | 0 notes on:
  * linux: Ubuntu 26.04.1, R-devel (2026-11-21 r90959)
  * windows: Windows Server 2022, R-devel (2026-11-17 r90926 ucrt)
  * macos: macOS Sequoia 15.7.9 (x86_64), R-devel (2026-11-21 r90959)
  * ubuntu-release: Ubuntu 26.04.1, R 4.6.2
  * ubuntu-next: Ubuntu 26.04.1, R 4.6.2 Patched (2026-11-20 r90959), the
    flavour on which the vignette of 1.0.3 failed

  On macos-arm64 (macOS Tahoe 26.7.2, R-devel 2026-11-21 r90959) the check
  could not start, twice: there are no CRAN binaries for R 4.7 on macOS
  arm64 yet, and the dependency arrow 26.0.1.1 did not build from source on
  the runner (Arrow C++). The same platform gave 0 errors | 0 warnings |
  0 notes on 2026-11-20 with the previous version, which differs only by
  the vignette "Map of the package" and a fix in `EEAaq_get_dataframe()`.
* win-builder: R-devel (to run)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / package was archived on CRAN (expected on the incoming
  checks).
