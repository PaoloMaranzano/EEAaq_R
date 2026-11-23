## Resubmission

This is a resubmission of 1.0.4, in reply to the comments of the CRAN team
(2026-11-23).

* *Does the package now pass standard checks even without internet access?*
  Yes. `R CMD check --as-cran` with every HTTP(S) request sent to an
  unreachable proxy, as on a machine without network, gives 0 errors |
  0 warnings | 1 note, "Unable to verify current time", which the check
  raises itself when it cannot reach its time server. Nothing that runs
  during the checks uses the internet: the vignettes run no code that
  downloads data (the extended one is precomputed), and the examples, which
  need the EEA Air Quality Download Service or the package metadata on
  GitHub, are in `\dontrun{}`.
* *Invalid URL https://dd.eionet.europa.eu/vocabulary/aq/pollutant.* The EEA
  server does not answer, from our network too. We removed the link and,
  for the same reason, the links to www.eea.europa.eu and
  discomap.eea.europa.eu, which at the moment time out or answer 502 Bad
  Gateway. The package now links only to the EEA Air Quality Download
  Service, from which it takes the data.
* *Invalid file URIs in README.md.* They pointed to parts of the GitHub
  repository that are not in the package (`Support_files/`, `dev/`,
  `archive/`); they are now full https://github.com/PaoloMaranzano/EEAaq_R
  URLs.

## Why the package was archived

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

## Test environments

* local: Windows 11, R 4.6.1, `R CMD check --as-cran` (2026-11-23): with
  network access, CRAN incoming checks of the URLs included, 0 errors |
  0 warnings | 1 note (new submission of an archived package); without
  network access 0 errors | 0 warnings | 1 note ("Unable to verify current
  time")
* R-hub, `R CMD check --as-cran` on GitHub Actions (2026-11-22; since then
  only links in DESCRIPTION, README and one help page have changed), with
  0 errors | 0 warnings | 0 notes on:
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
  0 notes on 2026-11-20 with an earlier revision of this release.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / package was archived on CRAN (expected on the incoming
  checks).
