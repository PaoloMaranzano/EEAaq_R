## ===========================================================================
## EEAaq: the maintainer's workflow, step by step
##
## Open EEAaq.Rproj in RStudio (the working directory becomes the root of the
## repository) and run this script ONE SECTION AT A TIME: Ctrl+Enter runs the
## current line or selection, Ctrl+Alt+T the section under the cursor, and
## Ctrl+Shift+O shows the list of the sections. Do not source() it: the first
## line stops it, because some steps publish (push, CRAN).
##
##   0. Setup                    packages, folders and helpers, once per session
##   1. Change a function        code, documentation, version, NEWS, map
##   2. Debug                    load_all(), debugonce(), browser(), recover
##   3. Check in RStudio         R CMD check --as-cran, as CRAN does
##   4. Commit and push          directly on main
##   5. The .tar.gz for CRAN     EEAaq_<version>.tar.gz in the GitHub folder
##   6. Submit to CRAN           R-hub, win-builder, upload by hand, tag
##
## A new version, typically: 0, 1 (and 2 if something does not work), 3 until
## the check is clean, 4, then 5 and 6 when the version goes to CRAN.
##
## Rules of the repository (see CLAUDE.md): man/ and NAMESPACE are generated
## by roxygen2; the extended vignette is edited in vignettes.Rmd.orig and
## precomputed; nothing that runs during the checks may use the internet;
## Support_files/ is read by every installed copy of the package.
## ===========================================================================
stop("Run this script one section at a time (Ctrl+Alt+T), do not source() it.")


## ---- 0. Setup ---------------------------------------------------------------
## The working directory must be the root of the repository.
stopifnot("Open EEAaq.Rproj first" = file.exists("DESCRIPTION") &&
            read.dcf("DESCRIPTION", "Package")[1, 1] == "EEAaq")
repo <- normalizePath(".", winslash = "/")

## Packages for the maintenance (installed only if missing)
needed <- c("roxygen2", "pkgload", "pkgbuild", "rcmdcheck", "knitr", "rmarkdown", "rhub", "gitcreds")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

## pandoc builds the vignettes: RStudio provides it; outside RStudio, use the
## copy that comes with RStudio.
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
  tools_dirs <- c("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
                  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/x86_64",
                  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")
  tools_dirs <- tools_dirs[file.exists(file.path(tools_dirs, "pandoc")) |
                             file.exists(file.path(tools_dirs, "pandoc.exe"))]
  if (length(tools_dirs)) Sys.setenv(RSTUDIO_PANDOC = tools_dirs[1])
}
rmarkdown::pandoc_available()

## Where the tarballs are kept: the folder that contains the repository, as
## for the other packages (the GitHub folder).
release_dir <- dirname(repo)

## A shortcut to run git from R; the RStudio Git pane (top right) does the same
## with buttons.
git <- function(...) system2("git", c(...))

## build_tarball() builds EEAaq_<version>.tar.gz, as R CMD build does, and
## returns its path. It works on a copy of the sources in a new folder of the
## temporary directory of this session, outside OneDrive, whose
## synchronisation locks the files the build writes; R deletes that folder
## when the session ends. The copy leaves out what .Rbuildignore excludes
## anyway (Support_files/ alone is 30 MB). The vignettes are rebuilt from
## their precomputed results, without internet. Used by sections 3 and 5.
build_tarball <- function() {
  src <- file.path(tempfile("EEAaq-build-"), "EEAaq")
  dir.create(src, recursive = TRUE)
  skip <- c(".git", ".Rproj.user", "dev", "archive", "Support_files", "doc", "Meta")
  file.copy(file.path(repo, setdiff(list.files(repo, all.files = TRUE, no.. = TRUE), skip)),
            src, recursive = TRUE)
  unlink(file.path(src, "inst", "doc"), recursive = TRUE)
  pkgbuild::build(src, dest_path = dirname(src), manual = FALSE)
}

## Current version and state of the repository
read.dcf("DESCRIPTION", "Version")[1, 1]
git("status", "--short", "--branch")


## ---- 1. Change a function -------------------------------------------------------
## 1.1 Start from the latest main on GitHub (someone else may have pushed).
git("pull", "--ff-only", "origin", "main")

## 1.2 Open the function and edit it: the code and the roxygen comments (#')
##     above it, which become its help page. One file per exported function.
file.edit("R/EEAaq_time_aggregate.R")

## 1.3 Regenerate the help pages (man/*.Rd) and the NAMESPACE from the roxygen
##     comments. Never edit man/ or NAMESPACE by hand.
roxygen2::roxygenise()

## 1.4 New version: change Version in DESCRIPTION (x.y.z: z for a bug fix, y
##     for a new feature) and add an entry at the top of NEWS.md.
file.edit("DESCRIPTION")
file.edit("NEWS.md")

## 1.5 If functions were added, removed or renamed, or the calls between them
##     changed, regenerate the map of the package (README and vignette).
source("dev/make-function-map.R")

## 1.6 If the change affects the extended vignette: edit
##     vignettes/vignettes.Rmd.orig (never vignettes.Rmd), then precompute it
##     (it downloads data, so it needs the internet; a few minutes).
file.edit("vignettes/vignettes.Rmd.orig")
system2(file.path(R.home("bin"), "Rscript"), "dev/precompute-vignette.R")

## 1.7 What changed
git("status", "--short")
git("diff", "--stat")


## ---- 2. Debug ---------------------------------------------------------------------
## 2.1 Load the package from the sources, without installing it: all the
##     functions, internal ones included, become available in the session.
##     Run it again after every change to R/ (it takes a few seconds).
pkgload::load_all(".")

## 2.2 Data to test with, without internet: the example of the vignette
##     (NO2 and PM10 in the province of Milano, 1-15 January 2024).
data <- readRDS("inst/extdata/vignette_data.rds")
class(data)
head(data)

## 2.3 Enter a function at its next call and run it line by line. At the
##     Browse> prompt: n = next line, s = step into the call, c = continue,
##     f = finish the loop, Q = quit; any R command (ls(), str(x), ...) works.
debugonce(EEAaq_time_aggregate)
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly", aggr_fun = "mean")

## 2.4 Several functions at once: debug() stays active until undebug().
debug(EEAaq_summary); debug(EEAaq_get_stations)
summ <- EEAaq_summary(data = data)          # needs the internet (station metadata)
undebug(EEAaq_summary); undebug(EEAaq_get_stations)

## 2.5 Stop at a precise line: write browser() in the code, run load_all(),
##     call the function; remove browser() before committing.

## 2.6 After an error: where did it happen?
traceback()
options(error = recover)   # on the next error, choose the call to inspect
options(error = NULL)      # back to normal

## 2.7 Internal functions are available after load_all(); with the installed
##     package they are reached with EEAaq:::
handle_dates(from = "2024-01-01", to = "2024-01-15")

## 2.8 Functions that use the internet: keep the requests small
stations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
one <- EEAaq_get_data(IDstations = "IT0477A", pollutants = "PM10",     # Milano, viale Marche
                      from = "2024-01-01", to = "2024-01-07")

## 2.9 Install the development version in your library, to use it in other
##     projects (the Install button of the RStudio Build pane does the same).
install.packages(repo, repos = NULL, type = "source")


## ---- 3. Check in RStudio ------------------------------------------------------------
## R CMD check --as-cran, as CRAN runs it, on the tarball built from the
## current files. Like the build, the check runs outside OneDrive. (The Check
## button of the RStudio Build pane checks in place, in OneDrive: do not use
## it.)
offline <- FALSE   # TRUE: every internet request fails, as on a CRAN machine
                   # without network (expected: 1 NOTE "unable to verify
                   # current time", which CRAN does not show)
manual <- nzchar(Sys.which("pdflatex"))   # check the PDF manual too (needs LaTeX)

## 3.1 Build the tarball (a minute or two).
tarball <- build_tarball()

## 3.2 Check it (two or three minutes).
env <- c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false", `_R_CHECK_FORCE_SUGGESTS_` = "false")
if (offline) {
  env <- c(env, http_proxy = "http://127.0.0.1:9", https_proxy = "http://127.0.0.1:9",
           HTTP_PROXY = "http://127.0.0.1:9", HTTPS_PROXY = "http://127.0.0.1:9")
}
res <- rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", if (!manual) "--no-manual"),
                            check_dir = dirname(tarball), error_on = "never", env = env)

## 3.3 The result: the target is 0 errors | 0 warnings | 0 notes. Every
##     remaining NOTE must be explained in cran-comments.md.
res
res$errors; res$warnings; res$notes
file.edit(file.path(dirname(tarball), "EEAaq.Rcheck", "00check.log"))   # the full log


## ---- 4. Commit and push -------------------------------------------------------------
## The work happens directly on main. Commit only what belongs to the change;
## the RStudio Git pane (Commit, then Push) does the same as these lines.
git("status", "--short")
git("add", "-A")                                    # look at the list above first
git("commit", "-m", shQuote("Describe the change in one line"))
git("push", "origin", "main")


## ---- 5. The .tar.gz for CRAN -----------------------------------------------------
## The file uploaded by hand on the CRAN page (section 6) is the "source
## tarball" of the package, EEAaq_<version>.tar.gz. These lines build it from
## the current files of the repository and save it in the GitHub folder, next
## to EEAaq_R. Before: section 0 in this session, the check of section 3 clean
## on these same files, and the new Version in DESCRIPTION (CRAN refuses a
## version number it already has).
tarball <- build_tarball()
tarball_for_cran <- file.path(release_dir, basename(tarball))
file.copy(tarball, tarball_for_cran, overwrite = TRUE)
tarball_for_cran          # the file to upload
browseURL(release_dir)    # opens the folder that contains it


## ---- 6. Submit to CRAN -----------------------------------------------------------
## 6.1 R-hub: the same check on Linux, Windows and macOS, on GitHub. It checks
##     the copy on GitHub, so push first (section 4). Results in one to two
##     hours in the Actions tab of the repository.
source("dev/run-rhub-checks.R")

## 6.2 win-builder: the CRAN machine for Windows, with R-devel. On the page,
##     upload the file of section 5 in the R-devel field; the result comes by
##     e-mail to the maintainer in DESCRIPTION, usually within an hour.
browseURL("https://win-builder.r-project.org/upload.aspx")

## 6.3 Write the results in cran-comments.md (and delete its "Before
##     submitting" section); its text goes in the comment box of the form.
file.edit("cran-comments.md")

## 6.4 Submit by hand: on the CRAN form fill in your name and the e-mail of the
##     maintainer in DESCRIPTION, choose the file of section 5, paste the text
##     of cran-comments.md in the comments, upload, check the summary and
##     submit. CRAN then sends an e-mail to the maintainer with a link that
##     confirms the submission: until it is clicked, nothing reaches CRAN.
browseURL("https://cran.r-project.org/submit.html")

## 6.5 When CRAN publishes the package: tag the version and push the tag.
version <- read.dcf("DESCRIPTION", "Version")[1, 1]
git("tag", "-a", paste0("v", version), "-m", shQuote(paste("EEAaq", version, "on CRAN")))
git("push", "origin", paste0("v", version))
