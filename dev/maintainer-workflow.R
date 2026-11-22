## ===========================================================================
## EEAaq: the maintainer's workflow, step by step
##
## Open EEAaq.Rproj in RStudio (the working directory becomes the root of the
## repository) and run this script ONE SECTION AT A TIME: Ctrl+Enter runs the
## current line or selection, Ctrl+Alt+T the section under the cursor, and
## Ctrl+Shift+O shows the list of the sections. Do not source() it: the first
## line stops it, because some steps publish (push, CRAN).
##
##   0. Setup                    packages and folders, once per session
##   1. Change a function        code, documentation, version, NEWS, map
##   2. Debug                    load_all(), debugonce(), browser(), recover
##   3. Check in RStudio         R CMD check --as-cran, as CRAN does
##   4. Commit and push          directly on main
##   5. Release on CRAN          R-hub, tarball, win-builder, submission, tag
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
## R CMD check --as-cran, as CRAN runs it. The check must not run in the
## OneDrive folder, whose synchronisation locks the files the check writes:
## the sources are copied to a folder of the temporary directory of this
## session, which R deletes when the session ends. (The Check button of the
## RStudio Build pane checks in place, in OneDrive: do not use it.)
offline <- FALSE   # TRUE: every internet request fails, as on a CRAN machine
                   # without network (expected: 1 NOTE "unable to verify
                   # current time", which CRAN does not show)
manual <- nzchar(Sys.which("pdflatex"))   # check the PDF manual too (needs LaTeX)

check_dir <- file.path(tempdir(), "EEAaq-check")
unlink(check_dir, recursive = TRUE)        # only the folder created here
dir.create(check_dir)
src <- file.path(check_dir, "EEAaq")
dir.create(src)
skip <- c(".git", ".Rproj.user", "dev", "archive", "Support_files", "doc", "Meta")
file.copy(file.path(repo, setdiff(list.files(repo, all.files = TRUE, no.. = TRUE), skip)),
          src, recursive = TRUE)
unlink(file.path(src, "inst", "doc"), recursive = TRUE)

## 3.1 Build the tarball, as R CMD build does (the vignettes are rebuilt from
##     their precomputed results, without internet).
tarball <- pkgbuild::build(src, dest_path = check_dir, manual = FALSE)

## 3.2 Check it.
env <- c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false", `_R_CHECK_FORCE_SUGGESTS_` = "false")
if (offline) {
  env <- c(env, http_proxy = "http://127.0.0.1:9", https_proxy = "http://127.0.0.1:9",
           HTTP_PROXY = "http://127.0.0.1:9", HTTPS_PROXY = "http://127.0.0.1:9")
}
res <- rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", if (!manual) "--no-manual"),
                            check_dir = check_dir, error_on = "never", env = env)

## 3.3 The result: the target is 0 errors | 0 warnings | 0 notes. Every
##     remaining NOTE must be explained in cran-comments.md.
res
res$errors; res$warnings; res$notes
file.edit(file.path(check_dir, "EEAaq.Rcheck", "00check.log"))   # the full log


## ---- 4. Commit and push -------------------------------------------------------------
## The work happens directly on main. Commit only what belongs to the change;
## the RStudio Git pane (Commit, then Push) does the same as these lines.
git("status", "--short")
git("add", "-A")                                    # look at the list above first
git("commit", "-m", shQuote("Describe the change in one line"))
git("push", "origin", "main")


## ---- 5. Release on CRAN ----------------------------------------------------------
## 5.1 R-hub: the same check on Linux, Windows and macOS, on GitHub. It checks
##     the copy on GitHub, so push first (section 4). Results in about one
##     hour in the Actions tab of the repository.
source("dev/run-rhub-checks.R")

## 5.2 The tarball to upload: the one built in section 3, copied next to the
##     repository (rerun section 3 if the sources changed since).
file.copy(tarball, release_dir, overwrite = TRUE)
(tarball_for_cran <- file.path(release_dir, basename(tarball)))

## 5.3 win-builder: the CRAN machine for Windows, with R-devel. Upload the
##     tarball in the R-devel field of the page; the result comes by e-mail to
##     the maintainer in DESCRIPTION, usually within an hour.
browseURL("https://win-builder.r-project.org/upload.aspx")

## 5.4 Write the results in cran-comments.md (and delete its "Before
##     submitting" section); its text goes in the comment box of the form.
file.edit("cran-comments.md")

## 5.5 Submit: upload the tarball on the CRAN form. CRAN then sends an e-mail
##     to the maintainer with a link to confirm the submission.
browseURL("https://cran.r-project.org/submit.html")

## 5.6 When CRAN publishes the package: tag the version and push the tag.
version <- read.dcf("DESCRIPTION", "Version")[1, 1]
git("tag", "-a", paste0("v", version), "-m", shQuote(paste("EEAaq", version, "on CRAN")))
git("push", "origin", paste0("v", version))
