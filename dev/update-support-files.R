## ---------------------------------------------------------------------------
## Refreshes the metadata in Support_files/ that every installed copy of
## EEAaq downloads at run time from
##
##     https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/<name>.rds
##
## (EEAaq_get_dataframe() for LAU, NUTS, stations and pollutant;
## get_stations() for missing_cities). The files become live for all the
## users as soon as they are pushed to main, so:
##
##   1. run this script from the repository root on a branch,
##          Rscript dev/update-support-files.R pollutant
##          Rscript dev/update-support-files.R NUTS --year=2024
##          Rscript dev/update-support-files.R LAU --year=2024
##          Rscript dev/update-support-files.R stations
##      (stations is built from the LAU and NUTS files currently on GitHub,
##      so refresh and push those first);
##   2. check the summary it prints: a file whose columns differ from the
##      current one is NOT overwritten (pass --force once you have checked
##      that every version of the package in use still reads it);
##   3. commit, push, merge into main.
##
## This replaces upload_git(), which pushed the files through the GitHub API
## with a token written in the source: the files are now ordinary files of
## this repository and go through git.
##
## missing_cities.rds is maintained by hand (see Support_files/README.md).
##
## Known issue (November 2026): get_NUTS() and get_LAU() scrape the GISCO
## download pages, and get_NUTS() no longer finds the link to the shapefile
## for any edition (the page has changed). Fix the scraping, or download from
## the GISCO API, before refreshing NUTS.rds.
## ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (!file.exists("DESCRIPTION")) stop("Run this script from the repository root.")
what <- setdiff(args, grep("^--", args, value = TRUE))
year <- sub("^--year=", "", grep("^--year=", args, value = TRUE))
force <- "--force" %in% args
valid <- c("LAU", "NUTS", "pollutant", "stations")
if (!length(what) || !all(what %in% valid)) {
  stop("Say which file to refresh: one or more of ", paste(valid, collapse = ", "))
}
if (any(what %in% c("LAU", "NUTS")) && length(year) != 1L) {
  stop("LAU and NUTS need --year=YYYY (the edition of the GISCO boundaries).")
}

## the development version of the package, with its internal builders
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
  library(EEAaq)
}
ns <- asNamespace("EEAaq")
builders <- list(
  LAU = function() ns$get_LAU(year = year),
  NUTS = function() ns$get_NUTS(year = year),
  pollutant = function() ns$get_pollutants(),
  stations = function() ns$get_stations()
)

## Safety: up to version 1.0.3, get_LAU() and get_stations() worked in a fixed
## folder (C:/Temp) and get_LAU() deleted it on exit, and get_NUTS() deleted
## the whole tempdir() of the session. Never run the builders of an older
## installed version.
if (utils::packageVersion("EEAaq", lib.loc = NULL) < "1.0.4") {
  stop("Load the development version (install pkgload): the builders of ",
       "EEAaq < 1.0.4 delete folders outside their own temporary files.")
}

for (f in what) {
  path <- file.path("Support_files", paste0(f, ".rds"))
  message("Building ", f, " ...")
  new <- builders[[f]]()
  old <- if (file.exists(path)) readRDS(path) else NULL
  cat(sprintf("\n%s: %d rows x %d columns (was %s)\n", f, nrow(new), ncol(new),
              if (is.null(old)) "absent" else sprintf("%d x %d", nrow(old), ncol(old))))
  if (!is.null(old) && !identical(names(old), names(new))) {
    cat("  columns removed:", paste(setdiff(names(old), names(new)), collapse = ", "), "\n")
    cat("  columns added:  ", paste(setdiff(names(new), names(old)), collapse = ", "), "\n")
    if (!force) {
      cat("  NOT written: the columns changed. Re-run with --force once checked.\n")
      next
    }
  }
  saveRDS(new, path, compress = "xz")
  cat("  written to", path, "\n")
}
