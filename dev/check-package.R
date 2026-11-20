## ---------------------------------------------------------------------------
## Build and check EEAaq the way CRAN does, from a folder that is NOT
## synchronized by OneDrive/Google Drive (the sync clients lock the many small
## files R CMD check creates and the check stalls).
##
##     Rscript dev/check-package.R              # build + check --as-cran
##     Rscript dev/check-package.R --no-check   # build only
##
## The tarball and the .Rcheck folder are written to a temporary directory
## whose path is printed at the end; the tarball is also copied next to the
## repository. pandoc is taken from RStudio when it is not on the PATH.
## Requires pkgbuild and rcmdcheck.
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo, "DESCRIPTION"))) {
  stop("Run this script from the root of the EEAaq_R repository.", call. = FALSE)
}
if (!nzchar(Sys.which("pandoc")) && !nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools"
  )
  found <- candidates[file.exists(file.path(candidates, "pandoc")) |
                        file.exists(file.path(candidates, "pandoc.exe"))]
  if (length(found)) Sys.setenv(RSTUDIO_PANDOC = found[1L])
}
work <- file.path(tempdir(), "EEAaq-check")
unlink(work, recursive = TRUE)
dir.create(work, showWarnings = FALSE, recursive = TRUE)
message("working directory: ", work)

## Build from a plain copy of the sources without the folders that
## .Rbuildignore excludes anyway (Support_files alone is 30 MB).
source_copy <- file.path(work, "EEAaq")
dir.create(source_copy)
ignore <- c(".git", ".Rproj.user", "dev", "archive", "Support_files",
            "inst/doc", "doc", "Meta")
top <- setdiff(list.files(repo, all.files = TRUE, no.. = TRUE), ignore)
file.copy(file.path(repo, top), source_copy, recursive = TRUE)
unlink(file.path(source_copy, "inst", "doc"), recursive = TRUE)

tar <- pkgbuild::build(source_copy, dest_path = work, quiet = FALSE)
message("built: ", tar)
kept <- file.path(dirname(repo), basename(tar))
if ("--no-check" %in% args) {
  file.copy(tar, kept, overwrite = TRUE)
  cat("tarball kept at:", kept, "\n")
  quit(save = "no")
}

res <- rcmdcheck::rcmdcheck(
  tar,
  args = c("--as-cran", "--no-manual"),
  check_dir = work,
  error_on = "never",
  env = c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
          `_R_CHECK_FORCE_SUGGESTS_` = "false")
)
print(res)
cat("\nERRORS:", length(res$errors), " WARNINGS:", length(res$warnings),
    " NOTES:", length(res$notes), "\n")
cat("check directory:", work, "\n")

## Keep the tarball next to the repository (the temporary directory is
## deleted when this R session ends), like the other packages in this folder.
file.copy(tar, kept, overwrite = TRUE)
cat("tarball kept at:", kept, "\n")
