## ---------------------------------------------------------------------------
## Precomputes the vignette: evaluates vignettes/vignettes.Rmd.orig and writes
## vignettes/vignettes.Rmd, in which every result is already printed and every
## figure is a PNG file in vignettes/figures/. CRAN builds vignettes.Rmd
## without running code that needs the internet (EEAaq 1.0.3 was archived
## because its vignette downloaded data while CRAN was building it).
##
##     Rscript dev/precompute-vignette.R
##
## Run it from the repository root whenever the vignette source, the
## functions it shows or the metadata in Support_files change, then commit
## vignettes.Rmd.orig, vignettes.Rmd and vignettes/figures/ together.
## Internet access is needed (the functions download the metadata from
## GitHub). The current sources are installed into a temporary library first,
## so the vignette runs the package exactly as a user would.
## ---------------------------------------------------------------------------

if (!file.exists("DESCRIPTION")) stop("Run this script from the repository root.")
repo <- normalizePath(".", winslash = "/")
invisible(Sys.setlocale("LC_TIME", "C"))            # English dates

lib <- file.path(tempdir(), "EEAaq-lib")
dir.create(lib, showWarnings = FALSE)
status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", paste0("--library=", shQuote(lib)),
                    shQuote(repo)))
if (status != 0) stop("R CMD INSTALL failed")
.libPaths(c(lib, .libPaths()))
stopifnot(utils::packageVersion("EEAaq", lib.loc = lib) ==
            read.dcf(file.path(repo, "DESCRIPTION"), "Version")[1, 1])

setwd(file.path(repo, "vignettes"))
unlink("figures", recursive = TRUE)
knitr::knit("vignettes.Rmd.orig", output = "vignettes.Rmd", envir = new.env(), quiet = TRUE)

## the precomputed file must not contain anything that knitr would evaluate
chunks <- grep("^```\\{r", readLines("vignettes.Rmd"), value = TRUE)
if (length(chunks)) stop("vignettes.Rmd still contains R chunks: ", paste(chunks, collapse = "; "))
message("vignettes.Rmd written; figures: ",
        paste(list.files("figures"), collapse = ", "))
