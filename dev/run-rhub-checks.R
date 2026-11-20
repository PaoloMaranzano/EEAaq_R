## ---------------------------------------------------------------------------
## Launches the R-hub checks of EEAaq: R CMD check --as-cran on the GitHub
## Actions runners of R-hub, through the workflow .github/workflows/rhub.yaml.
##
##     Rscript dev/run-rhub-checks.R                     # CRAN set, branch main
##     Rscript dev/run-rhub-checks.R linux windows       # only these platforms
##     Rscript dev/run-rhub-checks.R --branch=my-branch  # another pushed branch
##     Rscript dev/run-rhub-checks.R --list              # available platforms
##     Rscript dev/run-rhub-checks.R --dry-run           # pre-flight checks only
##
## From RStudio, with the project open: source("dev/run-rhub-checks.R")
## launches the default set on main.
##
## What it needs
##   * the rhub package (>= 2.0.0): install.packages("rhub");
##   * a GitHub personal access token (PAT) with the "repo" and "workflow"
##     scopes, either in the git credential store (gitcreds::gitcreds_set())
##     or in the environment variable GITHUB_PAT, e.g. a line
##     GITHUB_PAT=ghp_... in ~/.Renviron. Never write the token in a file of
##     this repository.
##
## R-hub checks the copy of the branch that is on GitHub, not the local
## files: the script stops if the branch has commits that are not pushed, and
## warns about uncommitted changes. The results appear in the Actions tab of
## the repository (one job per platform, 10-30 minutes each); copy the
## outcome into cran-comments.md.
## ---------------------------------------------------------------------------

## Default platforms for a CRAN (re)submission:
##   linux, windows, macos, macos-arm64  GitHub's virtual machines
##   ubuntu-release                      the current R release
##   ubuntu-next                         R-patched: the flavour on which the
##                                       vignette of 1.0.3 failed (2026-08)
default_platforms <- c("linux", "windows", "macos", "macos-arm64",
                       "ubuntu-release", "ubuntu-next")

args <- commandArgs(trailingOnly = TRUE)
flags <- grep("^--", args, value = TRUE)
platforms <- setdiff(args, flags)
if (!length(platforms)) platforms <- default_platforms
branch <- sub("^--branch=", "", grep("^--branch=", flags, value = TRUE))
if (!length(branch)) branch <- "main"
dry_run <- "--dry-run" %in% flags

## ---- pre-flight checks ----------------------------------------------------------
if (!file.exists("DESCRIPTION") || !file.exists(".github/workflows/rhub.yaml")) {
  stop("Run this script from the root of the EEAaq_R repository.", call. = FALSE)
}
if (!requireNamespace("rhub", quietly = TRUE) ||
    utils::packageVersion("rhub") < "2.0.0") {
  stop("Install the rhub package (>= 2.0.0): install.packages(\"rhub\")", call. = FALSE)
}
if ("--list" %in% flags) {
  print(rhub::rhub_platforms())
  quit(save = "no")
}

git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(ok = is.null(status) || status == 0, out = out)
}
if (!git("--version")$ok) stop("git is not on the PATH.", call. = FALSE)

gh_url <- sub("\\.git$", "", git("remote", "get-url", "origin")$out[1])
message("repository: ", gh_url, "   branch: ", branch)

if (!git("fetch", "--quiet", "origin", branch)$ok) {
  stop("Branch '", branch, "' is not on GitHub: push it first.", call. = FALSE)
}
remote_sha <- git("rev-parse", paste0("origin/", branch))$out[1]
local <- git("rev-parse", "--verify", "--quiet", branch)
if (local$ok && local$out[1] != remote_sha) {
  ahead <- git("rev-list", "--count", paste0("origin/", branch, "..", branch))$out[1]
  if (ahead != "0") {
    stop("Local branch '", branch, "' has ", ahead, " commit(s) not on GitHub: ",
         "push them first, R-hub checks the GitHub copy.", call. = FALSE)
  }
  message("Note: GitHub has commits that the local branch does not; ",
          "R-hub checks the GitHub copy (", substr(remote_sha, 1, 7), ").")
}
dirty <- git("status", "--porcelain", "--untracked-files=no")$out
if (length(dirty)) {
  warning("Uncommitted local changes are NOT checked by R-hub:\n  ",
          paste(dirty, collapse = "\n  "), call. = FALSE, immediate. = TRUE)
}
if (!git("cat-file", "-e", paste0("origin/", branch, ":.github/workflows/rhub.yaml"))$ok) {
  stop("The workflow .github/workflows/rhub.yaml is not on origin/", branch, ".",
       call. = FALSE)
}
known <- rhub::rhub_platforms()$name
unknown <- setdiff(platforms, known)
if (length(unknown)) {
  stop("Unknown R-hub platform(s): ", paste(unknown, collapse = ", "),
       ". See Rscript dev/run-rhub-checks.R --list", call. = FALSE)
}
message("platforms: ", paste(platforms, collapse = ", "))
message("commit checked: ", substr(remote_sha, 1, 7), " (origin/", branch, ")")

if (dry_run) {
  message("Dry run: pre-flight checks passed, no check started.")
  quit(save = "no")
}

## ---- token ----------------------------------------------------------------------
## rhub reads the token through gitcreds, which looks first at the variable
## GITHUB_PAT_GITHUB_COM and then at the git credential store; a token kept in
## GITHUB_PAT is passed on through that variable, for this session only.
if (nzchar(Sys.getenv("GITHUB_PAT")) && !nzchar(Sys.getenv("GITHUB_PAT_GITHUB_COM"))) {
  Sys.setenv(GITHUB_PAT_GITHUB_COM = Sys.getenv("GITHUB_PAT"))
}

## ---- launch ---------------------------------------------------------------------
## rhub_doctor() verifies the token and that the workflow is set up on GitHub;
## rhub_check() starts one job per platform.
rhub::rhub_doctor(gh_url = gh_url)
rhub::rhub_check(gh_url = gh_url, platforms = platforms, branch = branch)

message("\nFollow the jobs at ", gh_url, "/actions",
        "\nWhen they finish, record the results in cran-comments.md ",
        "(section 'Test environments') and remove its 'Before submitting' list.")
