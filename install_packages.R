# ==============================================================================
# Script: install_packages.R
# Purpose: Installs all R and C++ dependencies for the CFB Bayesian Ranking Engine
#          Uses 'pak' with Pop!_OS -> Ubuntu 24.04 binary mapping.
# ==============================================================================

cat("====================================================================\n")
cat("  CFB Retrospective Bayesian Ranking Engine: Environment Setup      \n")
cat("====================================================================\n\n")

# 1. Map Pop!_OS to Ubuntu 24.04 (noble) for pak and Posit Package Manager
# Pop!_OS identifies as ID=pop in /etc/os-release. Without this explicit mapping,
# pak falls back to source compilation, failing on missing C/C++ dev headers.
Sys.setenv(
  PKG_SYSREQS_PLATFORM = "ubuntu-24.04",
  LIBARROW_BINARY = "true"
)

options(
  pkg.sysreqs_platform = "ubuntu-24.04",
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"
  )
)

# 2. Ensure 'pak' is installed
if (!requireNamespace("pak", quietly = TRUE)) {
  cat("[Setup] Installing 'pak' package manager...\n")
  install.packages("pak", repos = "https://cloud.r-project.org")
}

# 3. Define required packages (CRAN and GitHub references)
packages_to_install <- c(
  # Data store & API
  "dplyr", "tidyr", "tibble", "duckdb", "DBI", "httr2", "jsonlite", "qs2",

  # Bayesian modeling & checks
  "stan-dev/cmdstanr", "posterior", "loo", "priorsense",

  # Pipeline
  "targets", "tarchetypes",

  # Reporting
  "quarto", "ggplot2", "gt", "scales",

  # Tests
  "testthat"
)

# Helper function to extract base package name from reference (e.g. "traversc/qs" -> "qs")
pkg_name <- function(ref) {
  sub(".*/", "", ref)
}

expected_pkg_names <- vapply(packages_to_install, pkg_name, character(1), USE.NAMES = FALSE)
installed_pkgs <- installed.packages()[, "Package"]

# 4. Identify and install missing packages
missing_mask <- !expected_pkg_names %in% installed_pkgs
missing_refs <- packages_to_install[missing_mask]

if (length(missing_refs) > 0) {
  cat(sprintf("[pak] Installing %d missing package(s) using Ubuntu 24.04 binaries:\n%s\n\n", 
              length(missing_refs), paste(paste0("  - ", missing_refs), collapse = "\n")))
  pak::pkg_install(missing_refs, ask = FALSE)
} else {
  cat("[pak] All required packages are already installed.\n\n")
}

# 5. Check CmdStan C++ Toolchain & Backend
cat("\nChecking CmdStan C++ backend...\n")
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  tryCatch({
    cs_path <- cmdstanr::cmdstan_path()
    cs_ver <- cmdstanr::cmdstan_version()
    cat(sprintf("[CmdStan] Found CmdStan version %s at %s\n", cs_ver, cs_path))
  }, error = function(e) {
    cat("[CmdStan] CmdStan C++ backend is not yet installed.\n")
    cat("To install CmdStan, run in R:\n")
    cat("  cmdstanr::check_cmdstan_toolchain()\n")
    cat("  cmdstanr::install_cmdstan(cores = parallel::detectCores())\n")
  })
} else {
  cat("[CmdStan] cmdstanr package is not yet available.\n")
}

# 6. Summary Verification
cat("\n====================================================================\n")
cat("                     Dependency Check Summary                       \n")
cat("====================================================================\n")

current_installed <- installed.packages()[, "Package"]
installed_check <- expected_pkg_names %in% current_installed

status_df <- data.frame(
  Package = expected_pkg_names,
  Reference = packages_to_install,
  Installed = ifelse(installed_check, "YES", "NO"),
  Version = sapply(expected_pkg_names, function(p) {
    if (p %in% current_installed) as.character(packageVersion(p)) else "N/A"
  }),
  stringsAsFactors = FALSE
)

print(status_df, row.names = FALSE)

cat("\nEnvironment setup script completed.\n")
