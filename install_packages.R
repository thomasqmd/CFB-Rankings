# ==============================================================================
# Script: install_packages.R
# Purpose: Installs all R and C++ dependencies for the CFB Bayesian Ranking Engine
# ==============================================================================

cat("====================================================================\n")
cat("  CFB Retrospective Bayesian Ranking Engine: Environment Setup      \n")
cat("====================================================================\n\n")

# Determine available CPU cores for parallel installation
num_cores <- max(1L, parallel::detectCores(), na.rm = TRUE)

# Set CRAN mirror and enable parallel installation
options(
  repos = c(
    stan = "https://stan-dev.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  ),
  Ncpus = num_cores
)

# Speed up C/C++ compilation and use prebuilt Arrow binaries:
# - LIBARROW_BINARY="true": Pop!_OS is not on Arrow's default allowlist, causing it
#   to compile from source by default. Setting this downloads official precompiled C++ binaries in seconds.
# - CMAKE_BUILD_PARALLEL_LEVEL & MAKEFLAGS: Build C/C++ packages across all cores if compiled from source.
Sys.setenv(
  LIBARROW_BINARY = "true",
  CMAKE_BUILD_PARALLEL_LEVEL = num_cores,
  MAKEFLAGS = sprintf("-j%d", num_cores)
)



# 1. List of required CRAN packages
cran_packages <- c(
  # Core data manipulation & analytical database
  "tidyverse",
  "duckdb",
  "DBI",
  "arrow",
  "qs",
  
  # College Football Data (SportsDataverse)
  "cfbfastR",
  
  # Bayesian modeling, inference & MCMC diagnostics
  "brms",
  "posterior",
  "bayesplot",
  "tidybayes",
  
  # Pipeline orchestration & parallel execution
  "targets",
  "tarchetypes",
  "crew",
  "future",
  "furrr",
  
  # Presentation, visualization & reporting
  "quarto",
  "ggplot2",
  "gt",
  "gtExtras",
  "patchwork",
  "plotly",
  "scales",
  
  # Interactive Dashboard
  "shiny",
  "bslib",
  "bsicons",
  
  # Package management utilities
  "remotes"
)

# 2. Identify missing CRAN packages
missing_pkgs <- cran_packages[!cran_packages %in% installed.packages()[, "Package"]]

if (length(missing_pkgs) > 0) {
  cat(sprintf("[CRAN] Installing %d missing package(s) using %d parallel worker(s):\n%s\n\n", 
              length(missing_pkgs), num_cores, paste(paste0("  - ", missing_pkgs), collapse = "\n")))
  install.packages(missing_pkgs, dependencies = TRUE, Ncpus = num_cores)
} else {
  cat("[CRAN] All base packages are already installed.\n\n")
}

# 3. Install cmdstanr if not present
if (!"cmdstanr" %in% installed.packages()[, "Package"]) {
  cat("[cmdstanr] Installing cmdstanr from Stan R-Universe...\n")
  install.packages(
    "cmdstanr",
    repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org"),
    Ncpus = num_cores
  )
} else {
  cat("[cmdstanr] cmdstanr package is already installed.\n")
}

# 4. Check CmdStan C++ Toolchain
cat("\nChecking CmdStan C++ backend...\n")
has_cmdstan <- FALSE
if ("cmdstanr" %in% installed.packages()[, "Package"]) {
  library(cmdstanr)
  tryCatch({
    cs_path <- cmdstan_path()
    cs_ver <- cmdstan_version()
    cat(sprintf("[CmdStan] Found CmdStan version %s at %s\n", cs_ver, cs_path))
    has_cmdstan <- TRUE
  }, error = function(e) {
    cat("[CmdStan] CmdStan C++ backend is not yet installed.\n")
    cat("To install CmdStan, run in R:\n")
    cat("  cmdstanr::check_cmdstan_toolchain()\n")
    cat("  cmdstanr::install_cmdstan(cores = parallel::detectCores())\n")
  })
}

# 5. Summary Verification
cat("\n====================================================================\n")
cat("                     Dependency Check Summary                       \n")
cat("====================================================================\n")

all_req <- c(cran_packages, "cmdstanr")
installed_check <- all_req %in% installed.packages()[, "Package"]

status_df <- data.frame(
  Package = all_req,
  Installed = ifelse(installed_check, "YES", "NO"),
  Version = sapply(all_req, function(p) {
    if (p %in% installed.packages()[, "Package"]) as.character(packageVersion(p)) else "N/A"
  }),
  stringsAsFactors = FALSE
)

print(status_df, row.names = FALSE)

cat("\nEnvironment setup script completed.\n")
