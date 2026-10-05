# ============================================================
# 01_setup.R — R environment setup
# ============================================================
#
# PURPOSE
#   Establish the project workspace, install/load all required
#   packages, set the random seed, and configure relative paths
#   so the project runs from a clean R session anywhere.
#
# HOW TO RUN
#   1. In RStudio: Session → Set Working Directory → To Project
#      Directory (or simply open stock-volatility-project.Rproj).
#   2. Source this file:
#        source("R/01_setup.R")
#   3. All subsequent scripts source 00_utils.R automatically via
#      the .Rprofile.

# ---- project root discovery ------------------------------------------------
# .Rprofile already sets .project_root(), but for a clean session we also
# detect it here explicitly.

.root <- normalizePath(sub("[/\\\\]?$", "", dirname(sys.frame(1)$ofile)), winslash = "/")
if (!dir.exists(file.path(.root, "data"))) {
  stop("Could not find the project root. Please open stock-volatility-project.Rproj and work from the project directory.")
}
Sys.setenv(STOCK_VOL_PROJECT_ROOT = .root)

# ---- library loading --------------------------------------------------------
# Required packages. Install any that are missing automatically (with a user
# prompt when run non-interactively). Only the packages that are actually
# needed are loaded here; unused packages are not loaded.

required_packages <- c(
  # Data acquisition
  "quantmod",
  # Core data manipulation + viz
  "tidyverse",        # dplyr, tidyr, ggplot2, lubridate, etc.
  "readr",
  # Statistical distributions
  "moments",
  # Distribution fitting
  "fitdistrplus",
  # Model comparison / time-series / risk
  "MASS",
  "tseries",
  "PerformanceAnalytics",
  # Convenience
  "scales",
  "patchwork"
)

cat_col("\nInstalling / checking required R packages...\n", "blue")

# Helper to install/load
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat_col(sprintf("  [MISSING] %s — attempting installation...\n", pkg), "yellow")
    if (interactive()) {
      install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
    } else {
      install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE,
                       quiet = FALSE, Ncpus = parallel::detectCores())
    }
  }
  suppressPackageStartupMessages({
    library(pkg, character.only = TRUE)
  })
}

cat_col("  All required packages loaded.\n", "green")
cat_col("\n", "green")

# ---- random seed --------------------------------------------------------------
set_project_seed(12345)  # for reproducible simulations
cat_col("  Random seed set to 12345 for reproducible simulations.\n", "green")

# ---- global analysis options ---------------------------------------------------
options(
  warn = 1,              # turn warnings into errors in scripts that respect it
  scipen = 999,          # avoid scientific notation in output tables
  digits = 5             # reasonable precision
)

cat_col(
  "\nProject environment ready.\n",
  "green"
)

# ---- verify required data files are present ----------------------------------
# (Soft check; the actual collection runs later.)
cat_col("\nProject skeleton verified. Data collection will populate data/raw/.\n", "white")
cat_col(sprintf("  Root: %s\n", .root), "white")
cat_col(sprintf("  Packages loaded: %d\n", length(required_packages)), "white")

# ---- confirm .representative_ticker is set (set by 06 later) -------------------
# If a variable named .representative_ticker exists in the environment, we use it.
if (exists(".representative_ticker", envir = .GlobalEnv, inherits = FALSE)) {
  cat_col(sprintf("  Representative ticker detected: %s\n", .representative_ticker), "cyan")
} else {
  cat_col("  Representative ticker not set yet (script 06 will set it).\n", "white")
}
