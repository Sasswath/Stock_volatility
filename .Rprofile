# stock-volatility-project/.Rprofile
# Project-specific R environment setup
# Set working directory to project root regardless of where R is launched
# This makes the project fully portable (no absolute paths)

# Store the project root so all scripts can find it
project_root <- getwd()

# Source shared utilities if they exist
utils_dir <- file.path(project_root, "R")
if (dir.exists(utils_dir) && file.exists(file.path(utils_dir, "00_utils.R"))) {
  source(file.path(utils_dir, "00_utils.R"))
}

# Set a clean, project-relative options
options(
  warn = 1,                    # turn warnings into errors for clean scripts
  scipen = 999,                # avoid scientific notation in output
  digits = 5                  # reasonable precision for reporting
)

cat("==================================================\n")
cat("  Stock Volatility Project — R environment ready\n")
cat("  Project root:", project_root, "\n")
cat("==================================================\n")
