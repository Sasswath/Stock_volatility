# ============================================================
# 00_utils.R — Shared helper functions for the project
# ============================================================
# This file is sourced at the start of every analysis script
# via the setup script (01_setup.R) OR directly from .Rprofile.
#
# It contains reusable, project-wide helper functions so that
# every script stays DRY (Don't Repeat Yourself) and outputs
# are consistent across the whole pipeline.

# ---- directory helpers ---------------------------------------------------

# Project root directory. Set in .Rprofile; fallback to the
# directory of the currently executing script.
.project_root <- function() {
  if (nzchar(Sys.getenv("STOCK_VOL_PROJECT_ROOT"))) {
    return(Sys.getenv("STOCK_VOL_PROJECT_ROOT"))
  }
  # Fall back to the directory of the sourcing script
  normalizePath(sub("[/\\\\]?$", "", dirname(sys.frame(1)$ofile)), winslash = "/")
}

# Helper: resolve a path relative to the project root and ensure
# the directory exists.
project_path <- function(...) {
  dirs <- c(.project_root(), list(...))
  out <- file.path(dirs)
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  out
}

# ---- figure / table / simulation output paths ----------------------------
fig_dir  <- function(name) project_path("output", "figures", name)
tab_dir  <- function(name) project_path("output", "tables", name)
sim_dir  <- function(name) project_path("output", "simulations", name)

# ---- reproducibility -----------------------------------------------------

# Reusable seed setter used by every simulation script.
set_project_seed <- function(seed = 12345) {
  set.seed(seed)
  invisible(TRUE)
}

# ---- common vector utilities ---------------------------------------------

# Remove only leading NA produced by lag/shift operations. Keeps
# genuine missing values so they are surfaced, not silently eaten.
drop_leading_na <- function(x) {
  x <- x[!is.na(x)]
  x
}

# Fill missing values with the previous observation (forward-fill).
# Use ONLY after deliberate decision, not silently.
ffill <- function(x) {
  stats::na.omit(stats::na.locf(x, na.rm = FALSE))
}

# ---- formatted console header ---------------------------------------------

section <- function(title, char = "=") {
  cat("\n")
  cat(paste0(rep(char, 70), collapse = ""), "\n")
  cat(sprintf("  %s", title))
  cat(sprintf("%s\n", rep(char, 70 - nchar(title) - 2)))
  cat(paste0(rep(char, 70), collapse = ""), "\n")
}

# ---- colour helpers for console -------------------------------------------

# Safe colour printer — returns plain text if colour support is absent.
cat_col <- function(text, colour = NULL) {
  if (isTRUE(getOption("stock_vol_use_colours", TRUE)) && !is.null(colour)) {
    codes <- c(
      "red"   = "\033[31m",
      "green" = "\033[32m",
      "yellow"= "\033[33m",
      "blue"  = "\033[34m",
      "magenta" = "\033[35m",
      "cyan"  = "\033[36m",
      "white" = "\033[37m"
    )
    reset <- "\033[0m"
    if (colour %in% names(codes)) {
      return(paste0(codes[colour], text, reset))
    }
  }
  text
}

# ---- table printing ---------------------------------------------------------

# Print a data frame as a clean markdown-style table to the console.
print_table <- function(df, caption = NULL, sep = " | ") {
  if (!is.null(caption)) cat(sprintf("\n-- %s --\n", caption))
  # Auto-detect numeric vs character columns
  fmt <- ifelse(widerThan10 <- sapply(df, function(x) max(nchar(as.character(x)), na.rm = TRUE)) > 10,
                function(x) formatC(x, format = "f", digits = 4),
                function(x) format(x, justify = "right"))
  df_print <- as.data.frame(lapply(seq_along(df), function(i) {
    if (is.numeric(df[[i]])) formatC(df[[i]], format = "f", digits = 4)
    else if (is.factor(df[[i]])) as.character(df[[i]])
    else as.character(df[[i]])
  }))
  print(df_print, row.names = FALSE)
  cat("\n")
}

# ---- safe percentile helper ------------------------------------------------

# Safe percentile helper (defined but not used by current scripts)
pct <- function(x, probs = c(0.01, 0.05, 0.95, 0.99)) {
  stats::quantile(drop_leading_na(x), probs = probs, na.rm = TRUE, names = TRUE)
}

# ---- correlation matrix with significance -----------------------------------

cor_test_matrix <- function(df) {
  # Pearson correlation matrix with pairwise complete observations
  mat <- stats::cor(df, use = "pairwise.complete.obs")
  # Add p-value matrix
  n <- nrow(df)
  pmat <- matrix(NA, nrow = ncol(df), ncol = ncol(df),
                 dimnames = dimnames(mat))
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i + 1):ncol(df)) {
      complete <- df[stats::complete.cases(df[, i], df[, j]), ]
      if (nrow(complete) > 3) {
        ct <- cor.test(complete[, i], complete[, j], method = "pearson")
        pmat[i, j] <- pmat[j, i] <- ct$p.value
      }
    }
  }
  list(corr = mat, p = pmat)
}

# ---- volatility helpers -----------------------------------------------------

# Annualized volatility from daily standard deviation.
annual_vol <- function(sd_daily, trading_days = 252) {
  sd_daily * sqrt(trading_days)
}

# ---- save helper with auto-extension ---------------------------------------
safe_save <- function(obj, path, verbose = TRUE) {
  if (inherits(obj, "ggplot")) {
    ggsave(path, obj, width = 10, height = 6, units = "in", dpi = 300)
  } else if (inherits(obj, "data.frame") || inherits(obj, "tbl_df")) {
    utils::write.csv(obj, path, row.names = FALSE)
  } else {
    saveRDS(obj, path)
  }
  if (verbose) cat(sprintf("  Saved: %s\n", path))
}

.onAttach <- function(lib, pkg) {
  invisible(TRUE)
}
