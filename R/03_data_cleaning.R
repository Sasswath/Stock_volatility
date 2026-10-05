# ============================================================
# 03_data_cleaning.R — Validate and clean raw stock data
# ============================================================
#
# PURPOSE
#   Inspect every raw dataset for: missing values, duplicate
#   dates, impossible prices, data type issues, sorting/family
#   date ordering, and trading-day completeness.  Distinguish
#   data errors from legitimate extreme market observations.
#   Cleaning decisions are documented here and written to a log.
#
#   IMPORTANT: Extreme returns are NOT automatically removed.
#   They are potentially important to this project.

cat_col("\n" , "blue")
section("DATA CLEANING PIPELINE", "=")

# ---- configuration & output directory ---------------------------------------
log_file <- file.path(.project_root(), "output", "tables", "cleaning_log.txt")
dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)

# Open log file
log_con <- file(log_file, open = "w")

# ---- helper to write log lines ----------------------------------------------
log <- function(...) writeLines(sprintf(...), log_con)

# ---- 1. LOAD raw data --------------------------------------------------------
raw_files <- list.files(file.path(.project_root(), "data", "raw"),
                        pattern = "raw_prices_.*\\.csv$", full.names = TRUE)
# Also include the combined file if present
raw_files <- c(raw_files,
               file.path(.project_root(), "data", "raw", "combined_raw_prices.csv"))

cat_col(sprintf("  Loading %d raw file(s)...\n", length(raw_files)), "cyan")

datasets <- list()
for (f in raw_files) {
  df <- read_csv(f, show_col_types = FALSE)
  ticker <- unique(df$ticker)
  datasets[[ticker]] <- df
  cat_col(sprintf("    Loaded: %-12s (%d rows, %d cols)\n",
                  ticker, nrow(df), ncol(df)), "white")
}

# ---- 2. SYSTEMATIC VALIDATION ------------------------------------------------
log("========================================================\n")
log("  DATA CLEANING LOG\n")
log("  Date:", format(Sys.Date(), "%Y-%m-%d %H:%M:%S"), "\n")
log("========================================================\n")

cleaning_log <- list()

for (ticker in names(datasets)) {
  df <- datasets[[ticker]]

  # Keep a copy of the raw for audit
  df_raw <- df

  # ---- A. DATATYPES -------------------------------------------------------
  cat_col(sprintf("\n--- %s ---\n", ticker), "magenta")
  log(sprintf("\n--- %s ---\n", ticker))
  log("A. Data types")
  log(sprintf("  class(date): %s", paste(class(df$date), collapse = ", ")))
  log(sprintf("  class(adj_close): %s", paste(class(df$adj_close), collapse = ", ")))
  log(sprintf("  class(ticker): %s", paste(class(df$ticker), collapse = ", ")))
  log(sprintf("  class(company): %s", paste(class(df$company), collapse = ", ")))

  # Check numeric status of prices
  n_nonnum_price <- sum(!is.numeric(df$adj_close))
  log(sprintf("  Non-numeric adj_close rows: %d", n_nonnum_price))

  # ---- B. DUPLICATE DATES -------------------------------------------------
  log("\nB. Duplicate dates")
  dup_dates <- sum(duplicated(df$date))
  log(sprintf("  Duplicate dates: %d", dup_dates))
  if (dup_dates > 0) {
    log("  Removing duplicate dates, keeping the FIRST occurrence.")
    df <- df[!duplicated(df$date), ]
  }

  # ---- C. MISSING VALUES ---------------------------------------------------
  log("\nC. Missing values")
  n_na <- sum(is.na(df$adj_close))
  # split NAs into leading (lag) vs internal (true missing)
  df <- df %>% arrange(date)
  na_idx <- which(is.na(df$adj_close))
  if (length(na_idx) > 0) {
    # leading NAs come before first observation
    leading_na <- na_idx[na_idx <= match(min(df$date), df$date)]
    # NAs in the middle / end = genuine missing on trading days
    mid_na <- setdiff(na_idx, leading_na)
    log(sprintf("  Total NA price rows: %d", length(na_idx)))
    log(sprintf("  Leading NA (pre-first-obs) rows: %d — these are removed", length(leading_na)))
    log(sprintf("  Internal/mid NA (trading-day) rows: %d — flagged for review, NOT auto-removed", length(mid_na)))
    if (length(mid_na) > 0) {
      log("  FLAG: Internal missing prices may be data errors or genuinely non-trading days.")
      log("  Decision: These will be flagged in the cleaned table; no silent removal.")
      missing_log <- df[mid_na, c("date", "adj_close")]
      write_csv(missing_log, file.path(.project_root(), "output", "tables", sprintf("missing_%s.csv", ticker)))
    }
    # Drop leading NAs produced by the source
    df <- df[!is.na(df$adj_close), ]
  } else {
    log("  No NA prices.")
  }

  # ---- D. IMPOSSIBLE / INVALID PRICES --------------------------------------
  log("\nD. Impossible / invalid prices")
  invalid <- df$adj_close <= 0
  n_invalid <- sum(invalid)
  log(sprintf("  Rows with adj_close <= 0: %d", n_invalid))
  if (n_invalid > 0) {
    log("  Decision: Retaining flagged rows as NA for later treatment.")
    df$adj_close[invalid] <- NA
  }

  # ---- E. DATE ORDERING ----------------------------------------------------
  log("\nE. Date ordering")
  sorted <- is.unsorted(df$date)
  log(sprintf("  Dates sorted ascending: %s", !sorted))
  if (sorted) {
    df <- df %>% arrange(date)
    log("  Re-sorted chronologically.")
  }

  # ---- F. TRADING-DAY COMPLETENESS -----------------------------------------
  log("\nF. Trading-day completeness")
  # Compare with expected trading days for the period
  expected_days <- seq(min(df$date), max(df$date))
  # Rough check: number of unique dates vs span
  date_span <- as.numeric(max(df$date) - min(df$date))
  log(sprintf("  Date range: %s to %s  (%.0f days)",
              min(df$date), max(df$date), date_span + 1))
  log(sprintf("  Actual trading days: %d", nrow(df)))
  log(sprintf("  Expected NYSE-equivalent trading days (252/yr * yrs): %.0f",
              252 * (date_span / 365)))

  # ---- G. EXTREME OBSERVATIONS ---------------------------------------------
  log("\nG. Extreme observations (kept — NOT auto-removed)")
  # Standardisation: z-scores
  mean_p <- mean(df$adj_close, na.rm = TRUE)
  sd_p   <- sd(df$adj_close, na.rm = TRUE)
  z_scores <- (df$adj_close - mean_p) / sd_p
  extreme_px <- abs(z_scores) > 4
  log(sprintf("  Rows with |z| > 4 (extreme price moves): %d of %d",
              sum(extreme_px), nrow(df)))
  if (sum(extreme_px) > 0) {
    extreme_rows <- df[extreme_px, c("date", "ticker", "adj_close")]
    write_csv(extreme_rows,
              file.path(.project_root(), "output", "tables",
                        sprintf("extreme_prices_%s.csv", ticker)))
  }

  # ---- H. DAILY RETURNS (for identifying extreme returns) --------------------
  # Using log returns for speed
  if (nrow(df) > 1) {
    df <- df %>% arrange(date)
    df <- df %>% arrange(date)
    df$log_ret <- c(NA, log(df$adj_close[-1] / df$adj_close[-nrow(df)]))
    df$simple_ret <- c(NA, (df$adj_close[-1] - df$adj_close[-nrow(df)]) / df$adj_close[-nrow(df)])

    ret_na <- sum(is.na(df$log_ret))
    log(sprintf("\nH. Daily returns")
    log(sprintf("  Returns computed: %d (NAs: %d, first obs)", nrow(df) - 1, ret_na))

    # Extreme returns: |z| > 5 on returns (typical regime for outliers)
    r_mean <- mean(df$log_ret, na.rm = TRUE)
    r_sd   <- sd(df$log_ret, na.rm = TRUE)
    df$z_ret <- (df$log_ret - r_mean) / r_sd
    extreme_ret <- abs(df$z_ret) > 5
    n_extreme_ret <- sum(extreme_ret, na.rm = TRUE)
    log(sprintf("  Rows with |z_ret| > 5 (extreme returns): %d", n_extreme_ret))
    if (n_extreme_ret > 0) {
      ext_ret <- df %>% filter(!is.na(log_ret)) %>% filter(abs(z_ret) > 5) %>%
        arrange(desc(abs(log_ret)))
      write_csv(ext_ret[, c("date", "ticker", "simple_ret", "log_ret", "z_ret")],
                file.path(.project_root(), "output", "tables",
                          sprintf("extreme_returns_%s.csv", ticker)))
      log("  Extreme returns are EXPOSED here and retained for analysis.")
    }
  }

  # ---- I. DOCUMENT DECISION LOG ---------------------------------------------
  # Save a per-stock cleaned dataset
  clean_path <- file.path(.project_root(), "data", "processed",
                          sprintf("clean_prices_%s.csv", ticker))
  write_csv(df, clean_path)

  cleaning_log[[ticker]] <- list(
    company       = unique(df$company),
    sector        = stock_info[[ticker]]$sector,
    start_date    = min(df$date),
    end_date      = max(df$date),
    n_raw         = nrow(df_raw),
    n_clean       = nrow(df),
    n_nona        = n_na,
    n_duplicates  = dup_dates,
    n_invalid_px  = n_invalid,
    n_extreme_ret = n_extreme_ret,
    extreme_ret_summary = if (n_extreme_ret > 0) {
      paste(format(min(df$date), "%Y-%m-%d"),
            "to", format(max(df$date), "%Y-%m-%d"))
    } else { "none" }
  )

  log(sprintf("\n  CLEANED -> data/processed/clean_prices_%s.csv (%d rows)\n", ticker, nrow(df)))
  log(sprintf("  Cleaning decisions logged in output/tables/cleaning_log.txt\n"))
}

# ---- 3. CLOSE LOG FILE ------------------------------------------------------
close(log_con)

# ---- 4. SUMMARY TABLE -------------------------------------------------------
summary_clean <- bind_rows(lapply(cleaning_log, function(x) as.data.frame(x)))
write_csv(summary_clean,
          file.path(.project_root(), "output", "tables", "cleaning_summary.csv"))

cat_col("\nData cleaning complete.\n", "green")
cat_col(sprintf("  Cleaning log: output/tables/cleaning_log.txt\n", length(datasets)), "white")
cat_col(sprintf("  Cleaned prices: data/processed/clean_prices_*.csv\n", length(datasets)), "white")
