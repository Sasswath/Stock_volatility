# ============================================================
# 03_data_cleaning.R — Validate and clean raw stock data
# ============================================================

library(dplyr)
library(readr)
source(file.path(.project_root(), "R", "00_stock_info.R"))

cat_col("\n", "blue")
section("DATA CLEANING PIPELINE", "=")

# ---- configuration & output directory ---------------------------------------
log_file <- file.path(.project_root(), "output", "tables", "cleaning_log.txt")
dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(.project_root(), "data", "processed"), showWarnings = FALSE, recursive = TRUE)

# Open log file
log_con <- file(log_file, open = "w")

# ---- helper to write log lines ----------------------------------------------
log_msg <- function(...) writeLines(sprintf(...), log_con)

# ---- 1. LOAD raw data --------------------------------------------------------
# Only load per-stock raw files to avoid duplicate/combined multi-ticker data
raw_files <- list.files(file.path(.project_root(), "data", "raw"),
                        pattern = "^raw_prices_.*\\.csv$", full.names = TRUE)

cat_col(sprintf("  Loading %d raw file(s)...\n", length(raw_files)), "cyan")

datasets <- list()
for (f in raw_files) {
  df <- read_csv(f, show_col_types = FALSE)
  ticker <- unique(df$ticker)[1]
  datasets[[ticker]] <- df
  cat_col(sprintf("    Loaded: %-12s (%d rows, %d cols)\n",
                  ticker, nrow(df), ncol(df)), "white")
}

# ---- 2. SYSTEMATIC VALIDATION ------------------------------------------------
log_msg("========================================================\n")
log_msg("  DATA CLEANING LOG\n")
log_msg("  Date: %s\n", format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
log_msg("========================================================\n")

cleaning_log <- list()

for (ticker in names(datasets)) {
  df <- datasets[[ticker]]
  df_raw <- df
  
  # ---- A. DATATYPES -------------------------------------------------------
  cat_col(sprintf("\n--- %s ---\n", ticker), "magenta")
  log_msg(sprintf("\n--- %s ---\n", ticker))
  log_msg("A. Data types")
  log_msg(sprintf("  class(date): %s", paste(class(df$date), collapse = ", ")))
  log_msg(sprintf("  class(adj_close): %s", paste(class(df$adj_close), collapse = ", ")))
  log_msg(sprintf("  class(ticker): %s", paste(class(df$ticker), collapse = ", ")))
  log_msg(sprintf("  class(company): %s", paste(class(df$company), collapse = ", ")))
  
  n_nonnum_price <- sum(!is.numeric(df$adj_close))
  log_msg(sprintf("  Non-numeric adj_close rows: %d", n_nonnum_price))
  
  # ---- B. DUPLICATE DATES -------------------------------------------------
  log_msg("\nB. Duplicate dates")
  dup_dates <- sum(duplicated(df$date))
  log_msg(sprintf("  Duplicate dates: %d", dup_dates))
  if (dup_dates > 0) {
    log_msg("  Removing duplicate dates, keeping the FIRST occurrence.")
    df <- df[!duplicated(df$date), ]
  }
  
  # ---- C. MISSING VALUES ---------------------------------------------------
  log_msg("\nC. Missing values")
  n_na <- sum(is.na(df$adj_close))
  df <- df %>% arrange(date)
  na_idx <- which(is.na(df$adj_close))
  if (length(na_idx) > 0) {
    leading_na <- na_idx[na_idx <= match(min(df$date), df$date)]
    mid_na <- setdiff(na_idx, leading_na)
    log_msg(sprintf("  Total NA price rows: %d", length(na_idx)))
    log_msg(sprintf("  Leading NA (pre-first-obs) rows: %d — these are removed", length(leading_na)))
    log_msg(sprintf("  Internal/mid NA (trading-day) rows: %d — flagged for review, NOT auto-removed", length(mid_na)))
    if (length(mid_na) > 0) {
      log_msg("  FLAG: Internal missing prices may be data errors or genuinely non-trading days.")
      log_msg("  Decision: These will be flagged in the cleaned table; no silent removal.")
      missing_log <- df[mid_na, c("date", "adj_close")]
      write_csv(missing_log, file.path(.project_root(), "output", "tables", sprintf("missing_%s.csv", ticker)))
    }
    df <- df[!is.na(df$adj_close), ]
  } else {
    log_msg("  No NA prices.")
  }
  
  # ---- D. IMPOSSIBLE / INVALID PRICES --------------------------------------
  log_msg("\nD. Impossible / invalid prices")
  invalid <- df$adj_close <= 0
  n_invalid <- sum(invalid)
  log_msg(sprintf("  Rows with adj_close <= 0: %d", n_invalid))
  if (n_invalid > 0) {
    log_msg("  Decision: Retaining flagged rows as NA for later treatment.")
    df$adj_close[invalid] <- NA
  }
  
  # ---- E. DATE ORDERING ----------------------------------------------------
  log_msg("\nE. Date ordering")
  sorted <- is.unsorted(df$date)
  log_msg(sprintf("  Dates sorted ascending: %s", !sorted))
  if (sorted) {
    df <- df %>% arrange(date)
    log_msg("  Re-sorted chronologically.")
  }
  
  # ---- F. TRADING-DAY COMPLETENESS -----------------------------------------
  log_msg("\nF. Trading-day completeness")
  expected_days <- seq(min(df$date), max(df$date), by="days")
  date_span <- as.numeric(max(df$date) - min(df$date))
  log_msg(sprintf("  Date range: %s to %s  (%.0f days)",
              min(df$date), max(df$date), date_span + 1))
  log_msg(sprintf("  Actual trading days: %d", nrow(df)))
  log_msg(sprintf("  Expected NYSE-equivalent trading days (252/yr * yrs): %.0f",
              252 * (date_span / 365)))
  
  # ---- G. EXTREME OBSERVATIONS ---------------------------------------------
  log_msg("\nG. Extreme observations (kept — NOT auto-removed)")
  mean_p <- mean(df$adj_close, na.rm = TRUE)
  sd_p   <- sd(df$adj_close, na.rm = TRUE)
  z_scores <- (df$adj_close - mean_p) / sd_p
  extreme_px <- abs(z_scores) > 4
  log_msg(sprintf("  Rows with |z| > 4 (extreme price moves): %d of %d",
              sum(extreme_px), nrow(df)))
  if (sum(extreme_px) > 0) {
    extreme_rows <- df[extreme_px, c("date", "ticker", "adj_close")]
    write_csv(extreme_rows,
              file.path(.project_root(), "output", "tables",
                        sprintf("extreme_prices_%s.csv", ticker)))
  }
  
  # ---- H. DAILY RETURNS (for identifying extreme returns) --------------------
  if (nrow(df) > 1) {
    df <- df %>% arrange(date)
    df$log_ret <- c(NA, log(df$adj_close[-1] / df$adj_close[-nrow(df)]))
    df$simple_ret <- c(NA, (df$adj_close[-1] - df$adj_close[-nrow(df)]) / df$adj_close[-nrow(df)])
    
    ret_na <- sum(is.na(df$log_ret))
    log_msg("\nH. Daily returns")
    log_msg(sprintf("  Returns computed: %d (NAs: %d, first obs)", nrow(df) - 1, ret_na))
    
    # Extreme returns: |z| > 5 on returns
    r_mean <- mean(df$log_ret, na.rm = TRUE)
    r_sd   <- sd(df$log_ret, na.rm = TRUE)
    df$z_ret <- (df$log_ret - r_mean) / r_sd
    extreme_ret <- abs(df$z_ret) > 5
    n_extreme_ret <- sum(extreme_ret, na.rm = TRUE)
    log_msg(sprintf("  Rows with |z_ret| > 5 (extreme returns): %d", n_extreme_ret))
    if (n_extreme_ret > 0) {
      ext_ret <- df %>% filter(!is.na(log_ret)) %>% filter(abs(z_ret) > 5) %>%
        arrange(desc(abs(log_ret)))
      write_csv(ext_ret[, c("date", "ticker", "simple_ret", "log_ret", "z_ret")],
                file.path(.project_root(), "output", "tables",
                          sprintf("extreme_returns_%s.csv", ticker)))
      log_msg("  Extreme returns are EXPOSED here and retained for analysis.")
    }
  }
  
  # ---- I. DOCUMENT DECISION LOG ---------------------------------------------
  clean_path <- file.path(.project_root(), "data", "processed",
                          sprintf("clean_prices_%s.csv", ticker))
  write_csv(df, clean_path)
  
  sec <- if (!is.null(stock_info[[ticker]]$sector)) stock_info[[ticker]]$sector else "Unknown"
  
  cleaning_log[[ticker]] <- list(
    company             = unique(df$company)[1],
    sector              = sec,
    start_date          = min(df$date),
    end_date            = max(df$date),
    n_raw               = nrow(df_raw),
    n_clean             = nrow(df),
    n_nona              = n_na,
    n_duplicates        = dup_dates,
    n_invalid_px        = n_invalid,
    n_extreme_ret       = if (exists("n_extreme_ret")) n_extreme_ret else 0,
    extreme_ret_summary = if (exists("n_extreme_ret") && n_extreme_ret > 0) {
      paste(format(min(df$date), "%Y-%m-%d"), "to", format(max(df$date), "%Y-%m-%d"))
    } else { "none" }
  )
  
  log_msg(sprintf("\n  CLEANED -> data/processed/clean_prices_%s.csv (%d rows)\n", ticker, nrow(df)))
  log_msg("  Cleaning decisions logged in output/tables/cleaning_log.txt\n")
}

# ---- 3. CLOSE LOG FILE ------------------------------------------------------
close(log_con)

# ---- 4. SUMMARY TABLE -------------------------------------------------------
summary_clean <- bind_rows(lapply(cleaning_log, function(x) as.data.frame(x)))
write_csv(summary_clean,
          file.path(.project_root(), "output", "tables", "cleaning_summary.csv"))

cat_col("\nData cleaning complete.\n", "green")
cat_col("  Cleaning log: output/tables/cleaning_log.txt\n", "white")
cat_col("  Cleaned prices: data/processed/clean_prices_*.csv\n", "white")
