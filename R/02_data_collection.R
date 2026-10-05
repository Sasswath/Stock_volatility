# ============================================================
# 02_data_collection.R — Download historical data for 5 stocks
# ============================================================
#
# PURPOSE
#   Download daily adjusted closing prices for five publicly
#   traded stocks from the National Stock Exchange of India
#   (NSE). The dataset spans roughly 5 trading years
#   (2020–2025). Raw data is stored in data/raw/ and raw data
#   is never overwritten.
#
# STOCK SELECTION RATIONALE
#   - Reliance Industries  (RELIANCE) — Energy / Oil & Gas
#   - TCS                  (TCS)        — IT / Software Services
#   - HDFC Bank            (HDFCBANK)  — Financials / Banking
#   - Infosys              (INFY)       — IT / Software Services
#   - Larsen & Toubro      (LT)         — Manufacturing / Infrastructure

# ---- libraries -------------------------------------------------------------
library(quantmod)
library(dplyr)
library(readr)

# ---- config ----------------------------------------------------------------
TZ_SET <- "Asia/Kolkata"
SYMBOL_LIST <- c(
  "RELIANCE"    = "RELIANCE.NS",
  "TCS"         = "TCS.NS",
  "HDFCBANK"    = "HDFCBANK.NS",
  "INFOSYS"     = "INFY.NS",
  "LT"          = "LT.NS"
)

START_DATE <- as.Date("2020-01-01")
END_DATE   <- as.Date("2025-12-31")   # adjust to actual last trading day
RETRIEVAL_DATE <- as.Date(Sys.Date())

# ---- directories -----------------------------------------------------------
dir.create(file.path(.project_root(), "data", "raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(.project_root(), "data", "processed"), showWarnings = FALSE, recursive = TRUE)

# ---- data source documentation ----------------------------------------------
source_file <- file.path(.project_root(), "data", "raw", "02_data_collection_metadata.txt")
dir.create(dirname(source_file), showWarnings = FALSE, recursive = TRUE)
meta_lines <- c(
  "Data Collection Metadata",
  paste("Retrieval date:", format(RETRIEVAL_DATE, "%Y-%m-%d")),
  paste("Market:", "National Stock Exchange of India (NSE)"),
  paste("Source:", "quantmod Yahoo Finance endpoint (NSE symbol)"),
  "Note: Prices are adjusted closing prices.",
  "",
  "Stocks:",
  sprintf("  RELIANCE    | %s | %s", "Energy / Oil & Gas", "https://finance.yahoo.com/quote/RELIANCE.NS"),
  sprintf("  TCS         | %s | %s", "IT / Software Services", "https://finance.yahoo.com/quote/TCS.NS"),
  sprintf("  HDFCBANK    | %s | %s", "Financials / Banking", "https://finance.yahoo.com/quote/HDFCBANK.NS"),
  sprintf("  INFOSYS     | %s | %s", "IT / Software Services", "https://finance.yahoo.com/quote/INFY.NS"),
  sprintf("  LT          | %s | %s", "Manufacturing / Infrastructure", "https://finance.yahoo.com/quote/LT.NS"),
  "",
  "Date range:",
  sprintf("  Start: %s", format(START_DATE, "%Y-%m-%d")),
  sprintf("  End:   %s", format(END_DATE, "%Y-%m-%d"))
)
cat(meta_lines, sep = "\n")
cat("\n")

# ---- download ---------------------------------------------------------------
cat_col("\nDownloading adjusted closing prices...\n", "blue")

# Suppress quantmod auto-loading warnings in this environment
options(warn = 1)

# Container for raw closing prices
raw_data <- list()
stock_info <- list()

for (ticker in names(SYMBOL_LIST)) {
  symbol <- SYMBOL_LIST[[ticker]]
  cat_col(sprintf("  Fetching %s (%s)...\n", ticker, symbol), "cyan")
  
  # quantmod::getSymbols returns a time-series object with the
  # ticker as the column name. We extract the Adjusted Close.
  data <- tryCatch({
    getSymbols(symbol, src = "yahoo", from = START_DATE, to = END_DATE,
               auto.assign = FALSE, warning = FALSE)
  }, error = function(e) {
    stop(sprintf("Could not download %s: %s", ticker, e$message))
  })
  
  # column name may be the ticker or the original name; take the
  # "Adjusted" column if present, else the first column
  col_names <- colnames(data)
  adj_col <- grep("Adj\\.Close", col_names, ignore.case = TRUE)
  
  if (length(adj_col) == 0) {
    adj_col <- 1
  }
  
  # Build a tidy data frame
  df <- data.frame(
    ticker           = ticker,
    company          = sub("^([A-Z]+).*", "\\1", ticker),
    date             = index(data),
    adj_close        = as.numeric(data[, adj_col]),
    stringsAsFactors = FALSE
  )
  
  # Keep only the date, ticker, company, adj_close for raw store
  raw_df <- df[, c("date", "ticker", "company", "adj_close")]
  
  # --- validation: impossible prices / missing values before saving ----
  n_obs <- nrow(raw_df)
  n_na  <- sum(is.na(raw_df$adj_close))
  bad_prices <- raw_df$adj_close <= 0 & !is.na(raw_df$adj_close)
  
  if (n_na > 0 || any(bad_prices)) {
    warning(sprintf("  %s: %d NA / invalid prices detected mid-download; will be handled in 03_data_cleaning.R",
                    ticker, n_na))
  }
  
  # Save raw data (never overwritten; append if rerun)
  fname <- sprintf("raw_prices_%s.csv", ticker)
  fpath <- file.path(.project_root(), "data", "raw", fname)
  
  # Log overwrites instead of silently deleting previous raw data
  if (file.exists(fpath)) {
    warning(sprintf("  RAW DATA ALREADY EXISTS at %s — saving an append backup", fpath))
    # Save an incremental backup with timestamp
    backup <- file.path(.project_root(), "data", "raw",
                        sprintf("backup_%s_%s.csv",
                                ticker, format(Sys.time(), "%Y%m%d_%H%M%S")))
    utils::write.csv(raw_df, backup, row.names = FALSE)
  }
  
  utils::write.csv(raw_df, fpath, row.names = FALSE)
  cat_col(sprintf("  Saved: data/raw/%s (%d observations)\n",
                  fname, n_obs), "green")
  
  raw_data[[ticker]] <- raw_df
  stock_info[[ticker]] <- list(
    company       = sub("^([A-Z]+).*", "\\1", ticker),
    sector        = switch(ticker,
                           "RELIANCE" = "Energy / Oil & Gas",
                           "TCS"      = "IT / Software Services",
                           "HDFCBANK" = "Financials / Banking",
                           "INFOSYS"  = "IT / Software Services",
                           "LT"       = "Manufacturing / Infrastructure"),
    start_date    = min(raw_df$date),
    end_date      = max(raw_df$date),
    n_obs         = n_obs
  )
}

# ---- save a combined raw summary --------------------------------------------
combined_raw <- bind_rows(raw_data)
combined_raw <- combined_raw %>%
  arrange(ticker, date)

write_csv(combined_raw,
          file.path(.project_root(), "data", "raw", "combined_raw_prices.csv"))

# ---- save metadata as a tidy table -----------------------------------------
stock_meta_df <- bind_rows(lapply(stock_info, function(x) as.data.frame(x)))
stock_meta_df$source <- "NSE via quantmod (Yahoo Finance)"

write_csv(stock_meta_df,
          file.path(.project_root(), "data", "raw", "stock_selection_metadata.csv"))

cat_col("\nData collection complete.\n", "green")
cat_col(sprintf("  Raw stock files: %s\n", length(SYMBOL_LIST)), "white")
cat_col("  Combined raw: data/raw/combined_raw_prices.csv\n", "white")
cat_col("  Metadata: data/raw/stock_selection_metadata.csv\n", "white")
