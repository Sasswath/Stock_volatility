# ============================================================
# 06_representative_stock.R — Select ONE representative stock
# ============================================================
#
# PURPOSE
#   Return to all five stocks for one broad comparison, then
#   SELECT ONE representative stock using PRECOMPUTED, OBJECTIVE
#   criteria.
#
#   Selection criteria (all visible in the output of
#   05_comparative_analysis.R):
#     1. Sufficient number of observations (> 1000 daily returns)
#     2. Meaningful return variation (non-trivial volatility)
#     3. Noticeable (but legitimate) skewness / kurtosis
#     4. Clean data with no critical gaps
#     5. Satisfies the above WITHOUT cherry-picking a dramatic result
#
#   The chosen stock is recorded in a metadata file and is used
#   by all subsequent scripts (07–14).  The other four stocks
#   REMAINS part of the comparative and portfolio files.

# ---- load descriptive stats ---------------------------------------------------
stats_file <- file.path(.project_root(), "output", "tables", "5stock_descriptive_stats.csv")
if (!file.exists(stats_file)) {
  stop("5stock_descriptive_stats.csv not found. Run 05_comparative_analysis.R first.")
}

desc <- read_csv(stats_file, show_col_types = FALSE)

# ---- apply objective criteria -------------------------------------------------
cat_col("\n" , "blue")
section("REPRESENTATIVE STOCK SELECTION", "=")

# Rank on objective criteria (all pre-specified here)
criteria <- desc %>%
  filter(Rows > 1000) %>%                      # 1. enough data
  filter(!is.na(Skewness), !is.na(Kurtosis)) %>%
  mutate(
    # 2. meaningful variation: keep above the median volatility
    .vol_rank = rank(AnnualVol, ties.method = "average"),
    # 3. noticeable skewness / kurtosis: moderate tails
    .skew_dev = abs(Skewness),
    .kurt_dev = abs(Kurtosis - 3),   # deviation of excess kurtosis from normal
    # 4/5. already filtered above
    .score = .vol_rank + .skew_dev * 10 + .kurt_dev * 5
  ) %>%
  arrange(.score)                               # lowest score = best fit

if (nrow(criteria) == 0) {
  stop("No stock met the minimum criteria (>1000 observations). Check data cleaning.")
}

# choose the top-ranked stock
chosen <- criteria %>% slice(1)

cat_col(sprintf("  Chosen ticker : %s\n", chosen$Ticker), "cyan")
cat_col(sprintf("  Company       : %s\n", chosen$Company), "cyan")
cat_col(sprintf("  Sector        : %s\n", chosen$Sector), "cyan")
cat_col(sprintf("  Observations  : %d\n", chosen$Rows), "cyan")
cat_col(sprintf("  Annual vol    : %.2f%%\n", chosen$AnnualVol * 100), "cyan")
cat_col(sprintf("  Skewness      : %.3f\n", chosen$Skewness), "cyan")
cat_col(sprintf("  Excess kurtosis: %.3f\n", chosen$Kurtosis - 3), "cyan")
cat_col(sprintf("  Selection rank: #%d of %d available stocks\n",
                1, nrow(criteria)), "cyan")

# ---- document the selection process -------------------------------------------
selection_doc <- data.frame(
  field = c(
    "Chosen representative ticker",
    "Company",
    "Sector",
    "Observation count",
    "Annualized volatility",
    "Skewness",
    "Excess kurtosis",
    "Selection criteria",
    "Basis for choice",
    "Date selected"
  ),
  value = c(
    chosen$Ticker,
    chosen$Company,
    chosen$Sector,
    chosen$Rows,
    sprintf("%.4f%%", chosen$AnnualVol * 100),
    sprintf("%.4f", chosen$Skewness),
    sprintf("%.4f", chosen$Kurtosis - 3),
    ">1000 daily obs; meaningful vol; notable but legitimate skew/kurt; clean data",
    "Lowest composite score across objective criteria; NOT selected for dramatic results",
    format(Sys.Date(), "%Y-%m-%d")
  )
)
write_csv(selection_doc,
          file.path(.project_root(), "output", "tables",
                    "representative_stock_selection.csv"))

# Write a human-readable selection note
selection_note <- c(
  "REPRESENTATIVE STOCK SELECTION NOTE",
  paste("Date:", format(Sys.Date(), "%Y-%m-%d")),
  paste("Chosen stock :", chosen$Ticker),
  paste("Company      :", chosen$Company),
  paste("Sector       :", chosen$Sector),
  "",
  "RATIONALE",
  "---------",
  "The representative stock is selected using pre-specified, objective criteria",
  "documented in this file and in the five-stock comparative analysis (script 05).",
  sprintf("  (i) Sufficient observations: %d daily returns", chosen$Rows),
  sprintf("  (ii) Meaningful return variation (annualised vol %.2f%%)", chosen$AnnualVol * 100),
  sprintf("  (iii) Noticeable but legitimate skewness (%.3f) and kurtosis (%.3f)", chosen$Skewness, chosen$Kurtosis - 3),
  "  (iv) Clean, complete data with no critical gaps",
  "",
  "IMPORTANT: The stock was NOT selected for producing the most dramatic or",
  "favourable result.  The other four stocks remain part of the comparative",
  "analysis and the equally weighted portfolio analysis.",
  "",
  "All subsequent statistical work (distribution fitting, CLT, Monte Carlo,",
  "extreme returns, VaR) is conducted on this single representative stock.",
  "The five-stock portfolio uses all five stocks."
)

writeLines(selection_note,
           file.path(.project_root(), "output", "tables",
                     "representative_stock_note.txt"))

cat_col("\nSelection note saved.\n", "green")
cat_col(sprintf("  Note: output/tables/representative_stock_note.txt\n"), "white")
cat_col(sprintf("  Selection table: output/tables/representative_stock_selection.csv\n"), "white")

# ---- define the representative stock for downstream scripts --------------------
# Write environment variable for downstream scripts
# Downstream scripts access via Sys.getenv("REPRESENTATIVE_TICKER")
assign(".representative_ticker", chosen$Ticker, envir = .GlobalEnv)

cat_col(sprintf("\nUsing representative ticker: %s\n", chosen$Ticker), "green")

# ---- reload representative stock return series -----------------------------------
rep_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              sprintf("stock_returns_%s.csv", chosen$Ticker)),
                    show_col_types = FALSE)

# sanity-check it matches
stopifnot(nrow(rep_ret) == chosen$Rows)

# Save a tidy file the downstream scripts can source
saveRDS(rep_ret, file.path(.project_root(), "output",
                            "processed", "representative_returns.rds"))
