# ============================================================
# 05_comparative_analysis.R — Five-stock comparative analysis
# ============================================================
#
# PURPOSE
#   Do a broad, five-stock comparison.  This stage is BROAD
#   only — it does NOT run the full statistical pipeline on
#   each stock.  It answers: "How do the five stocks differ
#   statistically?"
#
#   Metrics: mean, median, sd, variance, min, max, range,
#            Q1, Q3, IQR, skewness, kurtosis, annualized
#            volatility, rolling volatility.
#
#   Outputs: side-by-side table, comparative volatility chart,
#            comparative return chart, boxplot, distribution
#            panels, correlation matrix + heatmap, rolling-vol
#            comparison, cumulative performance.

# ---- load combined returns ---------------------------------------------------
all_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              "stock_returns_combined.csv"),
                    show_col_types = FALSE)

# Gadget: limit to a single ticker for fast testing
available <- unique(all_ret$ticker)
if (identical(Sys.getenv("TEST_TICKER"), "")) {
  TICKER <- available
} else {
  TICKER <- Sys.getenv("TEST_TICKER")
  cat_col(sprintf("  Running in TEST mode for ticker: %s\n", TICKER), "yellow")
}
all_ret <- all_ret %>% filter(ticker == TICKER)

# ---- 1. summary table ---------------------------------------------------------
metrics <- data.frame(
  Mean          = mean(all_ret$log_ret, na.rm = TRUE),
  Median        = median(all_ret$log_ret, na.rm = TRUE),
  SD            = sd(all_ret$log_ret, na.rm = TRUE),
  Variance      = sd(all_ret$log_ret, na.rm = TRUE)^2,
  Minimum       = min(all_ret$log_ret, na.rm = TRUE),
  Maximum       = max(all_ret$log_ret, na.rm = TRUE),
  Range         = max(all_ret$log_ret, na.rm = TRUE) - min(all_ret$log_ret, na.rm = TRUE),
  Q1            = quantile(all_ret$log_ret, 0.25, na.rm = TRUE),
  Q3            = quantile(all_ret$log_ret, 0.75, na.rm = TRUE),
  IQR           = IQR(all_ret$log_ret, na.rm = TRUE),
  Skewness      = moments::skewness(all_ret$log_ret, na.rm = TRUE),
  Kurtosis      = moments::kurtosis(all_ret$log_ret, na.rm = TRUE),
  AnnualVol     = annual_vol(sd(all_ret$log_ret, na.rm = TRUE)),
  Rows          = length(all_ret$log_ret)
)
# format percentages
fmt_pct <- function(x) sprintf("%.4f%%", x * 100)
metrics_pct <- metrics
metrics_pct$AnnualVol <- fmt_pct(metrics$AnnualVol)
metrics_pct$SD        <- fmt_pct(metrics$SD)

cat_col("\n" , "blue")
section("FIVE-STOCK COMPARATIVE SUMMARY (single ticker test)")
cat("\n")
print_table(metrics, caption = "Descriptive statistics — single ticker")
cat("\n")

# For the FULL run (5 tickers), produce a side-by-side table
if (identical(TICKER, available)) {
  all_metrics <- bind_rows(lapply(available, function(t) {
    r <- all_ret %>% filter(ticker == t)
    data.frame(
      Ticker = t,
      Company = unique(r$company),
      Sector = stock_info[[t]]$sector,
      Mean = mean(r$log_ret, na.rm = TRUE),
      Median = median(r$log_ret, na.rm = TRUE),
      SD = sd(r$log_ret, na.rm = TRUE),
      Variance = sd(r$log_ret, na.rm = TRUE)^2,
      Minimum = min(r$log_ret, na.rm = TRUE),
      Maximum = max(r$log_ret, na.rm = TRUE),
      Range = max(r$log_ret, na.rm = TRUE) - min(r$log_ret, na.rm = TRUE),
      Q1 = quantile(r$log_ret, 0.25, na.rm = TRUE),
      Q3 = quantile(r$log_ret, 0.75, na.rm = TRUE),
      IQR = IQR(r$log_ret, na.rm = TRUE),
      Skewness = moments::skewness(r$log_ret, na.rm = TRUE),
      Kurtosis = moments::kurtosis(r$log_ret, na.rm = TRUE),
      AnnualVol = annual_vol(sd(r$log_ret, na.rm = TRUE)),
      Rows = length(r$log_ret),
      stringsAsFactors = FALSE
    )
  }))

  write_csv(all_metrics,
            file.path(.project_root(), "output", "tables",
                      "5stock_descriptive_stats.csv"))

  cat_col("\nFull five-stock table saved.\n", "green")
  print_table(all_metrics, caption = "Descriptive statistics — all five stocks")

  # ---- 2. comparative volatility ----------------------------------------------
  vol_df <- all_metrics %>% arrange(desc(AnnualVol))
  p_vol <- ggplot(vol_df, aes(x = reorder(Ticker, AnnualVol), y = AnnualVol,
                              fill = reorder(Ticker, AnnualVol))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Annualised Volatility — Five Stocks",
         x = "Stock", y = "Annualised Volatility (daily σ × √252)",
         subtitle = "Based on daily log returns, ~2020–2025") +
    scale_fill_viridis_d() +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
  fig_dir("comparative_volatility.png")
  ggsave(fig_dir("comparative_volatility.png"), p_vol,
         width = 9, height = 5.5, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/comparative_volatility.png\n"), "green")

  # ---- 3. comparative returns (cumulative) -------------------------------------
  # Compute cumulative log-return index (base = 1)
  cum_df <- all_ret %>%
    group_by(ticker) %>%
    arrange(date) %>%
    mutate(cum_ret = cumprod(1 + log_ret) * 100)  # index, base 100

  p_ret <- ggplot(cum_df, aes(x = date, y = cum_ret, color = ticker, group = ticker)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = "Cumulative Return Index — Five Stocks",
         x = "Date", y = "Cumulative return index (base 100)",
         subtitle = "Base period = first available day of each stock") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  fig_dir("comparative_returns.png")
  ggsave(fig_dir("comparative_returns.png"), p_ret,
         width = 11, height = 5.5, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/comparative_returns.png\n"), "green")

  # ---- 4. boxplots --------------------------------------------------------------
  p_box <- ggplot(all_ret, aes(x = ticker, y = log_ret, fill = ticker)) +
    geom_boxplot(alpha = 0.8) +
    labs(title = "Return Distribution Boxplots — Five Stocks",
         x = "Stock", y = "Daily log return") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
  fig_dir("boxplots_five_stocks.png")
  ggsave(fig_dir("boxplots_five_stocks.png"), p_box,
         width = 9, height = 5, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/boxplots_five_stocks.png\n"), "green")

  # ---- 5. return distribution panels ---------------------------------------------
  p_dist <- ggplot(all_ret, aes(x = log_ret, fill = ticker)) +
    geom_histogram(bins = 60, alpha = 0.5, position = "identity") +
    labs(title = "Return Distribution — Five Stocks",
         x = "Daily log return", y = "Frequency") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  fig_dir("distribution_five_stocks.png")
  ggsave(fig_dir("distribution_five_stocks.png"), p_dist,
         width = 12, height = 6, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/distribution_five_stocks.png\n"), "green")
}

# ---- 6. correlation matrix ------------------------------------------------------
cat_col("\nCorrelation analysis...\n", "blue")
# Build wide format: rows = dates, cols = tickers
ret_wide <- all_ret %>%
  select(date, ticker, log_ret) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret)

# compute correlation for all available tickers
avail <- intersect(names(stock_info), names(ret_wide))
if (length(avail) >= 2) {
  cor_mat <- cor(ret_wide[, avail, drop = FALSE], use = "pairwise.complete.obs")

  # Correlation heatmap (ggplot)
  cor_long <- cor_mat %>%
    as.data.frame() %>%
    rownames_to_column("Stock1") %>%
    pivot_longer(-Stock1, names_to = "Stock2", values_to = "Correlation")

  p_corr <- ggplot(cor_long, aes(x = Stock1, y = Stock2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), size = 4) +
    scale_fill_viridis_c(low = "blue", high = "red", midpoint = 0) +
    labs(title = "Correlation Matrix — Five Stocks",
         x = "Stock", y = "Stock",
         subtitle = "Daily log returns, pairwise complete observations") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 11))
  fig_dir("correlation_heatmap.png")
  ggsave(fig_dir("correlation_heatmap.png"), p_corr,
         width = 8, height = 6, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/correlation_heatmap.png\n"), "green")

  write_csv(as.data.frame(cor_mat),
            file.path(.project_root(), "output", "tables", "correlation_matrix.csv"))
} else {
  cat_col("  Not enough stocks with valid price data for correlation.\n", "yellow")
}
