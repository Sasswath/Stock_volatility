# ============================================================
# 13_portfolio_analysis.R — Equally weighted five-stock portfolio
# ============================================================
#
# PURPOSE
#   Return to all five stocks and construct an equally weighted
#   (20% each) portfolio.  The purpose is to show how
#   diversification affects portfolio-level risk and return.
#
#   Outputs:
#     -- Daily portfolio returns
#     -- Portfolio mean / variance / sd / annualized vol
#     -- Comparison of individual-stock volatility vs portfolio
#     -- Correlation matrix (already computed in 05)
#     -- Cumulative portfolio performance

section("FIVE-STOCK PORTFOLIO ANALYSIS", "=")

# ---- 1. Load returns for all five stocks -----------------------------------------
stock_prices <- file.path(.project_root(), "data", "processed",
                           "stock_returns_combined.csv")
all_ret <- read_csv(stock_prices, show_col_types = FALSE)

# ensure a tidy format: date, ticker, log_ret
ret_wide <- all_ret %>%
  dplyr::select(date, ticker, log_ret) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret)

# Ensure all five stocks are present
available <- intersect(c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "LT"),
                       names(ret_wide))
missing <- setdiff(c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "LT"),
                   available)
if (length(missing) > 0) {
  cat_col(sprintf("  WARNING: Missing tickers: %s — skipping them from portfolio.\n",
                  paste(missing, collapse = ", ")), "yellow")
}

port_tick <- intersect(c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "LT"),
                       available)

# ---- 2. Daily portfolio return (equally weighted) ---------------------------------
w <- rep(1 / length(port_tick), length(port_tick))  # 20% each
port_return <- as.numeric(as.matrix(ret_wide[, port_tick, drop = FALSE]) %*% w)

port_df <- data.frame(
  date   = ret_wide$date,
  port_ret = port_return
)

# ---- 3. Portfolio statistics -------------------------------------------------------
mean_port <- mean(port_return, na.rm = TRUE)
sd_port   <- sd(port_return, na.rm = TRUE)
var_port  <- sd_port^2
ann_mean  <- mean_port * 252
ann_vol   <- sd_port * sqrt(252)

cat_col(sprintf("\n  Portfolio size: %d stocks (equal weight 20%% each)\n", length(port_tick)), "cyan")
cat_col(sprintf("  Mean daily return : %.4f%%\n", mean_port * 100), "cyan")
cat_col(sprintf("  Portfolio SD      : %.4f%%\n", sd_port * 100), "cyan")
cat_col(sprintf("  Portfolio variance: %.4f\n", var_port), "cyan")
cat_col(sprintf("  Annualised return : %.2f%%\n", ann_mean * 100), "cyan")
cat_col(sprintf("  Annualised volatility: %.2f%%\n", ann_vol * 100), "cyan")

# ---- 4. Individual stock volatilities vs portfolio ---------------------------------
# For every stock in the portfolio, compute daily and annualized vol
stock_stats <- data.frame(
  Ticker = port_tick,
  Company = sapply(port_tick, function(t) stock_info[[t]]$company),
  Sector  = sapply(port_tick, function(t) stock_info[[t]]$sector),
  DailySD = sapply(port_tick, function(t) sd(ret_wide[[t]], na.rm = TRUE)),
  stringsAsFactors = FALSE
)
stock_stats$AnnualSD <- stock_stats$DailySD * sqrt(252)
stock_stats$MeanRet  <- sapply(port_tick, function(t) mean(ret_wide[[t]], na.rm = TRUE))
stock_stats$AnnualMean <- stock_stats$MeanRet * 252

cat_col("\n  Individual stock volatility vs portfolio:", "white")
print_table(stock_stats[, c("Ticker", "Company", "Sector",
                             "DailySD", "AnnualSD", "MeanRet")],
            caption = "Individual stock statistics vs portfolio")

cat_col(sprintf("\n  Portfolio annualized vol: %.2f%%\n", ann_vol * 100), "cyan")
cat_col(sprintf("  Average of individual annualized vols: %.2f%%\n",
                mean(stock_stats$AnnualSD) * 100), "cyan")

# ---- 5. Correlation matrix (already computed in 05; recompute for return) ---------
cat_col("\n  Correlation matrix (five stocks):", "white")
cor_mat <- cor(ret_wide[, port_tick, drop = FALSE], use = "pairwise.complete.obs")
write_csv(as.data.frame(cor_mat),
          file.path(.project_root(), "output", "tables",
                    "portfolio_correlation.csv"))

print_table(as.data.frame(cor_mat), caption = "Correlation matrix")

# ---- 6. Diversification effect -------------------------------------------------------
# The diversification effect is quantified through the portfolio variance formula.
# For an equally weighted portfolio:
#   Var(port) = (1/k^2) * [k * avg_var + k*(k-1)*avg_cov]
#   = (1/k) * avg_var + ((k-1)/k) * avg_cov
# where avg_var is the average individual variance and avg_cov is the
# average pairwise covariance.

k <- length(port_tick)
avg_var <- mean(diag(cor_mat)) * mean(sapply(port_tick, function(t) sd(ret_wide[[t]])^2))
# simpler: average of the individual variances
avg_var_simple <- mean(sapply(port_tick, function(t) sd(ret_wide[[t]], na.rm = TRUE)^2))
avg_cov <- mean(cor_mat[upper.tri(cor_mat)]) * avg_var_simple

# portfolio variance via formula
var_port_formula <- (1/k) * avg_var_simple + ((k - 1) / k) * avg_cov
cat_col(sprintf("\n  Diversification check (formula):", "white"))
cat_col(sprintf("    Average individual variance: %.4f\n", avg_var_simple), "white")
cat_col(sprintf("    Average pairwise correlation: %.4f\n", mean(cor_mat[upper.tri(cor_mat)])), "white")
cat_col(sprintf("    Portfolio variance (empirical): %.4f\n", var_port), "white")
cat_col(sprintf("    Portfolio variance (formula):    %.4f\n", var_port_formula), "white")

# ---- 7. Cumulative performance -----------------------------------------------------
cum_port <- cumprod(1 + port_return) * 100

p_port <- ggplot(port_df, aes(x = date, y = cum_port)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(title = "Cumulative Portfolio Performance (Equally Weighted)",
       subtitle = sprintf("Five stocks, 20%% each, %s–%s",
                          min(port_df$date), max(port_df$date)),
       x = "Date", y = "Cumulative return index (base 100)") +
  theme_minimal(base_size = 13)

fig_dir("portfolio_performance.png")
ggsave(fig_dir("portfolio_performance.png"), p_port,
       width = 11, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/portfolio_performance.png\n"), "green")

# ---- 8. Diversification conclusion ---------------------------------------------------
cat_col("\n  DIVERSIFICATION CONCLUSION:", "white")
cat_col("    Portfolio vol (%.2f%%) vs average individual vol (%.2f%%)\n", "white")
cat_col("    — the difference reflects the correlation between the stocks.\n", "white")
cat_col("    If correlations were < 1, diversification would reduce\n", "white")
cat_col("    portfolio volatility below the average individual volatility.\n", "white")
cat_col("    If correlations were close to 1, diversification would have\n", "white")
cat_col("    little effect.\n", "white")
cat_col("    We do NOT claim diversification ALWAYS reduces risk —\n", "white")
cat_col("    it reduces risk only when the constituent assets are NOT\n", "white")
cat_col("    perfectly correlated.\n", "white")
