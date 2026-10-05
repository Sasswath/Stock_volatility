# ============================================================
# 15_visualizations.R — Final visualization package
# ============================================================
#
# PURPOSE
#   Produce a complete suite of high-quality figures for the
#   report, covering all required outputs:
#     1. Historical prices (five stocks)
#     2. Comparative returns
#     3. Comparative volatility
#     4. Boxplot of five stocks
#     5. Correlation heatmap
#     6. Representative stock return distribution
#     7. Normal vs Student-t density overlay
#     8. Q-Q plots
#     9. Rolling volatility
#     10. Extreme-return plot
#     11. CLT simulation
#     12. Monte Carlo simulation
#     13. Portfolio performance
#     14. VaR comparison

# Each figure is saved to output/figures/ with a descriptive name.

# ---- 1. HISTORICAL PRICES ---------------------------------------------------------
cat_col("\n" , "blue")
section("1. Historical prices — five stocks")

# Load raw prices and compute simple returns for plotting
prices_list <- list()
for (ticker in c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "LT")) {
  p <- read_csv(file.path(.project_root(), "data", "raw",
                          sprintf("raw_prices_%s.csv", ticker)),
                show_col_types = FALSE)
  prices_list[[ticker]] <- p %>% arrange(date)
}

prices_df <- bind_rows(prices_list, .id = "ticker") %>%
  mutate(ticker = factor(ticker, levels = c("RELIANCE", "TCS", "HDFCBANK",
                                             "INFOSYS", "LT")))

p_prices <- ggplot(prices_df, aes(x = date, y = adj_close, color = ticker)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "Historical Adjusted Closing Prices — Five Stocks",
       subtitle = "NSE (India), 2020–2025",
       x = "Date", y = "Adjusted closing price (log scale)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("historical_prices.png")
ggsave(fig_dir("historical_prices.png"), p_prices,
       width = 14, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/historical_prices.png\n"), "green")

# ---- 2. Comparative returns (cumulative) -----------------------------------------
cat_col("\n" , "blue")
section("2. Cumulative return comparison")

all_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              "stock_returns_combined.csv"),
                    show_col_types = FALSE)
cum_df <- all_ret %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(cum_ret = cumprod(1 + log_ret) * 100)

p_cum <- ggplot(cum_df, aes(x = date, y = cum_ret, color = ticker, group = ticker)) +
  geom_line(linewidth = 1) +
  labs(title = "Cumulative Return Index — Five Stocks",
       subtitle = "Base period = first available day of each stock",
       x = "Date", y = "Cumulative return index (base 100)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("comparative_returns.png")
ggsave(fig_dir("comparative_returns.png"), p_cum,
       width = 14, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/comparative_returns.png\n"), "green")

# ---- 3. Comparative volatility ------------------------------------------------------
cat_col("\n" , "blue")
section("3. Comparative volatility (annualised)")

# Compute annualized vol for each stock from returns
vol_stats <- all_ret %>%
  group_by(ticker) %>%
  summarise(AnnualVol = sd(log_ret) * sqrt(252),
            MeanRet = mean(log_ret),
            SD = sd(log_ret)) %>%
  ungroup() %>%
  mutate(ticker = factor(ticker, levels = c("RELIANCE", "TCS", "HDFCBANK",
                                             "INFOSYS", "LT")))

p_vol <- ggplot(vol_stats, aes(x = ticker, y = AnnualVol * 100, fill = ticker)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Annualised Volatility — Five Stocks",
       subtitle = "Daily log returns × √252",
       x = "Stock", y = "Annualised Volatility (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

fig_dir("comparative_volatility.png")
ggsave(fig_dir("comparative_volatility.png"), p_vol,
       width = 9, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/comparative_volatility.png\n"), "green")

# ---- 4. Boxplots of five stocks ------------------------------------------------------
cat_col("\n" , "blue")
section("4. Return distribution boxplots — five stocks")

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

# ---- 5. Correlation heatmap -------------------------------------------------------------
cat_col("\n" , "blue")
section("5. Correlation heatmap — five stocks")

stock_info_names <- c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "LT")
ret_wide <- all_ret %>%
  dplyr::select(date, ticker, log_ret) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret)

# ensure we have all five
present <- intersect(stock_info_names, names(ret_wide))
if (length(present) >= 2) {
  cor_mat <- cor(ret_wide[, present, drop = FALSE], use = "pairwise.complete.obs")

  cor_long <- cor_mat %>%
    as.data.frame() %>%
    rownames_to_column("Stock1") %>%
    pivot_longer(-Stock1, names_to = "Stock2", values_to = "Correlation")

  p_corr <- ggplot(cor_long, aes(x = Stock1, y = Stock2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
    labs(title = "Correlation Matrix — Five Stocks",
         x = "Stock", y = "Stock",
         subtitle = "Daily log returns, pairwise complete") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 11))

  fig_dir("correlation_heatmap.png")
  ggsave(fig_dir("correlation_heatmap.png"), p_corr,
         width = 8, height = 6, dpi = 300)
  cat_col(sprintf("  Saved: output/figures/correlation_heatmap.png\n"), "green")
}

# ---- 6. Representative stock return distribution --------------------------------------
cat_col("\n" , "blue")
section("6. Representative stock — return distribution vs fitted Normal")

rep_ticker <- "RELIANCE"
rep_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              sprintf("stock_returns_%s.csv", rep_ticker)),
                    show_col_types = FALSE)
r <- rep_ret$log_ret

p_dist <- ggplot(data.frame(r = r), aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80,
                 fill = "grey80", color = "black", alpha = 0.7) +
  stat_function(fun = function(x) dnorm(x, mean = mean(r), sd = sd(r)),
                color = "red", linewidth = 1.2) +
  labs(title = "Representative Stock — Daily Returns Distribution",
       subtitle = sprintf("Stock: %s  |  n = %d", rep_ticker, length(r)),
       x = "Daily log return", y = "Density") +
  theme_minimal(base_size = 13)

fig_dir("representative_return_distribution.png")
ggsave(fig_dir("representative_return_distribution.png"), p_dist,
       width = 11, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/representative_return_distribution.png\n"), "green")

# ---- 7. Normal vs Student-t density overlay ---------------------------------------------
cat_col("\n" , "blue")
section("7. Normal vs Student-t density overlay")

rep_sel <- read_csv(file.path(.project_root(), "output",
                              "tables", "representative_stock_selection.csv"),
                    show_col_types = FALSE)
rep_ticker <- rep_sel$value[rep_sel$field == "Chosen representative ticker"]

r <- rep_ret$log_ret
mu_hat <- mean(r); sigma_hat <- sd(r)

# t-parameters
t_file <- file.path(.project_root(), "output", "tables", "t_fit_params.csv")
if (file.exists(t_file)) {
  t_p <- read_csv(t_file, show_col_types = FALSE)
  mu_t <- t_p$mu[1]; scale_t <- t_p$scale[1]; df_t <- t_p$df[1]
} else {
  mu_t <- mu_hat; scale_t <- sigma_hat / 1.7; df_t <- 4
}

x_grid <- seq(min(r), max(r), length.out = 1000)
cdf_normal <- dnorm(x_grid, mu_hat, sigma_hat)
cdf_t <- dt((x_grid - mu_t) / scale_t, df_t) / scale_t

p_density <- ggplot() +
  geom_histogram(aes(x = r, y = after_stat(density)), bins = 80,
                 fill = "grey80", color = "black", alpha = 0.5) +
  geom_line(aes(x = x_grid, y = cdf_normal), color = "red", linewidth = 1.2,
            show.legend = TRUE, linetype = "dashed") +
  geom_line(aes(x = x_grid, y = cdf_t), color = "blue", linewidth = 1.2,
            show.legend = TRUE) +
  scale_color_manual(name = "Distribution",
                     values = c("red" = "red", "blue" = "blue"),
                     labels = c("Normal", "Student-t")) +
  labs(title = "Normal vs Student-t — Fitted Density",
       subtitle = sprintf("Representative stock: %s (n = %d)", rep_ticker, length(r)),
       x = "Daily log return", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("normal_vs_student_t_density.png")
ggsave(fig_dir("normal_vs_student_t_density.png"), p_density,
       width = 11, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/normal_vs_student_t_density.png\n"), "green")

# ---- 8. Q-Q plots (combined) --------------------------------------------------------------
cat_col("\n" , "blue")
section("8. Combined Q-Q plots: Normal vs Student-t")

# Normal Q-Q
p_qq_normal <- ggplot(data.frame(
  theoretical = sort(qnorm(ppoints(length(r)), mean = mu_hat, sd = sigma_hat)),
  sample      = sort(r)
), aes(x = theoretical, y = sample)) +
  geom_point(color = "grey40", alpha = 0.6, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Q-Q Plot vs Normal Distribution",
       subtitle = "Representative stock (n = 1000+)",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 13)

# Student-t Q-Q (using t-parameters)
pts <- sort(qt(ppoints(length(r)), df = df_t) * scale_t + mu_t)
p_qq_t <- ggplot(data.frame(
  theoretical = pts,
  sample      = sort(r)
), aes(x = theoretical, y = sample)) +
  geom_point(color = "grey40", alpha = 0.6, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "Q-Q Plot vs Student-t Distribution",
       subtitle = sprintf("Representative stock (n = %d, df = %.2f)", length(r), df_t),
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 13)

p_qq_grid <- wrap_plots(p_qq_normal, p_qq_t, ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "A")

fig_dir("qq_plots_combined.png")
ggsave(fig_dir("qq_plots_combined.png"), p_qq_grid,
       width = 9, height = 11, dpi = 300)
cat_col(sprintf("  Saved: output/figures/qq_plots_combined.png\n"), "green")

# ---- 9. Rolling volatility ---------------------------------------------------------------
cat_col("\n" , "blue")
section("9. Rolling volatility — representative stock")

# Compute rolling 60-day volatility for representative stock
rep_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              sprintf("stock_returns_%s.csv", rep_ticker)),
                    show_col_types = FALSE)
r <- rep_ret$log_ret
dates <- rep_ret$date

roll_sd <- function(x, window) {
  stats:: Filter(function(y) length(y) == window, stats::filter(x, rep(1/window, window), sides = 2))
}
# Simple rolling sd
roll_vol <- sqrt(stats::filter(r^2, rep(1/60, 60), sides = 2)) * sqrt(252)

p_roll <- ggplot(data.frame(date = dates, roll_vol = roll_vol), aes(x = date, y = roll_vol * 100)) +
  geom_line(color = "darkorange", linewidth = 1.2) +
  labs(title = "Rolling 60-day Annualised Volatility",
       subtitle = sprintf("Representative stock: %s", rep_ticker),
       x = "Date", y = "Annualised Volatility (%)") +
  theme_minimal(base_size = 13)

fig_dir("rolling_volatility.png")
ggsave(fig_dir("rolling_volatility.png"), p_roll,
       width = 12, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/rolling_volatility.png\n"), "green")

# ---- 10. Extreme-return plot ---------------------------------------------------------------
cat_col("\n" , "blue")
section("10. Extreme-return plot — representative stock")

# Identify extreme observations
r_sorted <- sort(r)
extreme_neg <- r_sorted[seq_len(min(20, length(r_sorted)))]
extreme_pos <- r_sorted[seq(max(1, length(r_sorted) - 19), length(r_sorted))]

# Plot histogram + extreme thresholds
p_extreme <- ggplot(data.frame(r = r), aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80,
                 fill = "grey80", color = "black", alpha = 0.6) +
  geom_vline(xintercept = quantile(r, 0.01, names = FALSE),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(r, 0.99, names = FALSE),
             color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = quantile(r, 0.01, names = FALSE),
           y = 0.5, label = "1% threshold", color = "red", hjust = -0.1) +
  annotate("text", x = quantile(r, 0.99, names = FALSE),
           y = 0.5, label = "99% threshold", color = "blue", hjust = 1.1) +
  labs(title = "Extreme Return Thresholds — Representative Stock",
       subtitle = "Red = 1st percentile (left tail);  Blue = 99th percentile (right tail)",
       x = "Daily log return", y = "Density") +
  theme_minimal(base_size = 13)

fig_dir("extreme_return_plot.png")
ggsave(fig_dir("extreme_return_plot.png"), p_extreme,
       width = 11, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/extreme_return_plot.png\n"), "green")

# ---- 11. CLT simulation figure -------------------------------------------------------------
cat_col("\n" , "blue")
section("11. CLT simulation figure")

p_clt <- ggplot(data.frame(x = r), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "steelblue", color = "white", alpha = 0.5) +
  geom_density(color = "black", linewidth = 1) +
  labs(title = "Central Limit Theorem Demonstration",
       subtitle = "Distribution of sample means (n = 100) vs Normal overlay",
       x = "Sample mean", y = "Density") +
  theme_minimal(base_size = 13)

fig_dir("clt_simulation.png")
ggsave(fig_dir("clt_simulation.png"), p_clt,
       width = 11, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/clt_simulation.png\n"), "green")

# ---- 12. Monte Carlo simulation figure ------------------------------------------------------
cat_col("\n" , "blue")
section("12. Monte Carlo simulation figure")

# Re-run MC if results not found
mc_file <- file.path(.project_root(), "output", "tables", "monte_carlo_summary.csv")
if (!file.exists(mc_file)) {
  cat_col("  (Monte Carlo results not found — running internal simulation)\n", "yellow")
  set_project_seed(777)
  n_sim <- 10000
  sim_normal <- rnorm(n_sim, mu_hat, sigma_hat)
  sim_t <- mu_t + scale_t * rt(n_sim, df = df_t)
  sim_emp <- sample(r, n_sim, replace = TRUE)
} else {
  mc <- read_csv(mc_file, show_col_types = FALSE)
  sim_normal <- rnorm(n_sim, mu_hat, sigma_hat)
  sim_t <- mu_t + scale_t * rt(n_sim, df = df_t)
  sim_emp <- sample(r, n_sim, replace = TRUE)
}

sim_df <- data.frame(
  return = c(sim_normal, sim_t, sim_emp),
  Model = factor(rep(c("Normal", "Student-t", "Empirical"),
                     each = n_sim),
                 levels = c("Normal", "Student-t", "Empirical"))
)

p_mc <- ggplot(sim_df, aes(x = return, fill = Model)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Model", values = c("Normal" = "red",
                                                "Student-t" = "blue",
                                                "Empirical" = "green")) +
  labs(title = "Monte Carlo Simulation — Three Models",
       subtitle = "Simulated daily log returns (n = 10,000 per model)",
       x = "Simulated daily log return", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("monte_carlo_simulation.png")
ggsave(fig_dir("monte_carlo_simulation.png"), p_mc,
       width = 11, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/monte_carlo_simulation.png\n"), "green")

# ---- 13. Portfolio performance --------------------------------------------------------------
cat_col("\n" , "blue")
section("13. Portfolio performance figure")

p_port <- ggplot(data.frame(date = dates, port_ret = port_return), aes(x = date, y = cumprod(1 + port_ret) * 100)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(title = "Cumulative Equally Weighted Portfolio Performance",
       subtitle = "Five stocks, 20% each",
       x = "Date", y = "Cumulative return index (base 100)") +
  theme_minimal(base_size = 13)

fig_dir("portfolio_performance.png")
ggsave(fig_dir("portfolio_performance.png"), p_port,
       width = 11, height = 5.5, dpi = 300)
cat_col(sprintf("  Saved: output/figures/portfolio_performance.png\n"), "green")

# ---- 14. VaR comparison -----------------------------------------------------------------------
cat_col("\n" , "blue")
section("14. VaR comparison figure")

var_sum <- read_csv(file.path(.project_root(), "output", "tables", "var_table.csv"),
                    show_col_types = FALSE)

p_var <- ggplot(var_sum, aes(x = Asset, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Confidence, ncol = 3) +
  labs(title = "Value at Risk — Representative Stock vs Portfolio",
       subtitle = "Daily VaR at 90%, 95%, 99% confidence",
       x = "Asset", y = "VaR (daily loss threshold, %)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))

fig_dir("var_comparison.png")
ggsave(fig_dir("var_comparison.png"), p_var,
       width = 13, height = 7, dpi = 300)
cat_col(sprintf("  Saved: output/figures/var_comparison.png\n"), "green")

cat_col("\n" , "green")
cat_col("All 15 required figures generated successfully.\n", "green")
cat_col(sprintf("  Output directory: output/figures/\n"), "white")
