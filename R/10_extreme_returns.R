# ============================================================
# 10_extreme_returns.R — Extreme-return analysis of the
#                          representative stock
# ============================================================
#
# PURPOSE
#   Identify and quantify extreme returns in the representative
#   stock's daily log returns:
#
#     -- Percentiles: 1st, 5th, 95th, 99th
#     -- Largest positive and negative returns
#     -- Extreme positive and negative observations
#     -- Compare observed tails with the Normal and Student-t
#         fitted models (by simulation).
#
#   Explanation: extreme returns matter because they drive
#   risk management, capital reserves, and VaR.  Financial
#   returns are notorious for their fat tails, which the
#   Normal model underestimates.

section("EXTREME RETURNS ANALYSIS", "=")
cat_col(sprintf("Representative stock: %s\n", rep_ticker), "magenta")

r <- rep_ret$log_ret
n <- length(r)

# ---- fitted parameters from distribution-fitting script ------------------
# These must be available; if not, we recompute from the representative data
mu_hat    <- mean(r)
sigma_hat <- sd(r)

# Try to load t-fit parameters from the model comparison table
t_fit_file <- file.path(.project_root(), "output", "tables", "t_fit_params.csv")
if (file.exists(t_fit_file)) {
  t_p <- read_csv(t_fit_file, show_col_types = FALSE)
  mu_t    <- t_p$mu[1]
  scale_t <- t_p$scale[1]
  df_t    <- t_p$df[1]
} else {
  mu_t    <- mu_hat
  scale_t <- sigma_hat / 1.7
  df_t    <- 4
}

# ---- 1. Percentile analysis ----------------------------------------------------
cat_col("\n" , "blue")
section("1. Percentiles", "=")

# ---- 1. Percentile analysis ----------------------------------------------------
cat_col("\n" , "blue")
section("1. Percentiles", "=")

p1 <- quantile(r, 0.01, na.rm = TRUE)
p5 <- quantile(r, 0.05, na.rm = TRUE)
p50 <- quantile(r, 0.50, na.rm = TRUE)
p95 <- quantile(r, 0.95, na.rm = TRUE)
p99 <- quantile(r, 0.99, na.rm = TRUE)

cat_col(sprintf("  1st percentile (tail risk threshold): %+.4f%%\n", p1 * 100), "cyan")
cat_col(sprintf("  5th percentile (tail risk threshold): %+.4f%%\n", p5 * 100), "cyan")
cat_col(sprintf("  50th percentile (median)              : %+.4f%%\n", p50 * 100), "cyan")
cat_col(sprintf("  95th percentile (tail upside): %+.4f%%\n", p95 * 100), "cyan")
cat_col(sprintf("  99th percentile (tail upside): %+.4f%%\n", p99 * 100), "cyan")

# ---- 2. Largest positive and negative returns ------------------------------------
cat_col("\n" , "blue")
section("2. Largest positive and negative returns", "=")

top_n <- 10
top_pos <- sort(r, decreasing = TRUE)[seq_len(min(top_n, n))]
top_neg <- sort(r, decreasing = FALSE)[seq_len(min(top_n, n))]

cat_col(sprintf("  Largest positive returns (n = %d):\n", top_n), "cyan")
for (v in top_pos) cat_col(sprintf("    %+.4f%%  (%.4f in z-units)\n", v * 100, (v - mean(r)) / sd(r)), "green")

cat_col(sprintf("  Largest negative returns (n = %d):\n", top_n), "cyan")
for (v in top_neg) cat_col(sprintf("    %+.4f%%  (%.4f in z-units)\n", v * 100, (v - mean(r)) / sd(r)), "red")

# Also identify extremes using the fitted Normal model
extreme_negative <- r[r < quantile(r, 0.005, na.rm = TRUE)]
extreme_positive <- r[r > quantile(r, 0.995, na.rm = TRUE)]

cat_col(sprintf("\n  Extreme negative observations (|z| > 3): %d", length(extreme_negative)), "cyan")
cat_col(sprintf("  Extreme positive observations (|z| > 3): %d\n", length(extreme_positive)), "cyan")

# ---- 3. Compare observed tails with fitted Normal and Student-t models ------------
cat_col("\n" , "blue")
section("3. Tail comparison: Observed vs Fitted Models", "=")

# Observed quantile proportions
obs_p1 <- mean(r <= p1)
obs_p5 <- mean(r <= p5)
obs_p95 <- mean(r <= p95)
obs_p99 <- mean(r <= p99)

# Fitted model proportions
exp_below_p1_normal <- pnorm(p1, mu_hat, sigma_hat)
exp_below_p5_normal <- pnorm(p5, mu_hat, sigma_hat)
exp_below_p99_normal <- pnorm(p99, mu_hat, sigma_hat)

exp_below_p1_t <- pt((p1 - mu_t) / scale_t, df = df_t)
exp_below_p5_t <- pt((p5 - mu_t) / scale_t, df = df_t)
exp_below_p99_t <- pt((p99 - mu_t) / scale_t, df = df_t)

tail_table <- data.frame(
  Metric = c("1st percentile (losses)", "5th percentile (losses)",
             "99th percentile (gainers)", "99.5th percentile (gains)"),
  Observed = c(round(obs_p1 * 100, 2), round(obs_p5 * 100, 2),
               round(obs_p95 * 100, 2), round(obs_p99 * 100, 2)),
  Normal_Pct = c(round(exp_below_p1_normal * 100, 2),
                 round(exp_below_p5_normal * 100, 2),
                 round(exp_below_p99_normal * 100, 2), NA),
  Studentt_Pct = c(round(exp_below_p1_t * 100, 2),
                   round(exp_below_p5_t * 100, 2),
                   round(exp_below_p99_t * 100, 2), NA)
)

cat_col("\n  Tail comparison (Empirical vs Model):", "white")
print_table(tail_table, caption = "Tail behaviour — observed vs fitted models")

# ---- 4. Simulation-based tail comparison -----------------------------------------
cat_col("\n" , "blue")
section("4. Simulation-based tail comparison", "=")

set_project_seed(999)
n_sim <- 50000

sim_normal <- rnorm(n_sim, mean = mu_hat, sd = sigma_hat)
sim_t <- mu_t + scale_t * rt(n_sim, df = df_t)

# Empirical 1% and 5% quantiles from the historical data
hist_p1 <- quantile(r, 0.01, na.rm = TRUE)
hist_p5 <- quantile(r, 0.05, na.rm = TRUE)

# Fitted Normal model
normal_p1 <- quantile(sim_normal, 0.01)
normal_p5 <- quantile(sim_normal, 0.05)

# Fitted Student-t
t_p1 <- quantile(sim_t, 0.01)
t_p5 <- quantile(sim_t, 0.05)

tail_sim_table <- data.frame(
  Threshold = c("1%", "5%"),
  Historical = c(round(hist_p1 * 100, 4), round(hist_p5 * 100, 4)),
  Normal = c(round(normal_p1 * 100, 4), round(normal_p5 * 100, 4)),
  Student_t = c(round(t_p1 * 100, 4), round(t_p5 * 100, 4))
)

cat_col("\n  Simulated 1% and 5% tail thresholds (simulated from fitted models):", "white")
print_table(tail_sim_table, caption = "Simulated tail thresholds — Historical vs Fitted models")

# Interpretation
cat_col("\n  INTERPRETATION:\n", "white")
cat_col("  The Normal model and the Student-t model will produce DIFFERENT\n", "white")
cat_col("  tail estimates.  This difference is exactly what VaR analysis\n", "white")
cat_col("  in script 14 will explore further.\n", "white")
cat_col("  In general, heavy-tailed models (Student-t) assign MORE\n", "white")
cat_col("  probability to extreme losses than the Normal model.\n", "white")
