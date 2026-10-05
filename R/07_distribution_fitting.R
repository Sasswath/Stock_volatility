# ============================================================
# 07_distribution_fitting.R — Fit Normal and Student-t to
#                              representative stock returns
# ============================================================
#
# PURPOSE
#   This is where the detailed statistical analysis of the
#   REPRESENTATIVE stock begins.  Both the Normal and
#   Student's t distributions are fitted by Maximum Likelihood
#   Estimation (MLE) to the representative stock's daily log
#   returns, and the fitted densities are overlaid on the
#   histogram.
#
#   Normal model:  location = mean, scale = sd
#   Student-t     : location, scale, df (all estimated by MLE)
#
#   Outputs: density overlays, Q-Q plots, CDF comparisons,
#            log-likelihood, AIC, BIC, and a summary table.

# ---- acquire representative returns (re-source if needed) --------------------
if (!exists(".representative_ticker", envir = .GlobalEnv, inherits = FALSE)) {
  # Fallback: auto-select if not already set (for direct runs)
  if (!file.exists(file.path(.project_root(), "output",
                              "tables", "representative_stock_selection.csv"))) {
    stop("Representative stock not selected. Run 06_representative_stock.R first.")
  }
  rep_sel <- read_csv(file.path(.project_root(), "output",
                                "tables", "representative_stock_selection.csv"),
                      show_col_types = FALSE)
  .representative_ticker <- rep_sel$Ticker[1]
}
rep_ticker <- .representative_ticker

cat_col(sprintf("\nRepresentative ticker: %s\n", rep_ticker), "magenta")

rep_ret <- readRDS(file.path(.project_root(), "output",
                              "processed", "representative_returns.rds"))

r <- rep_ret$log_ret
n <- length(r)

section("DISTRIBUTION FITTING FOR REPRESENTATIVE STOCK", "=")

# Print a quick description of the returns series
cat_col(sprintf("  Series    : daily log returns (%s)\n", rep_ticker), "cyan")
cat_col(sprintf("  N         : %d\n", n), "cyan")
cat_col(sprintf("  Mean      : %.4f%%\n", mean(r) * 100), "cyan")
cat_col(sprintf("  SD        : %.4f%%\n", sd(r) * 100), "cyan")
cat_col(sprintf("  Skewness  : %.4f\n", moments::skewness(r)), "cyan")
cat_col(sprintf("  Kurtosis  : %.4f (excess %.4f)\n",
              moments::kurtosis(r), moments::kurtosis(r) - 3), "cyan")

# ---- 1. NORMAL DISTRIBUTION (MLE) --------------------------------------------
cat_col("\n" , "blue")
section("1. NORMAL DISTRIBUTION — MLE", "=")

# Under the Normal model, MLE of mu and sigma^2 are simply the sample
# mean and sample variance (with n-1 denominator, unbiased).
mu_hat    <- mean(r)
sigma_hat <- sd(r)

# Log-likelihood for the Normal model
loglik_normal <- sum(dnorm(r, mean = mu_hat, sd = sigma_hat, log = TRUE))

# AIC and BIC
k_normal <- 2   # number of estimated parameters (mu, sigma)
aic_normal <- -2 * loglik_normal + 2 * k_normal
bic_normal <- -2 * loglik_normal + k_normal * log(n)

cat_col(sprintf("  mu (mean)  : %.6f\n", mu_hat), "cyan")
cat_col(sprintf("  sigma (sd) : %.6f\n", sigma_hat), "cyan")
cat_col(sprintf("  log-likelihood : %.4f\n", loglik_normal), "cyan")
cat_col(sprintf("  AIC             : %.4f\n", aic_normal), "cyan")
cat_col(sprintf("  BIC             : %.4f\n", bic_normal), "cyan")
cat_col("  Note: AIC/BIC are for RELATIVE comparison only; one model is\n", "white")
cat_col("  not necessarily 'true'.  A lower AIC/BIC only says the model\n", "white")
cat_col("  that balances fit and complexity better for THIS data.        \n", "white")

# ---- 2. STUDENT'S t DISTRIBUTION (MLE) ---------------------------------------
cat_col("\n" , "blue")
section("2. STUDENT'S t DISTRIBUTION — MLE", "=")

# Student's t density:
#   f(x|loc, scale, df) =
#     gamma((df+1)/2) / ( gamma(df/2) * sqrt(df*pi) * scale )
#     * ( 1 + (1/df) * ((x - loc)/scale)^2 ) ^ (-(df+1)/2)
#
# We fit with a manual MLE approach using optim since fitdist's "t"
# distribution uses R's dt() which only has df (no location/scale).
neg_loglik_t <- function(par) {
  mu <- par[1]; sigma <- exp(par[2]); df <- exp(par[3])
  -sum(dt((r - mu) / sigma, df = df, log = TRUE) - log(sigma))
}
fit0 <- optim(c(mu_hat, log(sigma_hat), log(4)),
              neg_loglik_t, method = "L-BFGS-B")

# Extract MLE estimates
mu_t     <- fit0$par[1]
scale_t  <- exp(fit0$par[2])
df_t     <- exp(fit0$par[3])

# Log-likelihood
loglik_t <- -fit0$value

# Save t-fit params for downstream scripts
t_params <- data.frame(mu = mu_t, scale = scale_t, df = df_t)
write_csv(t_params,
          file.path(.project_root(), "output", "tables", "t_fit_params.csv"))

# AIC and BIC
k_t <- 3   # location, scale, df
aic_t <- -2 * loglik_t + 2 * k_t
bic_t <- -2 * loglik_t + k_t * log(n)

cat_col(sprintf("  location (mu): %.6f\n", mu_t), "cyan")
cat_col(sprintf("  scale      : %.6f\n", scale_t), "cyan")
cat_col(sprintf("  df         : %.4f\n", df_t), "cyan")
cat_col(sprintf("  log-likelihood : %.4f\n", loglik_t), "cyan")
cat_col(sprintf("  AIC             : %.4f\n", aic_t), "cyan")
cat_col(sprintf("  BIC             : %.4f\n", bic_t), "cyan")

# ---- 3. FITTED DENSITY OVERLAY ------------------------------------------------
cat_col("\n" , "blue")
section("3. DENSITY OVERLAY — HISTOGRAM + FITTED MODELS", "=")

# Histogram of the returns
hist_dat <- data.frame(
  return = r,
  model  = "Historical"
)

p_hist <- ggplot() +
  geom_histogram(data = hist_dat, aes(x = return), bins = 80,
                 fill = "grey80", color = "black", alpha = 0.7) +
  stat_function(fun = function(x) dnorm(x, mu_hat, sigma_hat),
                color = "red", linewidth = 1.2, size = 1.2) +
  stat_function(fun = function(x) dt((x - mu_t) / scale_t, df_t) / scale_t,
                color = "blue", linewidth = 1.2, size = 1.2) +
  labs(title = "Historical Returns vs Fitted Distributions",
       subtitle = sprintf("Representative stock: %s  |  n = %d", rep_ticker, n),
       x = "Daily log return", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top") +
  labs(title = "Historical Returns vs Fitted Distributions",
       subtitle = sprintf("Representative stock: %s  |  n = %d", rep_ticker, n),
       x = "Daily log return", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

fig_dir("density_fitted_models.png")
ggsave(fig_dir("density_fitted_models.png"), p_hist,
       width = 11, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/density_fitted_models.png\n"), "green")

# ---- 4. Q-Q PLOTS ---------------------------------------------------------------
cat_col("\n" , "blue")
section("4. Q-Q PLOTS", "=")

# Normal Q-Q plot
p_qq_normal <- qqnorm(r, plot.it = FALSE)
p_qq_normal <-    ggplot(data.frame(
  theoretical = sort(qnorm(ppoints(n), mean = mu_hat, sd = sigma_hat)),
  sample      = sort(r)
), aes(x = theoretical, y = sample)) +
  geom_point(color = "grey40", alpha = 0.6, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Q-Q Plot vs Normal Distribution",
       subtitle = sprintf("Representative stock: %s (n = %d)", rep_ticker, n),
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 13)

fig_dir("qqplot_normal.png")
ggsave(fig_dir("qqplot_normal.png"), p_qq_normal,
       width = 8, height = 7, dpi = 300)
cat_col(sprintf("  Saved: output/figures/qqplot_normal.png\n"), "green")

# Student-t Q-Q plot
# Compute theoretical quantiles of the fitted t distribution
pts <- sort(qt(ppoints(n), df = df_t) * scale_t + mu_t)
p_qq_t <- ggplot(data.frame(
  theoretical = pts,
  sample      = sort(r)
), aes(x = theoretical, y = sample)) +
  geom_point(color = "grey40", alpha = 0.6, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "Q-Q Plot vs Student-t Distribution",
       subtitle = sprintf("Representative stock: %s (n = %d, df = %.2f)", rep_ticker, n, df_t),
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 13)

fig_dir("qqplot_student_t.png")
ggsave(fig_dir("qqplot_student_t.png"), p_qq_t,
       width = 8, height = 7, dpi = 300)
cat_col(sprintf("  Saved: output/figures/qqplot_student_t.png\n"), "green")

# ---- 5. CDF COMPARISON -----------------------------------------------------------
cat_col("\n" , "blue")
section("5. CDF COMPARISON", "=")

x_grid <- seq(min(r), max(r), length.out = 500)

cdf_normal <- data.frame(x = x_grid, y = pnorm(x_grid, mu_hat, sigma_hat))
cdf_t      <- data.frame(x = x_grid, y = pt((x_grid - mu_t) / scale_t, df_t))

cdf_df <- rbind(
  data.frame(x = cdf_normal$x, y = cdf_normal$y, model = "Normal"),
  data.frame(x = cdf_t$x,      y = cdf_t$y,      model = "Student-t")
)
cdf_hist <- data.frame(x = r, y = ecdf(r)(r), model = "Historical")
cdf_hist <- cdf_hist[!is.na(cdf_hist$y), ]

p_cdf <- ggplot(cdf_hist, aes(x = x, y = y)) +
  geom_line(color = "black", alpha = 0.5, linewidth = 0.8) +
  geom_line(data = cdf_df, aes(color = model), linewidth = 1.1, show.legend = TRUE) +
  scale_color_manual(name = "Distribution",
                     values = c("Normal" = "red",
                                "Student-t" = "blue"),
                     labels = c("Normal", "Student-t")) +
  labs(title = "Empirical CDF vs Fitted CDFs",
       subtitle = sprintf("Representative stock: %s (n = %d)", rep_ticker, n),
       x = "Daily log return", y = "Cumulative probability") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("cdf_comparison.png")
ggsave(fig_dir("cdf_comparison.png"), p_cdf,
       width = 11, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/cdf_comparison.png\n"), "green")

# ---- 6. MODEL SUMMARY TABLE ------------------------------------------------------
cat_col("\n" , "blue")
section("6. MODEL SUMMARY TABLE", "=")

model_summary <- data.frame(
  Model    = c("Normal", "Student's t"),
  Parameters = c("μ, σ (2)", "μ, σ, ν (3)"),
  LogLik   = c(round(loglik_normal, 4), round(loglik_t, 4)),
  AIC      = c(round(aic_normal, 4), round(aic_t, 4)),
  BIC      = c(round(bic_normal, 4), round(bic_t, 4))
)

write_csv(model_summary,
          file.path(.project_root(), "output", "tables",
                    "model_comparison_summary.csv"))

print_table(model_summary, caption = "Model comparison — Normal vs Student's t")

cat_col("\n  LOG-LIKELIHOOD:  ", "white")
cat_col("The likelihood is the probability of the observed data\n", "white")
cat_col("under each model.  Higher = better fit.      \n", "white")
cat_col("\n  AIC / BIC:      \n", "white")
cat_col("Lower = relatively better fit after penalising\n", "white")
cat_col("extra parameters (Student-t has 1 more).\n", "white")
cat_col("\n  INTERPRETATION:\n", "white")
card <- ifelse(aic_t < aic_normal, "The Student-t model has a lower AIC/BIC,",
               "The Normal model has a lower AIC/BIC,")
cat_col(sprintf("  %s the Student-t model is selected as the preferred\n  empirical fit for tail behaviour.\n", card), "white")
cat_col("  (In this project we report BOTH and interpret the\n", "white")
cat_col("   difference, without declaring one 'correct'.)\n", "white")
