# ============================================================
# 12_monte_carlo.R — Monte Carlo simulation of returns under
#                         different models
# ============================================================
#
# PURPOSE
#   Simulate returns under three different models for the
#   representative stock:
#
#     -- Model 1: Normal model (mu_hat, sigma_hat)
#     -- Model 2: Student-t model (mu_t, sigma_t, df_t)
#     -- Model 3: Empirical / historical resampling (block/permutation)
#
#   Compare:
#     -- Mean
#     -- Standard deviation
#     -- 1% and 5% percentiles
#     -- Probability of loss
#     -- Extreme-loss frequency
#
#   IMPORTANT: Simulated results are MODEL-BASED.  They are NOT
#   guaranteed future predictions.  They describe the behaviour
#   implied by each fitted model, under its assumptions.

section("MONTE CARLO SIMULATION", "=")
cat_col(sprintf("Representative stock: %s\n", rep_ticker), "magenta")

r <- rep_ret$log_ret
n <- length(r)

# ---- fitted parameters (reuse from earlier scripts) ------------------------------
# These are defined in script 07; recreating here for standalone robustness.
mu_hat    <- mean(r)
sigma_hat <- sd(r)

if (exists(".representative_ticker", envir = .GlobalEnv, inherits = FALSE)) {
  rep_ticker <- .representative_ticker
}
# Reload representative selection to get t-fit params if available
if (file.exists(file.path(.project_root(), "output",
                           "tables", "model_comparison_summary.csv"))) {
  mod <- read_csv(file.path(.project_root(), "output",
                            "tables", "model_comparison_summary.csv"),
                  show_col_types = FALSE)
  # Extract Student-t params from the saved file is not stored there;
  # recompute from the representative series for correctness.
  mu_t <- mu_hat
  # We need the t-fit params.  We store them in a separate file
  # from script 07.  As a fallback, we use the Normal params and note
  # that the t-parameters were used from the distribution-fitting script.
} else {
  mu_t <- mu_hat
  scale_t <- sigma_hat / 1.7
  df_t <- 4
}

# If the t-file doesn't exist, we need the fitted t params.  We stored them
# in the output/tables from script 07.  Let's search for the t-params file
# we created in script 07 (it saved model_comparison_summary.csv without t params).
# Fallback: recompute t params by optimising below.
if (!file.exists(file.path(.project_root(), "output",
                           "tables", "t_fit_params.csv"))) {
  # Simple optimisation fallback
  neg_loglik_t <- function(par) {
    mu <- par[1]; sigma <- exp(par[2]); df <- exp(par[3])
    -sum(dt((r - mu) / sigma, df = df) / sigma, log = TRUE)
  }
  fit0 <- optim(c(mu_hat, log(sigma_hat), log(4)),
                neg_loglik_t, method = "L-BFGS-B",
                lower = c(-Inf, -Inf, -Inf))
  mu_t <- fit0$par[1]
  scale_t <- exp(fit0$par[2])
  df_t <- exp(fit0$par[3])
  # save
  t_params <- data.frame(mu = mu_t, scale = scale_t, df = df_t)
  write_csv(t_params,
            file.path(.project_root(), "output", "tables", "t_fit_params.csv"))
} else {
  t_params <- read_csv(file.path(.project_root(), "output",
                                  "tables", "t_fit_params.csv"),
                       show_col_types = FALSE)
  mu_t <- t_params$mu[1]
  scale_t <- t_params$scale[1]
  df_t <- t_params$df[1]
}

cat_col(sprintf("  Fitted Normal:  mu = %.6f, sigma = %.6f\n", mu_hat, sigma_hat), "cyan")
cat_col(sprintf("  Fitted Student-t: mu = %.6f, sigma = %.6f, df = %.2f\n",
                mu_t, scale_t, df_t), "cyan")

# ---- simulation parameters -------------------------------------------------------
n_sim <- 10000  # as required ("at least 10,000 returns" per model)
set_project_seed(777)

# ---- 1. Normal simulation ---------------------------------------------------------
sim_normal <- rnorm(n_sim, mean = mu_hat, sd = sigma_hat)

# ---- 2. Student-t simulation --------------------------------------------------------
sim_t <- mu_t + scale_t * rt(n_sim, df = df_t)

# ---- 3. Empirical resampling (block bootstrap for time-series dependence) -----------
# Simple permutation / independent-resampling approach: resample observed
# daily returns with replacement.  This preserves the empirical distribution
# but ignores serial correlation (a limitation we note).
set_project_seed(888)
emp_sample <- sample(r, size = n_sim, replace = TRUE)

# ---- 4. Comparison metrics -----------------------------------------------------------
compare_metrics <- function(sim, name) {
  data.frame(
    Model     = name,
    Mean      = mean(sim),
    SD        = sd(sim),
    P1        = quantile(sim, 0.01, names = FALSE),
    P5        = quantile(sim, 0.05, names = FALSE),
    Prob_Loss = mean(sim < 0),
    Extreme_Loss_Freq = mean(sim < quantile(r, 0.01, names = FALSE))
  )
}

mc_normal <- compare_metrics(sim_normal, "Normal")
mc_t      <- compare_metrics(sim_t, "Student-t")
mc_emp    <- compare_metrics(emp_sample, "Empirical resampling")

mc_summary <- bind_rows(mc_normal, mc_t, mc_emp)
write_csv(mc_summary,
          file.path(.project_root(), "output", "tables",
                    "monte_carlo_summary.csv"))

cat_col("\n" , "blue")
section("MONTE CARLO SUMMARY", "=")
print_table(mc_summary, caption = "Monte Carlo simulation results")

# ---- 5. Comparative visualizations -------------------------------------------------
cat_col("\n" , "blue")
section("COMPARATIVE VISUALIZATIONS", "=")

# Histogram of simulated returns from all three models
sim_df <- data.frame(
  return = c(sim_normal, sim_t, emp_sample),
  Model  = factor(rep(c("Normal", "Student-t", "Empirical"),
                       each = n_sim),
                  levels = c("Normal", "Student-t", "Empirical"))
)

p_sim_hist <- ggplot(sim_df, aes(x = return, fill = Model)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Model", values = c("Normal" = "red",
                                                "Student-t" = "blue",
                                                "Empirical" = "green")) +
  labs(title = "Simulated Returns — Three Models",
       subtitle = sprintf("Representative stock: %s  |  n = %d", rep_ticker, n_sim),
       x = "Simulated daily log return", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig_dir("monte_carlo_simulation.png")
ggsave(fig_dir("monte_carlo_simulation.png"), p_sim_hist,
       width = 11, height = 6, dpi = 300)
cat_col(sprintf("  Saved: output/figures/monte_carlo_simulation.png\n"), "green")

# ---- 6. Key takeaways ----------------------------------------------------------------
cat_col("\n" , "white")
cat_col("  KEY TAKEAWAYS:\n", "cyan")
cat_col("    -- The Normal and Student-t models produce DIFFERENT tail\n", "white")
cat_col("       estimates.  In this demo, which of the two tail estimates\n", "white")
cat_col("       is LARGER depends on the data.  This is exactly the\n", "white")
cat_col("       model-dependence we highlight with VaR in script 14.\n", "white")
cat_col("    -- Simulated 'returns' are model-based.  They are not\n", "white")
cat_col("       forecasts.  They do not say what will happen next\n", "white")
cat_col("       trading day.\n", "white")
