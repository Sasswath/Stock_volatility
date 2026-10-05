# ============================================================
# 14_var_analysis.R — Value at Risk (VaR) for representative
#                      stock and the five-stock portfolio
# ============================================================
#
# PURPOSE
#   Compute Value at Risk using four methods for two assets
#   (representative stock, five-stock portfolio):
#
#     -- Historical method
#     -- Normal parametric method
#     -- Student-t method
#     -- Monte Carlo method (from script 12)
#
#   Confidence levels: 90%, 95%, 99%
#
#   Interpretation rule:
#     "A one-day 95% VaR of X% means that, under the chosen
#      method and assumptions, losses worse than X% are
#      estimated to occur on approximately 5% of trading days."

section("VALUE AT RISK (VaR) ANALYSIS", "=")

# ---- 1. Load data ------------------------------------------------------------------
# Representative stock returns
rep_ret <- readRDS(file.path(.project_root(), "output",
                              "processed", "representative_returns.rds"))
r_rep <- rep_ret$log_ret

# Portfolio returns
all_ret <- read_csv(file.path(.project_root(), "data", "processed",
                              "stock_returns_combined.csv"),
                    show_col_types = FALSE)
ret_wide <- all_ret %>%
  select(date, ticker, log_ret) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret)

port_tick <- intersect(c("RELIANCE", "TCS", "HDFCBANK", "INFOSYS", "TATAMOTORS"),
                       names(ret_wide))

w <- rep(1 / length(port_tick), length(port_tick))
port_return <- colSums(ret_wide[, port_tick, drop = FALSE] * w)
r_port <- port_return[!is.na(port_return)]

# ---- 2. Confidence levels ------------------------------------------------------------
conf_levels <- c(0.90, 0.95, 0.99)

# ---- 3. Historical VaR ---------------------------------------------------------------
# For the historical method, the VaR at level alpha is the (1-alpha)
# empirical quantile of the observed returns (i.e., the loss threshold
# exceeded on (1-alpha)% of days).
var_historical <- function(x, alpha) quantile(x, probs = 1 - alpha, names = FALSE)

# ---- 4. Normal parametric VaR ---------------------------------------------------------
var_normal <- function(x, alpha, mu, sigma) {
  qnorm(1 - alpha, mean = mu, sd = sigma)
}

# ---- 5. Student-t VaR --------------------------------------------------------------------
var_student <- function(x, alpha, mu, sigma, df) {
  qt(1 - alpha, df = df) * sigma + mu
}

# ---- 6. Monte Carlo VaR (reuse simulation from script 12) ---------------------------------
# Re-run the MC with the same seed (already saved in file 12 output).
mc_file <- file.path(.project_root(), "output", "tables", "monte_carlo_summary.csv")
if (file.exists(mc_file)) {
  mc <- read_csv(mc_file, show_col_types = FALSE)
} else {
  # Fallback: recompute quickly
  mu_hat <- mean(r_rep); sigma_hat <- sd(r_rep)
  mu_t <- mu_hat; scale_t <- sigma_hat / 1.7; df_t <- 4
  set_project_seed(777)
  n_sim <- 10000
  sim_normal <- rnorm(n_sim, mu_hat, sigma_hat)
  sim_t <- mu_t + scale_t * rt(n_sim, df = df_t)
  sim_emp <- sample(r_rep, n_sim, replace = TRUE)
  mc <- data.frame(
    Model = c("Normal", "Student-t", "Empirical"),
    Mean = c(mean(sim_normal), mean(sim_t), mean(sim_emp)),
    SD = c(sd(sim_normal), sd(sim_t), sd(sim_emp)),
    P1 = c(quantile(sim_normal, 0.01), quantile(sim_t, 0.01), quantile(sim_emp, 0.01)),
    P5 = c(quantile(sim_normal, 0.05), quantile(sim_t, 0.05), quantile(sim_emp, 0.05)),
    Prob_Loss = c(mean(sim_normal < 0), mean(sim_t < 0), mean(sim_emp < 0)),
    Extreme_Loss_Freq = c(mean(sim_normal < quantile(r_rep, 0.01)),
                          mean(sim_t < quantile(r_rep, 0.01)),
                          mean(sim_emp < quantile(r_rep, 0.01)))
  )
}

# ---- 7. Compute VaR table ---------------------------------------------------------------
cat_col("\n" , "blue")
section("VaR TABLE", "=")

# Using the fitted parameters from the distribution-fitting script
mu_hat <- mean(r_rep)
sigma_hat <- sd(r_rep)
# Load t-parameters if available
t_file <- file.path(.project_root(), "output", "tables", "t_fit_params.csv")
if (file.exists(t_file)) {
  t_params <- read_csv(t_file, show_col_types = FALSE)
  mu_t <- t_params$mu[1]; scale_t <- t_params$scale[1]; df_t <- t_params$df[1]
} else {
  mu_t <- mu_hat; scale_t <- sigma_hat / 1.7; df_t <- 4
}

# Build the VaR table
var_table <- data.frame(
  Asset = c(rep("Representative stock", 4 * 3),
            rep("Five-stock portfolio", 4 * 3)),
  Method = rep(c("Historical", "Normal", "Student-t", "Monte Carlo"),
               each = length(conf_levels)),
  Confidence = rep(conf_levels, times = 8),
  stringsAsFactors = FALSE
)

# Historical VaR
var_table$Value <- NA
for (a in seq_along(conf_levels)) {
  alpha <- conf_levels[a]
  var_table$Value[var_table$Asset == "Representative stock" &
                  var_table$Method == "Historical" & a == 1] <- var_historical(r_rep, alpha)
  var_table$Value[var_table$Asset == "Five-stock portfolio" &
                  var_table$Method == "Historical" & a == 1] <- var_historical(r_port, alpha)
}
# Normal VaR
for (a in seq_along(conf_levels)) {
  alpha <- conf_levels[a]
  var_table$Value[var_table$Asset == "Representative stock" &
                  var_table$Method == "Normal" & a == 1] <- var_normal(r_rep, alpha, mu_hat, sigma_hat)
  var_table$Value[var_table$Asset == "Five-stock portfolio" &
                  var_table$Method == "Normal" & a == 1] <- var_normal(r_port, alpha, mean(r_port), sd(r_port))
}
# Student-t VaR
for (a in seq_along(conf_levels)) {
  alpha <- conf_levels[a]
  var_table$Value[var_table$Asset == "Representative stock" &
                  var_table$Method == "Student-t" & a == 1] <- var_student(r_rep, alpha, mu_t, scale_t, df_t)
  # portfolio uses Normal approx as t parameters are stock-specific
  var_table$Value[var_table$Asset == "Five-stock portfolio" &
                  var_table$Method == "Student-t" & a == 1] <- NA
}
# Monte Carlo VaR
for (a in seq_along(conf_levels)) {
  alpha <- conf_levels[a]
  mc_idx <- which(var_table$Asset == "Representative stock" &
                  var_table$Method == "Monte Carlo" & var_table$Confidence == alpha)
  if (length(mc_idx) > 0) {
    var_table$Value[mc_idx] <- quantile(mc$P1[mc$Model == "Normal"], 1 - alpha)
  }
}

# Format percentages
var_table$Value <- var_table$Value * 100

# Clean up missings
var_table$Value[is.na(var_table$Value)] <- NA

write_csv(var_table,
          file.path(.project_root(), "output", "tables", "var_table.csv"))

cat_col("\n  VALUE AT RISK TABLE (daily, percentage returns):", "white")
print_table(var_table, caption = "Value at Risk — representative stock and portfolio")

# ---- 8. Interpretation notes -----------------------------------------------------------
cat_col("\n  HOW TO READ VaR:", "white")
cat_col(sprintf("    95%% VaR = the loss threshold not exceeded on 95%% of days", "white"))
cat_col(sprintf("    (i.e., a loss worse than this happens on ~5% of trading days).", "white"))
cat_col(sprintf("    VaR is a QUANTILE, NOT a maximum loss.", "white"))
cat_col(sprintf("    The four methods can give quite different numbers because", "white"))
cat_col(sprintf("    they make different assumptions about the distribution of", "white"))
cat_col(sprintf("    extreme returns.", "white"))
cat_col(sprintf("    Under the Normal model, the 99% VaR for the", "white"))
cat_col(sprintf("    representative stock is approximately %.4f%%. Under the", "white"))
cat_col(sprintf("    Student-t model it is approximately %.4f%.", "white"))
cat_col(sprintf("    The difference is a direct consequence of the fat tails.", "white"))
