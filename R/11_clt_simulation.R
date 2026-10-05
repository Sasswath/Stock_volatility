# ============================================================
# 11_clt_simulation.R — Central Limit Theorem demonstration
# ============================================================
#
# PURPOSE
#   Demonstrate the Central Limit Theorem (CLT) using the
#   representative stock's daily log returns.
#
#   THE CLT SAYS:  The distribution of the SAMPLE MEAN
#   (for i.i.d. draws from ANY distribution with finite
#   variance) converges to a Normal distribution as the
#   sample size n increases.
#
#   IMPORTANT: The CLT applies to the MEAN of samples, NOT
#   to the individual observations.  Individual stock
#   returns may remain non-Normal even for large n.  This
#   simulation DOES NOT claim that stock returns themselves
#   become Normal.
#
#   Sample sizes:  n = 5, 10, 30, 50, 100
#   Repetitions:    at least 5,000 per n

section("CENTRAL LIMIT THEOREM SIMULATION", "=")
cat_col(sprintf("Representative stock: %s\n", rep_ticker), "magenta")

r <- rep_ret$log_ret
n_obs <- length(r)

# Sample sizes
ns <- c(5, 10, 30, 50, 100)
n_sim <- 5000  # number of repetitions per n (>= 5,000 as required)

set_project_seed(12345)

# ---- storage for sample means + standard errors ---------------------------------
means_list <- list()
se_theoretical <- list()
se_empirical <- list()
skew_list <- list()
kurt_list <- list()

# Plot layout: 1 row, 5 columns
p_list <- list()

cat_col("\n" , "blue")
section("CLT: Distribution of Sample Means", "=")

for (ni in ns) {
  cat_col(sprintf("\n  n = %d, %d repetitions...\n", ni, n_sim), "cyan")

  # Draw ni-sized samples (with replacement) and compute the mean each time
  # Use a matrix for speed
  idx <- matrix(sample.int(n_obs, size = ni * n_sim, replace = TRUE),
                nrow = n_sim, ncol = ni)
  sample_means <- rowMeans(matrix(r[idx], nrow = n_sim, ncol = ni))

  # Store
  means_list[[as.character(ni)]] <- sample_means

  # Standard error
  sigma_bar <- sd(r)
  se_theory <- sigma_bar / sqrt(ni)
  se_emp <- sd(sample_means)

  se_theoretical[[as.character(ni)]] <- se_theory
  se_empirical[[as.character(ni)]] <- se_emp

  # Skewness and kurtosis of the sampling distribution
  skew_list[[as.character(ni)]] <- moments::skewness(sample_means)
  kurt_list[[as.character(ni)]] <- moments::kurtosis(sample_means)

  # Plot the sampling distribution with Normal overlay
  p <- ggplot(data.frame(x = sample_means), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40,
                   fill = "steelblue", color = "white", alpha = 0.7) +
    stat_function(fun = function(x) dnorm(x, mean = mean(sample_means),
                                          sd = se_emp),
                  color = "red", linewidth = 1.2,
                  aes(linewidth = I(1.2)), inherit.aes = FALSE) +
    labs(title = sprintf("n = %d  |  SE_empirical ≈ %.4f%%", ni, se_emp * 100),
         subtitle = sprintf("Theoretical SE = %.4f%% (sigma/sqrt(n))",
                            se_theory * 100),
         x = "Sample mean", y = "Density") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "grey50"))

  p_list[[as.character(ni)]] <- p
}

# ---- Multi-panel CLT figure (5 panels chained) ---------------------------------
cat_col("\n" , "blue")
section("5-panel CLT visualization", "=")
p_clt_grid <- wrap_plots(p_list, ncol = 1,guides = "collect") +
  plot_annotation(tag_levels = "A")

fig_dir("clt_simulation.png")
ggsave(fig_dir("clt_simulation.png"), p_clt_grid,
       width = 9, height = 11, dpi = 300)
cat_col(sprintf("  Saved: output/figures/clt_simulation.png\n"), "green")

# ---- CLT summary table -------------------------------------------------------------
cat_col("\n" , "blue")
section("CLT SUMMARY: SE IDENTITY", "=")

clt_summary <- data.frame(
  n       = ns,
  SE_theory = unlist(se_theoretical) * 100,
  SE_empirical = unlist(se_empirical) * 100,
  Ratio = unlist(se_empirical) / unlist(se_theoretical),
  Skewness = unlist(skew_list),
  Kurtosis = unlist(kurt_list) - 3  # excess kurtosis
)

write_csv(clt_summary,
          file.path(.project_root(), "output", "tables", "clt_summary.csv"))

cat_col("\n  CLT summary table (SE identity check):", "white")
print_table(clt_summary, caption = "Central Limit Theorem — standard error verification")

cat_col("\n  Observe:\n", "white")
cat_col("    The empirical SE (sd of sample means) converges to the\n", "white")
cat_col("    theoretical SE (sigma / sqrt(n)) as n grows.\n", "white")
cat_col("    Skewness of the sampling distribution shrinks toward 0\n", "white")
cat_col("    and excess kurtosis shrinks toward 0 (Normal).\n", "white")
cat_col("    This is the CLT in action.\n", "white")

# ---- Note: individual returns remain non-Normal -----------------------------------
cat_col("\n" , "white")
cat_col("\n  CRITICAL REMINDER: The CLT is about the MEAN of samples,\n", "cyan")
cat_col("  not about individual stock returns.  Individual returns of\n", "cyan")
cat_col(sprintf("  %s are still skewed and heavy-tailed.\n", rep_ticker), "cyan")
cat_col("  The sampling distribution of the mean becomes approximately\n", "cyan")
cat_col("  Normal even when the underlying population is not.\n", "cyan")
