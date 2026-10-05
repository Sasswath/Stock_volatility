# ============================================================
# 09_hypothesis_testing.R — Hypothesis tests for the
#                             representative stock
# ============================================================
#
# PURPOSE
#   Formal hypothesis tests on the representative stock's daily
#   log returns:
#
#     Test 1 (main):  H0: mu = 0  vs  H1: mu != 0
#                     (one-sample t-test on the mean daily return)
#     Test 2 (optional): Levene/Brown-Forsythe test comparing
#                       volatility between representative stock
#                       and another stock (context only).
#
#   We report the test statistic, p-value, and a careful
#   interpretation that separates statistical significance from
#   practical significance.

section("HYPOTHESIS TESTING", "=")
cat_col(sprintf("Representative stock: %s\n", rep_ticker), "magenta")

r <- rep_ret$log_ret
n <- length(r)
alpha <- 0.05

# -----------------------------------------------------------------------
# Test 1: H0: mu = 0 vs H1: mu != 0
# -----------------------------------------------------------------------
cat_col("\n" , "blue")
section("TEST 1: Is the mean daily return different from zero?", "=")

# One-sample t-test
t_test <- t.test(r, mu = 0, alternative = "two.sided")

cat_col("  H0: mu = 0        (mean daily log return is zero)", "cyan")
cat_col("  H1: mu != 0       (mean daily log return is non-zero)", "cyan")
cat_col("  Significance level: alpha = 0.05", "cyan")

cat_col(sprintf("\n  Test statistic  t = %.4f\n", t_test$statistic), "cyan")
cat_col(sprintf("  Degrees of freedom = %d\n", t_test$parameter), "cyan")
cat_col(sprintf("  p-value         = %.4f\n", t_test$p.value), "cyan")

# 95% CI for the mean
ci <- t_test$conf.int
cat_col(sprintf("  95%% CI for the mean: [%.4f%%, %.4f%%]\n",
                ci[1] * 100, ci[2] * 100), "cyan")

# Interpretation
if (t_test$p.value < alpha) {
  cat_col("  Result: p < 0.05, so we reject H0 at the 5% level.", "cyan")
  cat_col("  We conclude the mean daily return is statistically", "cyan")
  cat_col("  significantly different from zero.", "cyan")
} else {
  cat_col("  Result: p >= 0.05, so we do NOT reject H0 at the 5% level.", "cyan")
  cat_col("  We conclude there is insufficient evidence that the mean", "cyan")
  cat_col("  daily return is different from zero.", "cyan")
}

cat_col(sprintf("\n  PRACTICAL SIGNIFICANCE:", "white"))
cat_col(sprintf("  The mean daily return is %.4f%%. Over a full year\n", "white"))
cat_col(sprintf("  (252 trading days) the average cumulative return is approximately\n", "white"))
cat_col(sprintf("  %.2f%%. This is a small number economically, even if it is\n", "white"))
cat_col(sprintf("  statistically detectable with n = %d observations.", "white"))
cat_col(sprintf("  Statistical significance does not equal economic importance.", "white"))

# -----------------------------------------------------------------------
# Test 2: Volatility comparison between representative stock and
#         another stock (optional, context only).
# -----------------------------------------------------------------------
cat_col("\n" , "blue")
section("TEST 2: Volatility comparison (representative vs TCS, context only)", "=")

other_ticker <- "TCS"
other_ret <- read_csv(file.path(.project_root(), "data", "processed",
                                 sprintf("stock_returns_%s.csv", other_ticker)),
                      show_col_types = FALSE)
o <- other_ret$log_ret

# Brown-Forsythe test (robust version of Levene's test) — uses
# absolute deviations from the group medians.
# We implement the test via a simple bootstrap / resampling.
# Simplified: Welch's t-test on absolute deviations tests equality
# of variances (the square of the test statistic approximates the
# variance-ratio test).
dev_rep <- abs(r - median(r))
dev_other <- abs(o - median(o))

# Welch's t-test on absolute deviations (equivalent to testing
# equality of variances under symmetry assumptions)
wl_test <- t.test(dev_rep, dev_other, alternative = "two.sided")

cat_col("  H0: sigma_rep = sigma_other (equal volatility)", "cyan")
cat_col("  H1: sigma_rep != sigma_other (unequal volatility)", "cyan")
cat_col(sprintf("  Representative:  sigma = %.4f%%\n", sd(r) * 100), "cyan")
cat_col(sprintf("  Other stock     :  sigma = %.4f%%\n", sd(o) * 100), "cyan")
cat_col(sprintf("  Test statistic  t = %.4f\n", wl_test$statistic), "cyan")
cat_col(sprintf("  p-value         = %.4f\n", wl_test$p.value), "cyan")

if (wl_test$p.value < alpha) {
  cat_col("  Result: p < 0.05 — evidence of unequal volatility.", "cyan")
} else {
  cat_col("  Result: p >= 0.05 — insufficient evidence of unequal vol.", "cyan")
}

# -----------------------------------------------------------------------
# Test 3: Skewness test (is the distribution symmetric?)
# -----------------------------------------------------------------------
cat_col("\n" , "blue")
section("TEST 3: Is the distribution symmetric? (Skewness = 0)", "=")

# Test skewness against 0 using a normal-approximation test.
# Under H0 (symmetrical), sqrt(n) * skew / sqrt(6/n) ~ N(0,1)
skew_stat <- moments::skewness(r)
se_skew <- sqrt(6 / n)
z_skew <- skew_stat / se_skew
p_skew <- 2 * (1 - pnorm(abs(z_skew)))

cat_col(sprintf("  H0: skewness = 0\n", "cyan"))
cat_col(sprintf("  H1: skewness != 0\n", "cyan"))
cat_col(sprintf("  Sample skewness = %.4f\n", skew_stat), "cyan")
cat_col(sprintf("  z-statistic     = %.4f\n", z_skew), "cyan")
cat_col(sprintf("  p-value         = %.4f\n", p_skew), "cyan")

if (p_skew < alpha) {
  cat_col("  Result: significant skewness — the distribution is asymmetric.", "cyan")
  if (skew_stat > 0) {
    cat_col("  Positive skew: fat right tail (occasional large gains).", "cyan")
  } else {
    cat_col("  Negative skew: fat left tail (occasional large losses).", "cyan")
  }
} else {
  cat_col("  Result: no significant skewness detected.", "cyan")
}

# -----------------------------------------------------------------------
# Test 4: Kurtosis test (excess kurtosis = 0, i.e., Normal tails)
# -----------------------------------------------------------------------
cat_col("\n" , "blue")
section("TEST 4: Are the tails heavier than Normal?", "=")

# Under H0 (Normal), excess kurtosis has variance 24/n approx.
kurt_stat <- moments::kurtosis(r)
excess_kurt <- kurt_stat - 3
se_kurt <- sqrt(24 / n)
z_kurt <- excess_kurt / se_kurt
p_kurt <- 2 * (1 - pnorm(abs(z_kurt)))

cat_col(sprintf("  H0: excess kurtosis = 0 (Normal tails)\n", "cyan"))
cat_col(sprintf("  H1: excess kurtosis != 0 (heavy or light tails)\n", "cyan"))
cat_col(sprintf("  Sample excess kurtosis = %.4f\n", excess_kurt), "cyan")
cat_col(sprintf("  z-statistic     = %.4f\n", z_kurt), "cyan")
cat_col(sprintf("  p-value         = %.4f\n", p_kurt), "cyan")

if (p_kurt < alpha) {
  cat_col("  Result: significant heavy/light tails detected.", "cyan")
  if (excess_kurt > 0) {
    cat_col("  Positive excess kurtosis = fat tails (leptokurtic).", "cyan")
    cat_col("  This is typical of financial returns and motivates", "cyan")
    cat_col("  the Student's t model studied elsewhere.", "cyan")
  } else {
    cat_col("  Negative excess kurtosis = thin tails (platykurtic).", "cyan")
  }
} else {
  cat_col("  Result: no significant evidence against Normal tails.", "cyan")
}
