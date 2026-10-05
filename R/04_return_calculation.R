# ============================================================
# 04_return_calculation.R — Calculate simple and log returns
# ============================================================
#
# PURPOSE
#   -- Simple return:  R_t = (P_t - P_(t-1)) / P_(t-1)
#   -- Log  return:    r_t = log(P_t / P_(t-1))
#
#   Both are computed, explained, and logged.  Log returns are
#   used for all subsequent statistical analysis because they:
#     (1) are time-additive (compounding is just summation),
#     (2) are (approximately) symmetric, so means behave well,
#     (3) are (approximately) normally distributed for many
#         assets, which fits the Gaussian-based tests used
#         elsewhere in the project.
#
#   Only the unavoidable NA (first observation per stock) is
#   dropped.  No extreme return is removed here.

# ---- load cleaned prices ----------------------------------------------------
clean_files <- list.files(file.path(.project_root(), "data", "processed"),
                          pattern = "clean_prices_.*\\.csv$", full.names = TRUE)
clean_files <- c(clean_files,
                 file.path(.project_root(), "data", "raw", "combined_raw_prices.csv"))

cat_col(sprintf("\nLoading cleaned price data for %d stock(s)...\n",
                length(clean_files)), "cyan")

price_list <- list()
for (f in clean_files) {
  ticker <- sub("clean_prices_|\\.csv", "", basename(f))
  # avoid overwriting unique ticker from combined file
  if (ticker == "combined_raw_prices") ticker <- "ALL"
  df <- read_csv(f, show_col_types = FALSE)
  # make sure date is a Date
  df$date <- as.Date(df$date)
  price_list[[ticker]] <- df %>% arrange(date)
}

# ---- compute returns ---------------------------------------------------------
ret_list <- list()

for (ticker in names(price_list)) {
  df <- price_list[[ticker]]

  # simple returns
  df$simple_ret <- c(NA, (df$adj_close[-nrow(df)] - df$adj_close[-1]) / df$adj_close[-1])

  # log returns  (r_t = log(P_t / P_(t-1)) = log(P_t) - log(P_(t-1)))
  df$log_ret <- c(NA, diff(log(df$adj_close)))

  # NAs:
  #   The first row has no previous price -> NA.  This is the ONLY
  #   NA we drop.  Everything else is retained.
  df_ret <- df %>% filter(!is.na(log_ret)) %>% arrange(date)

  # ---- save returns -----------------------------------------------------------
  out_path <- file.path(.project_root(), "data", "processed",
                        sprintf("stock_returns_%s.csv", ticker))
  write_csv(df_ret, out_path)

  cat_col(sprintf("\n--- %s ---\n", ticker), "magenta")
  cat_col(sprintf("  Simple returns: %d | Log returns: %d\n",
                  nrow(df_ret), nrow(df_ret)), "white")
  cat_col(sprintf("  Saved: %s\n", out_path), "green")

  # ---- quick stats ------------------------------------------------------------
  cat_col(sprintf("  Mean simple ret: %.4f%% | Mean log  ret: %.4f%%\n",
                  mean(df_ret$simple_ret) * 100,
                  mean(df_ret$log_ret) * 100), "white")
  cat_col(sprintf("  SD   simple ret: %.4f%% | Mean log  ret: %.4f%%\n",
                  sd(df_ret$simple_ret) * 100,
                  sd(df_ret$log_ret) * 100), "white")

  ret_list[[ticker]] <- df_ret
}

# ---- combined returns table (for later use) ------------------------------------
all_returns <- bind_rows(lapply(ret_list, function(x) x %>% select(ticker, date, log_ret)))
write_csv(all_returns,
          file.path(.project_root(), "data", "processed", "stock_returns_combined.csv"))

cat_col("\nReturn calculation complete.\n", "green")
cat_col("  Log returns used for all subsequent statistical work.", "white")
