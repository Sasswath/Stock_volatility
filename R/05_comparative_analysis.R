# ============================================================
# 05_comparative_analysis.R — Five-stock comparative analysis
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(moments)

# Resolve project path helper if present
get_root <- function() {
  if (exists(".project_root", mode = "function")) {
    return(.project_root())
  }
  return(getwd())
}

root_dir <- get_root()

# Fixed pattern: escaped double backslash \\.
clean_files <- list.files(file.path(root_dir, "data", "processed"), 
                          pattern = "^clean_prices_.*\\.csv$", 
                          full.names = TRUE)

if (length(clean_files) == 0) {
  clean_files <- list.files("data/processed", 
                            pattern = "^clean_prices_.*\\.csv$", 
                            full.names = TRUE)
}

all_ret <- bind_rows(lapply(clean_files, read_csv, show_col_types = FALSE))

# Write combined returns
dir.create(file.path(root_dir, "data", "processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(all_ret, file.path(root_dir, "data", "processed", "stock_returns_combined.csv"))
write_csv(all_ret, "data/processed/stock_returns_combined.csv")

# Create output directories
out_tab_dir <- file.path(root_dir, "output", "tables")
out_fig_dir <- file.path(root_dir, "output", "figures")
dir.create(out_tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_fig_dir, showWarnings = FALSE, recursive = TRUE)

tickers <- unique(all_ret[["ticker"]])
summary_list <- list()

for (t in tickers) {
  sub_df <- all_ret[all_ret[["ticker"]] == t & !is.na(all_ret[["log_ret"]]), ]
  r_vec <- sub_df[["log_ret"]]
  
  sd_val <- sd(r_vec, na.rm = TRUE)
  ann_vol <- sd_val * sqrt(252)
  
  summary_list[[t]] <- data.frame(
    Ticker    = t,
    Company   = unique(sub_df[["company"]])[1],
    Mean      = mean(r_vec, na.rm = TRUE),
    Median    = median(r_vec, na.rm = TRUE),
    SD        = sd_val,
    Variance  = sd_val^2,
    Minimum   = min(r_vec, na.rm = TRUE),
    Maximum   = max(r_vec, na.rm = TRUE),
    Range     = max(r_vec, na.rm = TRUE) - min(r_vec, na.rm = TRUE),
    Q1        = as.numeric(quantile(r_vec, 0.25, na.rm = TRUE)),
    Q3        = as.numeric(quantile(r_vec, 0.75, na.rm = TRUE)),
    IQR       = IQR(r_vec, na.rm = TRUE),
    Skewness  = moments::skewness(r_vec, na.rm = TRUE),
    Kurtosis  = moments::kurtosis(r_vec, na.rm = TRUE),
    AnnualVol = ann_vol,
    Rows      = length(r_vec),
    stringsAsFactors = FALSE
  )
}

all_metrics <- bind_rows(summary_list)

# Write to both target and relative directories so 06 finds it guaranteed
write_csv(all_metrics, file.path(out_tab_dir, "5stock_descriptive_stats.csv"))
write_csv(all_metrics, "output/tables/5stock_descriptive_stats.csv")

vol_df <- all_metrics %>% arrange(desc(AnnualVol))
p_vol <- ggplot(vol_df, aes(x = reorder(Ticker, AnnualVol), y = AnnualVol, fill = Ticker)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Annualised Volatility — Five Stocks",
       x = "Stock", y = "Annualised Volatility (daily sigma * sqrt(252))") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(out_fig_dir, "comparative_volatility.png"), p_vol, width = 9, height = 5.5, dpi = 300)

cum_df <- all_ret %>%
  filter(!is.na(log_ret)) %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(cum_ret = cumprod(1 + log_ret) * 100)

p_ret <- ggplot(cum_df, aes(x = date, y = cum_ret, color = ticker)) +
  geom_line(linewidth = 1) +
  labs(title = "Cumulative Return Index — Five Stocks",
       x = "Date", y = "Cumulative return index (base 100)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_fig_dir, "comparative_returns.png"), p_ret, width = 11, height = 5.5, dpi = 300)

p_box <- ggplot(all_ret %>% filter(!is.na(log_ret)), aes(x = ticker, y = log_ret, fill = ticker)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Return Distribution Boxplots — Five Stocks", x = "Stock", y = "Daily log return") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(out_fig_dir, "boxplots_five_stocks.png"), p_box, width = 9, height = 5, dpi = 300)

p_dist <- ggplot(all_ret %>% filter(!is.na(log_ret)), aes(x = log_ret, fill = ticker)) +
  geom_histogram(bins = 60, alpha = 0.5, position = "identity") +
  labs(title = "Return Distribution — Five Stocks", x = "Daily log return", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_fig_dir, "distribution_five_stocks.png"), p_dist, width = 12, height = 6, dpi = 300)

ret_wide <- all_ret %>%
  filter(!is.na(log_ret)) %>%
  dplyr::select(date, ticker, log_ret) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = log_ret)

numeric_cols <- setdiff(names(ret_wide), "date")
cor_mat <- cor(as.matrix(ret_wide[, numeric_cols]), use = "pairwise.complete.obs")

cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column("Stock1") %>%
  pivot_longer(-Stock1, names_to = "Stock2", values_to = "Correlation")

p_corr <- ggplot(cor_long, aes(x = Stock1, y = Stock2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4) +
  scale_fill_viridis_c(limits = c(-1, 1)) +
  labs(title = "Correlation Matrix — Five Stocks", x = "Stock", y = "Stock") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig_dir, "correlation_heatmap.png"), p_corr, width = 8, height = 6, dpi = 300)
write.csv(cor_mat, file.path(out_tab_dir, "correlation_matrix.csv"))

cat("\nStep 05 complete.\n")

