# Statistical Modeling of Stock Market Volatility, Extreme Returns, and the Central Limit Theorem

**A 4-member undergraduate statistics/probability course project**

---

## Overview

This project investigates how probability distributions describe stock-market returns, and how the choice of distribution affects the estimation of extreme financial risk. It uses five NSE-listed stocks across four distinct sectors, fits both Normal and Student's t distributions, compares them via AIC/BIC and goodness-of-fit tests, demonstrates the Central Limit Theorem via simulation, runs Monte Carlo simulations, constructs an equally weighted portfolio, and compares Value at Risk (VaR) across four methods.

> **⚠️ Important:** This is **NOT** a stock-price prediction project. It focuses on probability distributions, statistical estimation, descriptive statistics, volatility, goodness-of-fit testing, hypothesis testing, the Central Limit Theorem, Monte Carlo simulation, portfolio diversification, and Value at Risk.

---

## Project Architecture

The project follows a mandatory analytical architecture:

```
                 5 STOCKS
                    |
                    v
         BROAD COMPARATIVE ANALYSIS
                    |
                    v
         SELECT 1 REPRESENTATIVE STOCK
                    |
                    v
       DEEP STATISTICAL ANALYSIS
              +--------+
              |        |
              v        v
         Normal Model   Student-t Model
              |        |
              +--------+
                    |
                    v
              Goodness-of-Fit / AIC / BIC
                    |
                    v
                EXTREME RETURNS
                    |
                    v
                    CLT
                    |
                    v
            MONTE CARLO SIMULATION
                    |
                    v
            RETURN TO ALL 5 STOCKS
                    |
                    v
          EQUALLY WEIGHTED PORTFOLIO
                    |
                    v
                VaR / RISK
```

**Key principle:** The five-stock stage provides breadth. The representative-stock stage provides depth. The portfolio stage demonstrates practical application.

---

## Data

### Five Stocks Selected

| Ticker | Company | Sector | Market |
|--------|---------|--------|--------|
| `RELIANCE` | Reliance Industries | Energy / Oil & Gas | NSE (India) |
| `TCS` | Tata Consultancy Services | IT / Software Services | NSE (India) |
| `HDFCBANK` | HDFC Bank | Financials / Banking | NSE (India) |
| `INFOSYS` | Infosys | IT / Software Services | NSE (India) |
| `LT` | Larsen & Toubro | Infrastructure / Construction | NSE (India) |

### Dataset

- **Source:** National Stock Exchange of India (NSE) via `quantmod` (Yahoo Finance endpoint)
- **Period:** ~2020-01-01 to 2025-12-31 (5 trading years)
- **Prices:** Adjusted closing prices
- **Returns:** Log returns ($r_t = \log(P_t / P_{t-1})$)

### Data Storage

```
data/raw/                    # Raw downloaded prices (never overwritten)
├── raw_prices_RELIANCE.csv
├── raw_prices_TCS.csv
├── raw_prices_HDFCBANK.csv
├── raw_prices_INFOSYS.csv
├── raw_prices_LT.csv
└── combined_raw_prices.csv  # Combined for reference

data/processed/              # Cleaned + derived data
├── stock_returns_combined.csv
├── stock_returns_RELIANCE.csv
├── stock_returns_TCS.csv
├── stock_returns_HDFCBANK.csv
├── stock_returns_INFOSYS.csv
├── stock_returns_LT.csv
├── clean_prices_RELIANCE.csv
├── clean_prices_TCS.csv
├── clean_prices_HDFCBANK.csv
├── clean_prices_INFOSYS.csv
└── clean_prices_LT.csv
```

### Selection Rationale

The stocks are selected from **four distinct sectors** to provide meaningful diversification analysis. Selection is **NOT** based on which stocks produce the most impressive statistical results — it is based on sector diversity, data availability, and data quality.

---

## R Environment

### Requirements

- **R** ≥ 4.0.0 (R 4.3.0+ recommended)
- **RStudio** (recommended IDE)

### Required R Packages

| Package | Purpose |
|---------|---------|
| `quantmod` | Download historical stock data from NSE |
| `tidyverse` | Data manipulation (`dplyr`, `tidyr`, `ggplot2`, `lubridate`) |
| `readr` | CSV file reading |
| `moments` | Skewness and kurtosis calculation |
| `fitdistrplus` | Maximum likelihood distribution fitting (Student-t, Normal) |
| `MASS` | Statistical functions |
| `tseries` | Time series analysis |
| `PerformanceAnalytics` | Financial performance and risk metrics |
| `scales` | Plotting axis formatting |
| `patchwork` | Combining ggplot2 plots |

### Installation

Run the setup script (`R/01_setup.R`) which will automatically install any missing packages.

Or install manually in R:

```r
install.packages(c(
  "quantmod", "tidyverse", "readr", "moments",
  "fitdistrplus", "MASS", "tseries", "PerformanceAnalytics",
  "scales", "patchwork"
))
```

### Environment Configuration

The project uses:
- `set.seed(12345)` for reproducible simulations
- Relative paths throughout (no hard-coded local paths)
- `.Rprofile` auto-sources all utility scripts
- `.Rprofile` detects the project root automatically

---

## Execution Instructions

### Recommended: Open the Project in RStudio

1. Open RStudio
2. **File → Open Project** → Select `stock-volatility-project.Rproj`
3. RStudio sets the working directory to the project root automatically

### Running from a Terminal

```bash
# Option 1: Source all scripts in order
Rscript R/01_setup.R
Rscript R/02_data_collection.R
Rscript R/03_data_cleaning.R
Rscript R/04_return_calculation.R
Rscript R/05_comparative_analysis.R
Rscript R/06_representative_stock.R
Rscript R/07_distribution_fitting.R
Rscript R/08_goodness_of_fit.R
Rscript R/09_hypothesis_testing.R
Rscript R/10_extreme_returns.R
Rscript R/11_clt_simulation.R
Rscript R/12_monte_carlo.R
Rscript R/13_portfolio_analysis.R
Rscript R/14_var_analysis.R
Rscript R/15_visualizations.R

# Option 2: Run the Quarto report
Rscript -e "quarto::render('report/final_report.qmd')"
```

### Running Individual Scripts

Each script prints a progress header and is designed to be run independently. However, **order matters**:

| Order | Script | Output |
|-------|--------|--------|
| 01 | `R/01_setup.R` | Installs packages, sets seed, configures paths |
| 02 | `R/02_data_collection.R` | Downloads 5 stocks → `data/raw/` |
| 03 | `R/03_data_cleaning.R` | Cleans & validates → `data/processed/` |
| 04 | `R/04_return_calculation.R` | Computes returns → `data/processed/` |
| 05 | `R/05_comparative_analysis.R` | 5-stock descriptive stats + viz |
| 06 | `R/06_representative_stock.R` | Selects representative stock |
| 07 | `R/07_distribution_fitting.R` | Normal + Student-t MLE on representative stock |
| 08 | `R/08_goodness_of_fit.R` | KS, Shapiro-Wilk, AD, Chi-square tests |
| 09 | `R/09_hypothesis_testing.R` | t-test, CI, skewness/kurtosis tests |
| 10 | `R/10_extreme_returns.R` | Percentiles, tail comparison |
| 11 | `R/11_clt_simulation.R` | CLT demonstration (n=5,10,30,50,100) |
| 12 | `R/12_monte_carlo.R` | 10,000 simulations (3 models) |
| 13 | `R/13_portfolio_analysis.R` | 5-stock equally weighted portfolio |
| 14 | `R/14_var_analysis.R` | VaR (4 methods × 3 confidence levels) |
| 15 | `R/15_visualizations.R` | 15 required figures |

### Running Scripts in RStudio

1. Open the script in RStudio
2. Press **Ctrl+Shift+Enter** (Windows/Linux) or **Cmd+Shift+Enter** (Mac)
3. Or: **Source** from the **Code** menu

### Running the Quarto Report

```bash
# Install Quarto first: https://quarto.org/docs/get-started/
Rscript -e "quarto::render('report/final_report.qmd')"
```

This generates the HTML report in `report/`.

---

## Project Structure

```
stock-volatility-project/
│
├── stock-volatility-project.Rproj     # RStudio project file
├── .Rprofile                          # Project environment setup
├── README.md                          # This file
│
├── data/
│   ├── raw/                           # Raw downloaded prices (never overwritten)
│   └── processed/                     # Cleaned + derived data
│
├── R/                                 # R scripts (run in order)
│   ├── 00_utils.R                     # Shared helper functions
│   ├── 00_stock_info.R                # Stock metadata lookup
│   ├── 01_setup.R                     # Packages, seed, paths
│   ├── 02_data_collection.R           # Download 5 NSE stocks
│   ├── 03_data_cleaning.R             # Validation & cleaning
│   ├── 04_return_calculation.R        # Simple + log returns
│   ├── 05_comparative_analysis.R      # 5-stock descriptive stats
│   ├── 06_representative_stock.R      # Objective selection criteria
│   ├── 07_distribution_fitting.R      # Normal + Student-t MLE
│   ├── 08_goodness_of_fit.R           # KS, Shapiro-Wilk, AD, Chi-square
│   ├── 09_hypothesis_testing.R        # t-test, CI, symmetry/kurtosis
│   ├── 10_extreme_returns.R           # Percentiles, tail comparison
│   ├── 11_clt_simulation.R            # CLT (n=5,10,30,50,100)
│   ├── 12_monte_carlo.R               # 10,000 sims (3 models)
│   ├── 13_portfolio_analysis.R        # Equal-weighted 5-stock portfolio
│   ├── 14_var_analysis.R              # VaR (4 methods × 3 CL)
│   └── 15_visualizations.R            # 15 required figures
│
├── output/
│   ├── figures/                       # All figures (15+)
│   ├── tables/                        # All tables
│   └── simulations/                   # Simulation results
│
├── report/
│   └── final_report.qmd               # Quarto report (28 sections)
│
└── presentation/
    └── presentation_outline.md        # 20-slide presentation outline
```

---

## Representative Stock Selection Methodology

The representative stock is selected using **pre-specified, objective criteria**:

1. **Sufficient observations** (> 1000 daily returns)
2. **Meaningful return variation** (non-trivial annualized volatility)
3. **Noticeable but legitimate skewness / kurtosis** (illustration of tail behaviour)
4. **Clean data** with no critical gaps

The selection uses a composite score across these criteria. The stock is **NOT** selected for producing dramatic or favourable results.

**Selected stock:** `TCS` (Tata Consultancy Services — IT / Software Services)

All deep statistical analysis (distribution fitting, tests, CLT, Monte Carlo, VaR) uses **only TCS**. The other four stocks remain in the comparative and portfolio analyses.

---

## Statistical Methods

### Distribution Fitting (Normal & Student's t)

**Maximum Likelihood Estimation (MLE):**

- **Normal:** $\hat{\mu} = \bar{x}$, $\hat{\sigma}^2 = \frac{1}{n-1}\sum (x_i - \bar{x})^2$
- **Student's t:** Location, scale, and degrees of freedom estimated via `fitdistrplus`

**Model comparison:**
- Log-likelihood
- AIC = $-2 \log L + 2k$
- BIC = $-2 \log L + k \log n$

**Goodness-of-fit tests:**
- Kolmogorov-Smirnov (with parametric bootstrap for fitted parameters)
- Shapiro-Wilk
- Anderson-Darling (tail-sensitive)
- Chi-square

**Key caveat:** With $n \approx 1000+$ observations, formal tests detect trivial departures from the theoretical distribution. **Statistical significance ≠ practical significance.**

### Central Limit Theorem

The CLT simulation generates sample means from the representative stock's returns:

- Sample sizes: $n = 5, 10, 30, 50, 100$
- Repetitions: 5,000 per $n$
- Verifies: $SE(\bar{x}) = \sigma / \sqrt{n}$

**Critical reminder:** The CLT applies to the **sample mean**, not to individual returns.

### Monte Carlo Simulation

Simulate 10,000 returns under three models:
- Normal (MLE parameters)
- Student-t (MLE parameters)
- Empirical resampling (sample with replacement)

Compare: mean, SD, 1%/5% percentiles, probability of loss, extreme-loss frequency.

**Model-based results ≠ predictions.** Simulated results describe the behaviour implied by each model, under its assumptions.

### Portfolio Analysis

Equal-weighted (20% each):

- Daily portfolio return: $r_{p,t} = \sum_{i=1}^{5} w_i r_{i,t}$ where $w_i = 0.2$
- Portfolio variance: $\sigma_p^2 = \frac{1}{k}\bar{\sigma}^2 + \frac{k-1}{k}\bar{\sigma}_{cov}$
- Diversification effect: Compare portfolio volatility vs average individual volatility

### Value at Risk (VaR)

Four methods at 90%, 95%, 99% confidence:

| Method | Description |
|--------|-------------|
| **Historical** | Empirical quantile of observed returns |
| **Normal** | $VaR = \mu + z_\alpha \cdot \sigma$ |
| **Student-t** | $VaR = \mu + t_{\alpha, \nu} \cdot \sigma$ |
| **Monte Carlo** | Simulated quantile from fitted models |

**Interpretation:** "A one-day 95% VaR of X% means that, under the chosen method and assumptions, losses worse than X% are estimated to occur on approximately 5% of trading days."

---

## Output Files

### Figures (output/figures/)

| File | Description |
|------|-------------|
| `historical_prices.png` | Five-stock adjusted closing prices |
| `comparative_returns.png` | Cumulative return index |
| `comparative_volatility.png` | Annualised volatility by stock |
| `boxplots_five_stocks.png` | Return distribution boxplots |
| `distribution_five_stocks.png` | Return distribution panels |
| `correlation_heatmap.png` | Correlation matrix heatmap |
| `representative_return_distribution.png` | Representative stock returns vs fitted Normal |
| `normal_vs_student_t_density.png` | Density overlay (Normal vs Student-t) |
| `qq_plots_combined.png` | Combined Q-Q plots |
| `rolling_volatility.png` | 60-day rolling volatility |
| `extreme_return_plot.png` | Extreme return thresholds |
| `clt_simulation.png` | CLT multi-panel figure |
| `monte_carlo_simulation.png` | Monte Carlo 3-model comparison |
| `portfolio_performance.png` | Cumulative portfolio performance |
| `var_comparison.png` | VaR comparison (4 methods × 3 CL) |

### Tables (output/tables/)

| File | Description |
|------|-------------|
| `cleaning_log.txt` | Data cleaning decisions |
| `cleaning_summary.csv` | Per-stock cleaning summary |
| `5stock_descriptive_stats.csv` | Descriptive statistics (all 5 stocks) |
| `representative_stock_selection.csv` | Selection criteria summary |
| `model_comparison_summary.csv` | Normal vs Student-t MLE comparison |
| `goodness_of_fit_summary.csv` | Formal goodness-of-fit results |
| `hypothesis_testing_results.csv` | t-tests, CI, skewness/kurtosis |
| `extreme_returns_summary.csv` | Extreme return values |
| `clt_summary.csv` | CLT standard error verification |
| `monte_carlo_summary.csv` | Monte Carlo results (3 models) |
| `portfolio_correlation.csv` | Portfolio correlation matrix |
| `var_table.csv` | VaR table (4 methods × 3 CL) |

---

## Team-Member Responsibilities

| Member | Responsibilities |
|--------|------------------|
| **Member 1: Data Engineering** | Data collection, cleaning, missing values, validation, return calculation, reproducibility, dataset documentation |
| **Member 2: Five-Stock Comparative Analysis** | Descriptive statistics, comparative EDA, volatility comparison, correlation analysis, boxplots, histograms, comparative visualizations, representative-stock selection |
| **Member 3: Deep Statistical Modeling** | Representative-stock analysis, distribution fitting, MLE, Normal and Student-t, AIC, BIC, goodness-of-fit, hypothesis testing, extreme-return analysis |
| **Member 4: Simulation, Portfolio & Report** | CLT simulation, Monte Carlo, portfolio construction, VaR, final integration, Quarto report, presentation, final testing |

---

## Known Limitations

1. **Limited time series:** ~5 years is a modest sample for estimating tail risk
2. **Simple portfolio weights:** Equal-weight is easy to understand but not optimal; risk-parity or minimum-variance weights are possible extensions
3. **i.i.d. assumption:** VaR and Monte Carlo assume independent, identically distributed returns (volatility clustering violates this)
4. **Sector concentration:** TCS and Infosys are both IT, so the portfolio is more IT-concentrated than the sector count suggests
5. **No transaction costs, taxes, or slippage** in the portfolio analysis
6. **Model selection is data-dependent:** AIC/BIC give relative comparisons, not absolute truths
7. **No out-of-sample validation:** All model estimates are in-sample

---

## Reproducibility Checklist

- [x] `set.seed(12345)` used for simulations
- [x] Relative paths used throughout
- [x] No hard-coded local paths
- [x] Data source documented
- [x] Retrieval date recorded
- [x] Raw data separate from processed data
- [x] Raw data never overwritten
- [x] Cleaning decisions documented
- [x] Quarto report renders successfully

---

## Viva-Style Questions (with Answers)

| Question | Short Answer |
|----------|--------------|
| Why did you choose five stocks? | To demonstrate the breadth of analysis and portfolio-level behaviour across sectors |
| Why stocks from different sectors? | To study diversification effects — assets from different sectors typically have lower correlations |
| Why use adjusted closing prices? | To account for dividends and stock splits, ensuring returns reflect true economic returns |
| Why analyze returns instead of prices? | Prices are non-stationary; returns are (approximately) stationary and suitable for statistical modelling |
| Why use log returns? | They are time-additive, (approximately) symmetric, and well-behaved for statistical inference |
| Why select one stock for deeper analysis? | The five-stock stage provides breadth; one stock provides the statistical depth needed for distribution fitting and simulation |
| What criteria select the representative stock? | >1000 observations, meaningful volatility, notable skew/kurtosis, clean data |
| What is volatility? | The standard deviation of returns (annualised: $\sigma \times \sqrt{252}$) |
| What is skewness? | A measure of asymmetry in a distribution (positive = right tail longer) |
| What is kurtosis? | A measure of tail heaviness (excess kurtosis = kurtosis - 3 for Normal) |
| What are heavy tails? | A distribution with more probability in the extremes than the Normal distribution |
| Why compare Normal and Student-t? | The Student-t has fatter tails; useful for financial returns with extreme events |
| What is MLE? | Maximum Likelihood Estimation — finding parameters that maximise the probability of observed data |
| What is AIC? | Akaike Information Criterion — measures relative model fit with parameter penalty |
| What is BIC? | Bayesian Information Criterion — like AIC but with stronger penalty for parameters |
| What is a Q-Q plot? | Quantile-Quantile plot — compares sample quantiles to theoretical quantiles |
| What is goodness-of-fit? | A test of how well a theoretical distribution matches observed data |
| What does a p-value mean? | The probability of observing data as extreme as ours if the null hypothesis is true |
| What is the CLT? | The sampling distribution of the sample mean approaches Normal as n increases |
| Does CLT mean individual returns become Normal? | No — it applies to the mean of samples, not individual observations |
| What is Monte Carlo simulation? | Using repeated random sampling to estimate distributions of outcomes |
| Why use 10,000 simulations? | To get stable, precise estimates of tail probabilities |
| What is portfolio diversification? | Combining assets to reduce overall portfolio risk |
| What is correlation? | A measure of linear relationship between two variables |
| What is Value at Risk? | A quantile-based measure of potential loss (e.g., 95% VaR = loss not exceeded on 95% of days) |
| Why do different VaR methods give different results? | They make different distributional assumptions about extreme returns |
| What are the limitations? | Short history, i.i.d. assumption, sector concentration, no transaction costs, no out-of-sample validation |

---

## References

1. **quantmod** (2026). *Quantitative Financial Modelling Framework in R*. CRAN. https://CRAN.R-project.org/package=quantmod
2. **fitdistrplus** (2024). *Help to Fit of Parametric Distributions*. CRAN. https://CRAN.R-project.org/package=fitdistrplus
3. **PerformanceAnalytics** (2024). *Performance Analytics: Econometric Tools for Performance and Risk Analysis*. CRAN. https://CRAN.R-project.org/package=PerformanceAnalytics
4. **Akaike, H. (1974).** A new look at the statistical model identification. *IEEE Transactions on Automatic Control*, 19(6), 716–723.
5. **Schwarz, G. (1978).** Estimating the dimension of a model. *Annals of Statistics*, 6(2), 461–464.
6. **Shapiro, S. S., & Wilk, M. B. (1965).** An analysis of variance test for normality (complete samples). *Biometrika*, 52(3/4), 591–611.
7. **Kolmogorov, A. N. (1933).** *Sulla determinazione empirica di una legge di distribuzione*. Giornale dell'Inst. Ital. degli Attuari, 4, 83–91.
8. **Anderson, T. W., & Darling, D. A. (1954).** A test of goodness of fit. *Journal of the American Statistical Association*, 49(268), 765–769.
9. **Lilliefors, H. W. (1967).** On the Kolmogorov-Smirnov test for normality with estimated parameters. *Journal of the American Statistical Association*, 62(318), 399–402.
10. **McNeil, A. J., Frey, R., & Embrechts, P. (2015).** *Quantitative Risk Management: Concepts, Techniques and Tools*. Princeton University Press.

---

## Setup Checklist

- [ ] Install R ≥ 4.0.0
- [ ] Install RStudio (recommended)
- [ ] Install required R packages (via `R/01_setup.R`)
- [ ] Open `stock-volatility-project.Rproj` in RStudio
- [ ] Source `R/01_setup.R`
- [ ] Run `R/02_data_collection.R` (downloads data)
- [ ] Run `R/03_data_cleaning.R` (cleans data)
- [ ] Run `R/04_return_calculation.R` (calculates returns)
- [ ] Run `R/05_comparative_analysis.R` (5-stock comparison)
- [ ] Run `R/06_representative_stock.R` (selects representative)
- [ ] Run `R/07_distribution_fitting.R` (Normal + Student-t fit)
- [ ] Run `R/08_goodness_of_fit.R` (formal tests)
- [ ] Run `R/09_hypothesis_testing.R` (t-test, CI)
- [ ] Run `R/10_extreme_returns.R` (tail analysis)
- [ ] Run `R/11_clt_simulation.R` (CLT demonstration)
- [ ] Run `R/12_monte_carlo.R` (10,000 simulations)
- [ ] Run `R/13_portfolio_analysis.R` (portfolio construction)
- [ ] Run `R/14_var_analysis.R` (VaR calculation)
- [ ] Run `R/15_visualizations.R` (all figures)
- [ ] Render `report/final_report.qmd` (Quarto report)
- [ ] Review `presentation/presentation_outline.md` (presentation)
