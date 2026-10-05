# Presentation Outline — 15 Minutes

**Title:** Statistical Modeling of Stock Market Volatility, Extreme Returns, and the Central Limit Theorem

**Duration:** 15 minutes (+ 5 minutes Q&A)
**Audience:** Undergraduate statistics/probability course

---

## Slide 1 — Title (30 seconds)
- Title and team names
- Course / project type

## Slide 2 — Motivation (1 minute)
- Why study stock-return distributions?
- The "Normal vs Real" contrast: returns are skewed and heavy-tailed

## Slide 3 — Problem Statement (1 minute)
- The central research question
- Why distribution choice matters for risk estimation

## Slide 4 — Research Questions (1 minute)
- 10 questions summarized (5–6 bullets max)

## Slide 5 — Dataset: Five Stocks (1.5 minutes)
- Table: ticker, company, sector, market, period
- Reasoning for sector diversity

## Slide 6 — Overall Methodology (1 minute)
- The architecture diagram (5 stocks → comparative → representative → deep analysis → return to 5 stocks → portfolio → VaR)
- Data cleaning philosophy: no auto-removal of extremes

## Slide 7 — Five-Stock Comparative Analysis (1.5 minutes)
- Key statistics table (mean, SD, vol, skew, kurt)
- Boxplots and volatility chart
- Takeaway: stocks are different; IT stocks cluster

## Slide 8 — Representative Stock Selection (1.5 minutes)
- Objective criteria (>1000 obs, meaningful vol, skew/kurt, clean data)
- Chosen stock: TCS
- Why NOT chosen for dramatic results

## Slide 9 — Distribution Modeling (2 minutes)
- Normal MLE: μ, σ
- Student-t MLE: μ, σ, ν
- Density overlay
- Comparative visualization

## Slide 10 — Normal vs Student-t (1.5 minutes)
- LogLik, AIC, BIC table
- Interpretation: relative fit only
- Q-Q plots: sides

## Slide 11 — Extreme Returns (1.5 minutes)
- 1st, 5th, 95th, 99th percentiles
- Largest gains / losses
- Observed tail vs Normal vs Student-t (simulation)

## Slide 12 — CLT Demonstration (1.5 minutes)
- Setup: n = 5, 10, 30, 50, 100; 5000 reps
- Panel figure
- Key observation: SE ≈ σ/√n; skewness → 0

## Slide 13 — Monte Carlo Simulation (1.5 minutes)
- Three models: Normal, Student-t, Empirical
- Comparison table
- Label: model-based results, not forecasts

## Slide 14 — Five-Stock Portfolio (1.5 minutes)
- Equal weights (20% each)
- Portfolio volatility vs average individual vol
- Correlation matrix
- Diversification conclusion

## Slide 15 — VaR Comparison (1.5 minutes)
- Four methods × three confidence levels
- Representative stock vs portfolio
- Interpretation: VaR is a quantile, NOT a maximum loss

## Slide 16 — Key Findings (1 minute)
- 5 bullet summary of results

## Slide 17 — Limitations (1 minute)
- Short data history, i.i.d. assumption, sector concentration, no transaction costs

## Slide 18 — Conclusion (1 minute)
- Recap of the central story
- Answer to the research question

## Slide 19 — Future Work (1 minute)
- Longer history, GARCH, out-of-sample VaR testing, risk-parity weights

## Slide 20 — Questions (time remaining)
- Open floor

---

# Design Guidelines
- 10–12 words per slide maximum
- Use visuals (figures)
- One idea per slide
- Use consistent colour scheme
