# ============================================================
# 00_stock_info.R — Consolidated stock metadata
# ============================================================
#
# This file defines the canonical stock metadata used across
# all scripts.  It is sourced by 01_setup.R so it is available
# in every analysis session.
#
# Metadata: ticker, company, sector, market, data source,
#           start date, end date, number of observations.
#
# WARNING: Do NOT modify the metadata here to force a particular
# stock into first place.  Metadata describes the data.

stock_info <- list(
  RELIANCE    = list(
    company       = "Reliance Industries Limited",
    sector        = "Energy / Oil & Gas",
    market        = "NSE (India)",
    data_source   = "NSE via quantmod (Yahoo Finance)",
    start_date    = as.Date("2020-01-01"),
    end_date      = as.Date("2025-12-31"),
    n_obs         = NA  # filled in by the collection script
  ),
  TCS         = list(
    company       = "Tata Consultancy Services Limited",
    sector        = "IT / Software Services",
    market        = "NSE (India)",
    data_source   = "NSE via quantmod (Yahoo Finance)",
    start_date    = as.Date("2020-01-01"),
    end_date      = as.Date("2025-12-31"),
    n_obs         = NA
  ),
  HDFCBANK    = list(
    company       = "HDFC Bank Limited",
    sector        = "Financials / Banking",
    market        = "NSE (India)",
    data_source   = "NSE via quantmod (Yahoo Finance)",
    start_date    = as.Date("2020-01-01"),
    end_date      = as.Date("2025-12-31"),
    n_obs         = NA
  ),
  INFOSYS     = list(
    company       = "Infosys Limited",
    sector        = "IT / Software Services",
    market        = "NSE (India)",
    data_source   = "NSE via quantmod (Yahoo Finance)",
    start_date    = as.Date("2020-01-01"),
    end_date      = as.Date("2025-12-31"),
    n_obs         = NA
  ),
  LT          = list(
    company       = "Larsen & Toubro Limited",
    sector        = "Manufacturing / Infrastructure",
    market        = "NSE (India)",
    data_source   = "NSE via quantmod (Yahoo Finance)",
    start_date    = as.Date("2020-01-01"),
    end_date      = as.Date("2025-12-31"),
    n_obs         = NA
  )
)

# ticker -> standardised internal symbol used in files
ticker_map <- c(
  "RELIANCE"    = "RELIANCE",
  "TCS"         = "TCS",
  "HDFCBANK"    = "HDFCBANK",
  "INFOSYS"     = "INFOSYS",
  "LT"          = "LT"
)

# sector -> colour palette for consistent visuals
sector_colour <- function(sector) {
  cols <- c(
    "Energy"      = "#D55E00",
    "IT"          = "#0072B2",
    "Financials"  = "#009E73",
    "Automotive"  = "#CC79A7",
    "Manufacturing" = "#56B4E9"
  )
  # match first word of the sector string
  first_word <- sub(" .*$", "", sector)
  if (first_word %in% names(cols)) return(cols[first_word])
  "#999999"  # grey fallback
}
