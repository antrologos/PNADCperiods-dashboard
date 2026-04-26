# ==============================================================================
# Poverty Measures
# ==============================================================================
#
# FGT poverty family (Foster-Greer-Thorbecke 1984) + reference poverty lines.
# All FGT functions accept survey weights and handle NAs by complete-cases
# filtering.
#
# Split from utils_inequality.R (PR2): consumed by build_poverty_outputs only.
# Changes here do NOT invalidate prepared_microdata or inequality_assets.
#
# Note: get_wb_poverty_lines retains the deflateBR::inpc call as a fallback
# for callers that don't have an inpc_factor_table available (e.g., legacy
# scripts, dashboard offline mode). The targets pipeline computes WB lines
# inline in build_poverty_outputs using the L1 inpc_factor_table to avoid
# the redundant API call.
# ==============================================================================


# ==============================================================================
# FGT family
# ==============================================================================

#' FGT poverty measure family (Foster-Greer-Thorbecke, 1984)
#'
#' @param x Numeric vector of incomes
#' @param z Poverty line (scalar or vector)
#' @param w Numeric vector of survey weights (default: equal weights)
#' @param alpha FGT parameter: 0=headcount, 1=gap, 2=severity
#' @return Scalar FGT value
fgt <- function(x, z, w = NULL, alpha = 0) {
  if (is.null(w)) w <- rep(1, length(x))
  if (length(z) == 1) z <- rep(z, length(x))

  idx <- complete.cases(x, z, w)
  x <- x[idx]; z <- z[idx]; w <- w[idx]

  if (length(x) == 0) return(NA_real_)

  g <- pmax(0, (z - x) / z)
  fgt_val <- ifelse(x < z, g^alpha, 0)

  sum(w * fgt_val) / sum(w)
}


#' Compute all FGT measures at once
#'
#' @param x Numeric vector of incomes
#' @param z Poverty line
#' @param w Numeric vector of weights
#' @return Named list with fgt0, fgt1, fgt2, n_poor, total_pop, mean_income_poor
fgt_all <- function(x, z, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  if (length(z) == 1) z <- rep(z, length(x))

  idx <- complete.cases(x, z, w)
  x <- x[idx]; z <- z[idx]; w <- w[idx]

  if (length(x) == 0) {
    return(list(fgt0 = NA_real_, fgt1 = NA_real_, fgt2 = NA_real_,
                n_poor = NA_real_, total_pop = NA_real_,
                mean_income_poor = NA_real_))
  }

  poor <- x < z
  g <- pmax(0, (z - x) / z)

  total_w <- sum(w)
  poor_w <- sum(w[poor])

  list(
    fgt0 = sum(w * ifelse(poor, 1, 0)) / total_w,
    fgt1 = sum(w * ifelse(poor, g, 0)) / total_w,
    fgt2 = sum(w * ifelse(poor, g^2, 0)) / total_w,
    n_poor = poor_w,
    total_pop = total_w,
    mean_income_poor = if (poor_w > 0)
      sum(x[poor] * w[poor]) / poor_w else NA_real_
  )
}


# ==============================================================================
# Historical Minimum Wage Table (Brazil)
# ==============================================================================

#' Get historical minimum wage values for Brazil
#'
#' Returns a data.table with the nominal minimum wage for each year.
#' Values are the minimum wage effective for the majority of the year.
#'
#' @return data.table with columns: ano, salario_minimo
get_historical_minimum_wage <- function() {
  data.table::data.table(
    ano = 2012:2025,
    salario_minimo = c(
      622,   # 2012 (Jan: R$ 622)
      678,   # 2013 (Jan: R$ 678)
      724,   # 2014 (Jan: R$ 724)
      788,   # 2015 (Jan: R$ 788)
      880,   # 2016 (Jan: R$ 880)
      937,   # 2017 (Jan: R$ 937)
      954,   # 2018 (Jan: R$ 954)
      998,   # 2019 (Jan: R$ 998)
      1045,  # 2020 (Feb: R$ 1.045)
      1100,  # 2021 (Jan: R$ 1.100)
      1212,  # 2022 (Jan: R$ 1.212)
      1320,  # 2023 (Jan: R$ 1.320)
      1412,  # 2024 (Jan: R$ 1.412)
      1518   # 2025 (Jan: R$ 1.518)
    )
  )
}


# ==============================================================================
# World Bank Poverty Lines (June 2025 Update, 2021 PPP)
# ==============================================================================

#' Get World Bank poverty lines in monthly BRL
#'
#' Converts World Bank daily PPP poverty lines to monthly BRL values.
#' Uses 2021 PPP conversion factor for Brazil (private consumption).
#'
#' Lines from June 2025 update:
#'   - Extreme poverty: $3.00/day PPP 2021
#'   - Lower-middle income: $4.20/day PPP 2021
#'   - Upper-middle income: $8.30/day PPP 2021
#'
#' @param reference_date Character string for INPC deflation target (e.g., "12/2025")
#' @return data.table with columns: line_id, usd_per_day, brl_monthly_2021, brl_monthly_ref
get_wb_poverty_lines <- function(reference_date = "12/2025") {
  # 2021 PPP conversion factor for Brazil (private consumption)
  # Source: https://data.worldbank.org/indicator/PA.NUS.PRVT.PP
  ppp_factor <- 2.45
  days_to_month <- 365 / 12

  lines <- data.table::data.table(
    line_id = c("wb_300", "wb_420", "wb_830"),
    label_en = c("Extreme Poverty ($3.00/day)",
                 "Lower-Middle ($4.20/day)",
                 "Upper-Middle ($8.30/day)"),
    label_pt = c("Pobreza Extrema ($3,00/dia)",
                 "Renda Média-Baixa ($4,20/dia)",
                 "Renda Média-Alta ($8,30/dia)"),
    usd_per_day = c(3.00, 4.20, 8.30)
  )

  # Convert to monthly BRL at 2021 prices
  lines[, brl_monthly_2021 := usd_per_day * ppp_factor * days_to_month]

  # Deflate to reference date using INPC
  if (requireNamespace("deflateBR", quietly = TRUE)) {
    lines[, brl_monthly_ref := deflateBR::inpc(
      brl_monthly_2021,
      nominal_dates = as.Date("2021-07-01"),
      real_date = reference_date
    )]
  } else {
    warning("deflateBR package not available. Using 2021 BRL values without INPC adjustment.")
    lines[, brl_monthly_ref := brl_monthly_2021]
  }

  lines
}
