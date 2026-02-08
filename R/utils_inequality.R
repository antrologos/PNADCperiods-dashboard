# ==============================================================================
# PNADCperiods Dashboard - Inequality & Poverty Utility Functions
# ==============================================================================
#
# Lightweight functions for computing inequality and poverty measures.
# Used by both precompute scripts and dashboard modules.
#
# Based on methodology from:
#   - Foster, Greer & Thorbecke (1984) - FGT poverty family
#   - Lerman & Yitzhaki (1985) - Gini decomposition by income source
#
# All functions accept survey weights and handle NAs.
# ==============================================================================


# ==============================================================================
# Core Inequality Measures
# ==============================================================================

#' Weighted Gini coefficient
#'
#' Computes the Gini coefficient using the covariance formula with survey weights.
#' Handles NAs by removing incomplete cases.
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of survey weights (default: equal weights)
#' @return Scalar Gini coefficient (0 = perfect equality, 1 = perfect inequality)
weighted_gini <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))

  # Remove NAs
  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) < 2) return(NA_real_)

  # Sort by income
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  # Cumulative weight (normalized)
  cum_w <- cumsum(w)
  N <- sum(w)
  cum_w_norm <- cum_w / N

  # Weighted mean
  mu <- sum(x * w) / N

  if (mu == 0) return(NA_real_)

  # Gini via covariance method: G = (2/mu) * cov(x, F(x))
  # where F(x) is the cumulative distribution
  # Equivalent: G = (2 * sum(w * x * cum_w_norm)) / (N * mu) - 1
  gini <- (2 * sum(w * x * cum_w_norm)) / (N * mu) - 1

  # Ensure valid range
  max(0, min(1, gini))
}


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


#' Lorenz curve points
#'
#' Returns coordinates of the Lorenz curve sampled at n equally-spaced
#' population quantiles.
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of survey weights
#' @param n Number of points to sample (default: 100)
#' @return data.table with columns: p (population share), lorenz (income share)
lorenz_points <- function(x, w = NULL, n = 100) {
  if (is.null(w)) w <- rep(1, length(x))

  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) < 2) {
    return(data.table::data.table(
      p = seq(0, 1, length.out = n + 1),
      lorenz = seq(0, 1, length.out = n + 1)
    ))
  }

  # Sort by income
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  # Cumulative shares
  cum_w <- cumsum(w)
  cum_wx <- cumsum(w * x)
  p_cum <- cum_w / sum(w)
  l_cum <- cum_wx / sum(w * x)

  # Add origin
  p_cum <- c(0, p_cum)
  l_cum <- c(0, l_cum)

  # Interpolate at equally-spaced points
  p_grid <- seq(0, 1, length.out = n + 1)
  l_grid <- stats::approx(x = p_cum, y = l_cum, xout = p_grid,
                           method = "linear", yleft = 0, yright = 1)$y

  data.table::data.table(p = p_grid, lorenz = l_grid)
}


# ==============================================================================
# Percentile-Based Measures
# ==============================================================================

#' Weighted quantile function
#'
#' @param x Numeric vector
#' @param w Numeric vector of weights
#' @param probs Numeric vector of probabilities (0-1)
#' @return Numeric vector of quantile values
weighted_quantile <- function(x, w = NULL, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  if (is.null(w)) w <- rep(1, length(x))

  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) == 0) return(rep(NA_real_, length(probs)))

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  cum_w <- cumsum(w) / sum(w)

  sapply(probs, function(p) {
    i <- which(cum_w >= p)[1]
    if (is.na(i)) return(x[length(x)])
    x[i]
  })
}


#' Income shares by quantile groups
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of weights
#' @param groups Number of groups (5=quintiles, 10=deciles)
#' @return data.table with group_label and share columns
income_shares <- function(x, w = NULL, groups = 10) {
  if (is.null(w)) w <- rep(1, length(x))

  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) == 0) {
    labels <- if (groups == 5) paste0("Q", 1:5) else paste0("D", 1:groups)
    return(data.table::data.table(group_label = labels, share = NA_real_))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  cum_w <- cumsum(w) / sum(w)
  total_income <- sum(x * w)

  breaks <- seq(0, 1, length.out = groups + 1)
  labels <- if (groups == 5) {
    paste0("Q", 1:5)
  } else if (groups == 10) {
    paste0("D", 1:10)
  } else {
    paste0("G", 1:groups)
  }

  shares <- numeric(groups)
  for (g in seq_len(groups)) {
    lower <- breaks[g]
    upper <- breaks[g + 1]
    if (lower == 0) {
      in_group <- cum_w <= upper
    } else {
      in_group <- cum_w > lower & cum_w <= upper
    }
    shares[g] <- sum(x[in_group] * w[in_group]) / total_income
  }

  data.table::data.table(group_label = labels, share = shares)
}


#' Palma ratio (Top 10% income share / Bottom 40% income share)
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of weights
#' @return Scalar Palma ratio
palma_ratio <- function(x, w = NULL) {
  shares <- income_shares(x, w, groups = 10)
  if (any(is.na(shares$share))) return(NA_real_)

  top10 <- shares$share[10]
  bottom40 <- sum(shares$share[1:4])

  if (bottom40 == 0) return(NA_real_)
  top10 / bottom40
}


#' General percentile ratio
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of weights
#' @param p_high Upper percentile (e.g., 0.9)
#' @param p_low Lower percentile (e.g., 0.1)
#' @return Scalar ratio
percentile_ratio <- function(x, w = NULL, p_high = 0.9, p_low = 0.1) {
  q <- weighted_quantile(x, w, probs = c(p_low, p_high))
  if (any(is.na(q)) || q[1] == 0) return(NA_real_)
  q[2] / q[1]
}


#' Top k% income share
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of weights
#' @param k Percentage (e.g., 1 for top 1%, 10 for top 10%)
#' @return Scalar share (0-1)
top_share <- function(x, w = NULL, k = 10) {
  if (is.null(w)) w <- rep(1, length(x))

  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) == 0) return(NA_real_)

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  threshold <- 1 - k / 100
  cum_w <- cumsum(w) / sum(w)
  in_top <- cum_w > threshold

  sum(x[in_top] * w[in_top]) / sum(x * w)
}


#' Bottom k% income share
#'
#' @param x Numeric vector of incomes
#' @param w Numeric vector of weights
#' @param k Percentage (e.g., 50 for bottom 50%)
#' @return Scalar share (0-1)
bottom_share <- function(x, w = NULL, k = 50) {
  if (is.null(w)) w <- rep(1, length(x))

  idx <- complete.cases(x, w)
  x <- x[idx]
  w <- w[idx]

  if (length(x) == 0) return(NA_real_)

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  threshold <- k / 100
  cum_w <- cumsum(w) / sum(w)
  in_bottom <- cum_w <= threshold

  sum(x[in_bottom] * w[in_bottom]) / sum(x * w)
}


# ==============================================================================
# Income Decomposition
# ==============================================================================

#' Concentration coefficient
#'
#' Computes the concentration coefficient of income source x
#' with respect to total income ranking y.
#'
#' @param x Numeric vector (income source)
#' @param y Numeric vector (total income, used for ranking)
#' @param w Numeric vector of weights
#' @return Scalar concentration coefficient
concentration_coeff <- function(x, y, w = NULL) {
  if (is.null(w)) w <- rep(1, length(y))

  idx <- complete.cases(x, y, w)
  x <- x[idx]; y <- y[idx]; w <- w[idx]

  if (length(x) < 2) return(NA_real_)

  # Sort by total income (y)
  ord <- order(y)
  x <- x[ord]
  w <- w[ord]

  # Cumulative weight and cumulative x
  cum_w <- cumsum(w)
  cum_wx <- cumsum(w * x)
  N <- sum(w)
  total_x <- sum(w * x)

  if (total_x == 0) return(0)

  p_cum <- cum_w / N
  l_cum <- cum_wx / total_x

  # Add origin
  p_cum <- c(0, p_cum)
  l_cum <- c(0, l_cum)

  # Area under concentration curve (trapezoidal)
  area <- sum(diff(p_cum) * (l_cum[-1] + l_cum[-length(l_cum)]) / 2)

  # C = 2 * (0.5 - area)
  2 * (0.5 - area)
}


#' Gini decomposition by income source
#'
#' Decomposes the Gini coefficient into contributions from each income source
#' using the Lerman-Yitzhaki (1985) method.
#'
#' @param dt data.table with income columns and weight column
#' @param income_vars Character vector of income source column names
#' @param total_var Character string: name of total income column
#' @param weight_var Character string: name of weight column
#' @return data.table with columns: income_source, concentration_coeff,
#'   income_share, contribution_to_gini
gini_decomposition <- function(dt, income_vars, total_var, weight_var) {
  y <- dt[[total_var]]
  w <- dt[[weight_var]]

  results <- lapply(income_vars, function(var) {
    x <- dt[[var]]

    cc <- concentration_coeff(x, y, w)

    # Income share = sum(w*x) / sum(w*y)
    total_x <- sum(w * x, na.rm = TRUE)
    total_y <- sum(w * y, na.rm = TRUE)
    share <- if (total_y > 0) total_x / total_y else 0

    data.table::data.table(
      income_source = var,
      concentration_coeff = cc,
      income_share = share,
      conc_x_share = cc * share
    )
  })

  result <- data.table::rbindlist(results)

  # Contribution = (C_k * S_k) / sum(C_k * S_k)
  total_gini_approx <- sum(result$conc_x_share, na.rm = TRUE)
  result[, contribution_to_gini := if (total_gini_approx != 0)
    conc_x_share / total_gini_approx else NA_real_]
  result[, conc_x_share := NULL]

  result
}


# ==============================================================================
# Poverty Helper Functions
# ==============================================================================

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


# ==============================================================================
# Demographic Grouping Helpers
# ==============================================================================

#' UF code to region mapping
#'
#' @param uf Numeric UF code
#' @return Character region name
uf_to_region <- function(uf) {
  uf <- as.numeric(uf)
  data.table::fcase(
    uf %in% c(11, 12, 13, 14, 15, 16, 17), "Norte",
    uf %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29), "Nordeste",
    uf %in% c(31, 32, 33, 35), "Sudeste",
    uf %in% c(41, 42, 43), "Sul",
    uf %in% c(50, 51, 52, 53), "Centro-Oeste",
    default = NA_character_
  )
}

#' UF code to region name (bilingual)
#'
#' @param uf Numeric UF code
#' @param lang Language code
#' @return Character region name
uf_to_region_i18n <- function(uf, lang = "pt") {
  region_pt <- uf_to_region(uf)
  if (lang == "en") {
    data.table::fcase(
      region_pt == "Norte", "North",
      region_pt == "Nordeste", "Northeast",
      region_pt == "Sudeste", "Southeast",
      region_pt == "Sul", "South",
      region_pt == "Centro-Oeste", "Center-West",
      default = NA_character_
    )
  } else {
    region_pt
  }
}


#' UF code to state abbreviation mapping
uf_to_abbrev <- function(uf) {
  uf <- as.numeric(uf)
  uf_map <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )
  uf_map[as.character(uf)]
}


#' Create age group variable
#'
#' @param age Numeric age vector
#' @return Character age group
age_group <- function(age) {
  data.table::fcase(
    age <= 17, "0-17",
    age <= 29, "18-29",
    age <= 59, "30-59",
    age >= 60, "60+",
    default = NA_character_
  )
}

#' Create education group variable
#'
#' @param vd3004 PNADC education level variable
#' @return Character education group
education_group <- function(vd3004) {
  vd3004 <- as.numeric(vd3004)
  data.table::fcase(
    vd3004 == 1, "Sem instrucao",
    vd3004 == 2, "Fundamental",
    vd3004 %in% c(3, 4), "Medio",
    vd3004 %in% c(5, 6, 7), "Superior",
    default = NA_character_
  )
}

#' Create sex label
#'
#' @param v2007 PNADC sex variable (1=male, 2=female)
#' @return Character sex label
sex_label <- function(v2007) {
  v2007 <- as.numeric(v2007)
  data.table::fcase(
    v2007 == 1, "Homens",
    v2007 == 2, "Mulheres",
    default = NA_character_
  )
}

#' Create race/color label
#'
#' @param v2010 PNADC race/color variable
#' @return Character race label
race_label <- function(v2010) {
  v2010 <- as.numeric(v2010)
  data.table::fcase(
    v2010 == 1, "Branca",
    v2010 == 2, "Preta",
    v2010 == 3, "Amarela",
    v2010 == 4, "Parda",
    v2010 == 5, "Indigena",
    default = NA_character_
  )
}

#' Create urban/rural label
#'
#' @param v1022 PNADC urban/rural variable (1=urban, 2=rural)
#' @return Character label
urban_rural_label <- function(v1022) {
  v1022 <- as.numeric(v1022)
  data.table::fcase(
    v1022 == 1, "Urbano",
    v1022 == 2, "Rural",
    default = NA_character_
  )
}
