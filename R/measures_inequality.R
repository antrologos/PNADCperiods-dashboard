# ==============================================================================
# Inequality Measures
# ==============================================================================
#
# Weighted statistical measures for income inequality. All functions accept
# survey weights and handle NAs by complete-cases filtering.
#
# Methodology references:
#   - Lerman & Yitzhaki (1985) - Gini decomposition by income source
#   - Foster, Greer & Thorbecke (1984) - Note: FGT family lives in
#     measures_poverty.R
#
# Split from utils_inequality.R (PR2): consumed by build_inequality_outputs
# only. Changes here do NOT invalidate prepared_microdata or poverty_asset.
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

  # Degenerate group: all incomes zero (or non-positive total) → would
  # produce NaN cumulative shares and crash stats::approx with
  # "são precisos ao menos dois valores não-NA para interpolar".
  total_wx <- sum(w * x)
  if (!is.finite(total_wx) || total_wx <= 0) {
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
  l_cum <- cum_wx / total_wx

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
