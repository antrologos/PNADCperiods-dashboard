# ==============================================================================
# De-seasonalization Utilities
# ==============================================================================
# Wrappers for X-13 ARIMA-SEATS and STL decomposition methods
# Following Marcos Hecksher's methodology with questionnaire split at 2015-Q4

# Check package availability
seasonal_available <- requireNamespace("seasonal", quietly = TRUE)
forecast_available <- requireNamespace("forecast", quietly = TRUE)

# ==============================================================================
# X-13 ARIMA with X-11 Method (following Marcos Hecksher)
# ==============================================================================

#' Apply X-13 ARIMA seasonal adjustment using X-11 method
#'
#' Uses X-11 decomposition method (not SEATS) following Marcos Hecksher's
#' original Stata implementation which uses sax12 with satype(single) and
#' extracts d11 (the X-11 seasonally adjusted series).
#'
#' @param values Numeric vector of monthly values
#' @param dates Date vector corresponding to values
#' @param split_date Date to split series (IBGE questionnaire change)
#' @return Seasonally adjusted values (same length as input)
deseasonalize_x13 <- function(values, dates, split_date = as.Date("2015-10-01")) {

  if (!seasonal_available) {
    warning("Package 'seasonal' not available. Returning original values.")
    return(values)
  }

  # Handle NA values by interpolation (X-13 doesn't like NAs)
  na_mask <- is.na(values)
  if (all(na_mask)) return(values)

  # Create result vector
  result <- rep(NA_real_, length(values))

  # Order by date
  ord <- order(dates)
  values_ord <- values[ord]
  dates_ord <- dates[ord]

  # Find split point
  split_idx <- which(dates_ord >= split_date)[1]

  # Process periods separately (before and after questionnaire change)
  process_period <- function(vals, start_date) {
    if (length(vals) < 24 || all(is.na(vals))) {
      return(vals)  # Not enough data for seasonal adjustment
    }

    # Linear interpolation for internal NAs
    vals_clean <- vals
    na_idx <- which(is.na(vals_clean))
    if (length(na_idx) > 0 && length(na_idx) < length(vals_clean)) {
      non_na_idx <- which(!is.na(vals_clean))
      vals_clean[na_idx] <- approx(non_na_idx, vals_clean[non_na_idx], xout = na_idx)$y
    }

    # Create time series object
    start_year <- as.numeric(format(start_date, "%Y"))
    start_month <- as.numeric(format(start_date, "%m"))

    ts_obj <- ts(vals_clean, frequency = 12, start = c(start_year, start_month))

    # Apply X-13 ARIMA with X-11 method (following Marcos Hecksher's approach)
    # Using x11 = "" forces X-11 decomposition instead of SEATS
    # This matches Marcos's Stata: sax12 var, satype(single) / sax12im var, ext(d11)
    tryCatch({
      m <- seasonal::seas(ts_obj, x11 = "")
      adjusted <- as.numeric(seasonal::final(m))

      # ========================================================================
      # VALIDATION: Detect silent X-13 failures
      # X-13 can produce degenerate models that return without error but have
      # constant/flatline values, flat seasonal component, or excessive NAs
      # ========================================================================

      # Note: We removed the "trivial seasonal" variance check because X-11 uses
      # different model types (multiplicative vs additive) for different periods.
      # Multiplicative models have seasonal factors around 1.0, which have
      # inherently low variance even when seasonality is meaningful.
      # X-11 method is robust and will always extract a seasonal component,
      # so we trust its output without additional variance checks.

      # Check 1: All values identical or nearly identical (flatline)
      unique_vals <- length(unique(round(adjusted, 6)))
      if (unique_vals <= 3) {
        warning("X-13 ARIMA produced constant adjusted values - returning original")
        return(vals)
      }

      # Check 2: Too many NAs in output
      na_ratio <- sum(is.na(adjusted)) / length(adjusted)
      if (na_ratio > 0.1) {
        warning(paste0(
          "X-13 ARIMA produced too many NAs (",
          round(na_ratio * 100, 1), "%) - returning original"
        ))
        return(vals)
      }

      # Check 3: Variance anomaly - adjusted variance should be roughly similar
      # to original (within reasonable bounds)
      var_orig <- var(vals_clean, na.rm = TRUE)
      var_adj <- var(adjusted, na.rm = TRUE)
      if (var_orig > 0 && (var_adj < var_orig / 100 || var_adj > var_orig * 100)) {
        warning(paste0(
          "X-13 ARIMA variance anomaly (orig=", round(var_orig, 2),
          ", adj=", round(var_adj, 2), ") - returning original"
        ))
        return(vals)
      }

      return(adjusted)
    }, error = function(e) {
      warning(paste("X-13 ARIMA failed:", e$message, "- returning original values"))
      return(vals)
    })
  }

  # Period 1: Before questionnaire change (if we have data)
  if (!is.na(split_idx) && split_idx > 1) {
    idx1 <- 1:(split_idx - 1)
    vals1 <- values_ord[idx1]
    adjusted1 <- process_period(vals1, dates_ord[1])
    result[ord[idx1]] <- adjusted1
  }

  # Period 2: After questionnaire change (main period)
  if (!is.na(split_idx) && split_idx <= length(values_ord)) {
    idx2 <- split_idx:length(values_ord)
    vals2 <- values_ord[idx2]
    adjusted2 <- process_period(vals2, dates_ord[split_idx])
    result[ord[idx2]] <- adjusted2
  } else if (is.na(split_idx)) {
    # All data is before split (or split_idx is beyond data)
    adjusted <- process_period(values_ord, dates_ord[1])
    result[ord] <- adjusted
  }

  return(result)
}


# ==============================================================================
# STL Decomposition Wrapper
# ==============================================================================

#' Apply STL decomposition seasonal adjustment
#'
#' @param values Numeric vector of monthly values
#' @param dates Date vector corresponding to values
#' @return Seasonally adjusted values (same length as input)
deseasonalize_stl <- function(values, dates) {

  if (!forecast_available) {
    warning("Package 'forecast' not available. Returning original values.")
    return(values)
  }

  # Handle NA values
  na_mask <- is.na(values)
  if (all(na_mask)) return(values)

  # Need at least 2 full years for STL
  if (sum(!na_mask) < 24) {
    warning("Insufficient data for STL decomposition (need 24+ observations)")
    return(values)
  }

  # Order by date
  ord <- order(dates)
  values_ord <- values[ord]
  dates_ord <- dates[ord]

  # Linear interpolation for internal NAs
  vals_clean <- values_ord
  na_idx <- which(is.na(vals_clean))
  if (length(na_idx) > 0 && length(na_idx) < length(vals_clean)) {
    non_na_idx <- which(!is.na(vals_clean))
    vals_clean[na_idx] <- approx(non_na_idx, vals_clean[non_na_idx], xout = na_idx)$y
  }

  # Create time series
  start_year <- as.numeric(format(dates_ord[1], "%Y"))
  start_month <- as.numeric(format(dates_ord[1], "%m"))
  ts_obj <- ts(vals_clean, frequency = 12, start = c(start_year, start_month))

  # Apply STL
  result <- values  # Default to original values
  tryCatch({
    stl_result <- forecast::mstl(ts_obj)
    adjusted <- forecast::seasadj(stl_result)
    result[ord] <- as.numeric(adjusted)
  }, error = function(e) {
    warning(paste("STL decomposition failed:", e$message))
  })

  return(result)
}


# ==============================================================================
# Main De-seasonalization Function
# ==============================================================================

#' Apply seasonal adjustment to a series
#'
#' @param values Numeric vector of monthly values
#' @param dates Date vector corresponding to values
#' @param method Character: "none", "x13", "stl", or "both"
#' @return If method is "both", returns a list with $x13 and $stl components.
#'         Otherwise returns a numeric vector.
deseasonalize_series <- function(values, dates, method = "none") {

  if (method == "none" || is.null(method)) {
    return(values)
  }

  if (method == "x13") {
    return(deseasonalize_x13(values, dates))
  }

  if (method == "stl") {
    return(deseasonalize_stl(values, dates))
  }

  if (method == "both") {
    return(list(
      x13 = deseasonalize_x13(values, dates),
      stl = deseasonalize_stl(values, dates)
    ))
  }

  warning(paste("Unknown method:", method, "- returning original values"))
  return(values)
}


# ==============================================================================
# Check if De-seasonalization is Available
# ==============================================================================

#' Check which de-seasonalization methods are available
#' @return Named logical vector
check_deseasonalization_available <- function() {
  c(
    x13 = seasonal_available,
    stl = forecast_available
  )
}


# ==============================================================================
# Batch De-seasonalization for All Series
# ==============================================================================

#' De-seasonalize multiple series in a data.table
#'
#' @param dt data.table with date column and series columns
#' @param date_col Name of the date column
#' @param series_cols Character vector of series column names to de-seasonalize
#' @param method Character: "x13", "stl", or "both"
#' @param suffix Suffix to add to de-seasonalized column names
#' @return data.table with added de-seasonalized columns
deseasonalize_batch <- function(dt, date_col = "date", series_cols = NULL,
                                method = "x13", suffix = "_sa") {

  if (is.null(series_cols)) {
    # Get all numeric columns except date
    series_cols <- setdiff(names(dt)[sapply(dt, is.numeric)], date_col)
  }

  dates <- dt[[date_col]]
  result <- copy(dt)

  for (col in series_cols) {
    values <- dt[[col]]
    adjusted <- deseasonalize_series(values, dates, method)

    if (is.list(adjusted)) {
      # "both" method returns list
      result[[paste0(col, "_x13")]] <- adjusted$x13
      result[[paste0(col, "_stl")]] <- adjusted$stl
    } else {
      result[[paste0(col, suffix)]] <- adjusted
    }
  }

  return(result)
}
