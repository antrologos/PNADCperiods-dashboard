# ==============================================================================
# utils_sidra_mask.R — workaround for PNADCperiods <= 0.1.1 phantom values
# ==============================================================================
#
# When the SIDRA rolling-quarter input has a trailing NA in some series (e.g.,
# SIDRA published IPCA/INPC for month N before IBGE published the PNADC
# rolling quarter for that same month), `mensalize_sidra_series()` in
# PNADCperiods <= 0.1.1 emits a numerically plausible but spurious value:
#
#   .compute_cumsum_by_mesnotrim() does `cumsum(ifelse(is.na(d3m_k), 0, ...))`,
#   silently coercing NA -> 0. The cum<mesnotrim> for that position freezes at
#   its last observed value, then .apply_final_adjustment() falls back to
#   `m <- y` (because rq leads/lags are NA). Result: m[t] = y0[mesnotrim] +
#   cum_frozen, which numerically equals the value 3 months earlier in the
#   same mesnotrim position. In a unemployment series, this looks like the
#   curve "rewinding" by 3 months without warning.
#
# Fixed in PNADCperiods commits f39279c + cf23849 (post-0.1.1, NOT yet on
# CRAN). This file replicates the same masking outside the package so the
# dashboard release and runtime are protected.
#
# Remove this file and its 2 call sites (.github/scripts/fetch_sidra_daily.R
# Step 3.5; global.R::load_app_data() right after the SIDRA load loop) once
# PNADCperiods 0.1.2+ is on CRAN.
# ==============================================================================

#' Mask phantom mensalized values in monthly_sidra.
#'
#' For each `m_<series>` column in `monthly_sidra`, find the matching
#' `<series>` column in `rolling_quarters`, locate the last
#' `anomesfinaltrimmovel` where it is non-NA, and set
#' `monthly_sidra[anomesexato > last_obs, m_<series>] <- NA_real_`.
#'
#' Idempotent: if the series has no NA in `rolling_quarters`, no change.
#' Robust: if a `m_<series>` lacks a matching column in `rolling_quarters`,
#' it is left untouched.
#'
#' @param monthly_sidra data.table with `anomesexato` and `m_*` columns.
#' @param rolling_quarters data.table with `anomesfinaltrimmovel` and
#'   un-prefixed series columns (e.g., `taxadesocup`).
#' @return data.table identical to `monthly_sidra` except phantom positions
#'   are NA. Returns `monthly_sidra` unchanged if either argument is NULL.
mask_phantom_mensalized <- function(monthly_sidra, rolling_quarters) {
  if (is.null(monthly_sidra) || is.null(rolling_quarters)) {
    return(monthly_sidra)
  }
  ms <- data.table::as.data.table(data.table::copy(monthly_sidra))
  rq <- data.table::as.data.table(rolling_quarters)
  m_cols <- grep("^m_", names(ms), value = TRUE)
  for (mc in m_cols) {
    rc <- sub("^m_", "", mc)
    if (!rc %in% names(rq)) next
    obs_idx <- !is.na(rq[[rc]])
    if (!any(obs_idx)) next
    last_obs <- max(rq$anomesfinaltrimmovel[obs_idx])
    ms[anomesexato > last_obs, (mc) := NA_real_]
  }
  ms[]
}

#' Count phantom mensalized cells.
#'
#' Sentinel diagnostic: returns the number of `(t, m_<series>)` cells where
#' `m_<series>[t]` is non-NA but the corresponding `rq_<series>[t]` is NA.
#' Used by the SIDRA Action to fail the build if any phantom slips through
#' the mask.
#'
#' @inheritParams mask_phantom_mensalized
#' @return integer count of phantom cells.
count_phantom_mensalized <- function(monthly_sidra, rolling_quarters) {
  if (is.null(monthly_sidra) || is.null(rolling_quarters)) return(0L)
  ms <- data.table::as.data.table(monthly_sidra)
  rq <- data.table::as.data.table(rolling_quarters)
  total <- 0L
  m_cols <- grep("^m_", names(ms), value = TRUE)
  for (mc in m_cols) {
    rc <- sub("^m_", "", mc)
    if (!rc %in% names(rq)) next
    obs_idx <- !is.na(rq[[rc]])
    if (!any(obs_idx)) next
    last_obs <- max(rq$anomesfinaltrimmovel[obs_idx])
    total <- total + sum(!is.na(ms[anomesexato > last_obs, get(mc)]))
  }
  total
}
