# ==============================================================================
# tar-deflation.R — Phase 2 IPCA-based deflation
#
# Replaces the previous IBGE-XLS pipeline (CO1/CO2/CO3 per UF, plus
# inpc_factor for the year-end target). The single-national IPCA index is
# applied to every observation regardless of UF.
#
# Two flavours of income variable share this helper:
#   - "habitual" — nominal value refers to the OBSERVATION month
#   - "efetivo"  — nominal value refers to the MONTH BEFORE the observation
#                  (PNADC convention: efetivo recall is one month prior)
#
# The deflation target T_ref is the LAST month available in the income
# series being deflated (max(obs_yyyymm)). This means each tar_make()
# advances T_ref as new microdata arrives, and historical real values are
# rebased to the new T_ref. Stored in microdata_log.json for auditability.
# ==============================================================================

#' Compute IPCA-based deflator vector for a column of observation months.
#'
#' @param obs_yyyymm integer vector — observation month in YYYYMM format
#'   (one entry per microdata row).
#' @param ipca_table data.table(yyyymm, ipca_index) — typically the
#'   `ipca_index_table` target. Must cover all `obs_yyyymm` values plus the
#'   the previous-month lookups when kind == "efetivo".
#' @param kind "habitual" or "efetivo".
#' @return list(deflator = numeric vector, T_ref = integer scalar).
#'   The caller applies `deflator` to the nominal column.
deflate_ipca <- function(obs_yyyymm, ipca_table,
                         kind = c("habitual", "efetivo")) {
  kind <- match.arg(kind)
  if (!data.table::is.data.table(ipca_table)) {
    ipca_table <- data.table::as.data.table(ipca_table)
  }
  if (!all(c("yyyymm", "ipca_index") %in% names(ipca_table))) {
    stop("deflate_ipca: ipca_table must have columns 'yyyymm' and 'ipca_index'",
         call. = FALSE)
  }
  if (!length(obs_yyyymm) || all(is.na(obs_yyyymm))) {
    stop("deflate_ipca: obs_yyyymm is empty or all NA", call. = FALSE)
  }

  T_ref <- as.integer(max(obs_yyyymm, na.rm = TRUE))
  ipca_T_hits <- ipca_table[yyyymm == T_ref, ipca_index]
  if (!length(ipca_T_hits)) {
    stop(sprintf(
      "deflate_ipca: IPCA index for T_ref=%d not in ipca_table (range %d..%d)",
      T_ref, min(ipca_table$yyyymm), max(ipca_table$yyyymm)
    ), call. = FALSE)
  }
  ipca_T <- ipca_T_hits[1L]

  denom_yyyymm <- if (kind == "habitual") {
    as.integer(obs_yyyymm)
  } else {
    obs_int <- as.integer(obs_yyyymm)
    y <- obs_int %/% 100L
    m <- obs_int %% 100L
    data.table::fifelse(m == 1L,
                        (y - 1L) * 100L + 12L,
                        obs_int - 1L)
  }

  data.table::setkey(ipca_table, yyyymm)
  ipca_obs <- ipca_table[.(denom_yyyymm), ipca_index, on = "yyyymm"]

  list(
    deflator = ipca_T / ipca_obs,
    T_ref    = T_ref
  )
}

#' Convenience wrapper: deflate a single nominal column of a data.table in
#' place, attaching `_real` and the chosen kind tag.
#'
#' @param d a data.table with `obs_col` (default ref_month_yyyymm) and
#'   `nominal_col`.
#' @param ipca_table see deflate_ipca.
#' @param nominal_col character — the nominal column to deflate.
#' @param real_col character — output column name (default <nominal>_real).
#' @param kind see deflate_ipca.
#' @param obs_col character — observation month column (default ref_month_yyyymm).
#' @return d (modified by reference). Also attaches attribute "T_ref_<kind>"
#'   to d for downstream auditing.
deflate_ipca_inplace <- function(d, ipca_table, nominal_col, real_col = NULL,
                                 kind = c("habitual", "efetivo"),
                                 obs_col = "ref_month_yyyymm") {
  kind <- match.arg(kind)
  if (is.null(real_col)) real_col <- paste0(nominal_col, "_real")
  if (!nominal_col %in% names(d))
    stop("deflate_ipca_inplace: column '", nominal_col, "' not in d",
         call. = FALSE)
  res <- deflate_ipca(d[[obs_col]], ipca_table, kind = kind)
  d[, (real_col) := get(nominal_col) * res$deflator]
  attr(d, paste0("T_ref_", kind)) <- res$T_ref
  invisible(d)
}
