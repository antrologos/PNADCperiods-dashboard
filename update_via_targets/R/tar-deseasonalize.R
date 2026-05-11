# ==============================================================================
# Pipeline helper: deseasonalize a long-format data.table by group
# ==============================================================================
# Used by build_inequality_outputs() and build_poverty_outputs() to attach
# X-13 ARIMA and STL seasonally-adjusted columns to inequality_data.rds and
# poverty_data.rds.
#
# Reuses the dashboard's R/utils_deseasonalize.R helpers (deseasonalize_x13,
# deseasonalize_stl) so that the pipeline output and any on-the-fly fallback
# share a single implementation.
# ==============================================================================

# Load the dashboard's deseasonalization wrappers into globalenv. Idempotent.
load_deseasonalize_helpers <- function(utils_path = NULL) {
  if (is.null(utils_path)) {
    utils_path <- file.path(tar_dashboard_root(), "R", "utils_deseasonalize.R")
  }
  source(utils_path)
}

#' Add X-13 and STL deseasonalized columns to a long-format data.table
#'
#' For each name in `value_cols`, two new columns are appended: `<col>_x13`
#' and `<col>_stl`. NAs come back when the group is too short (< 24 obs) — in
#' practice the underlying `deseasonalize_x13` / `deseasonalize_stl` fall back
#' to the original values instead, which keeps the chart readable.
#'
#' @param data data.table; modified in place AND returned (data.table idiom).
#' @param time_col Character name of the integer YYYYMM column.
#' @param group_cols Character vector of columns identifying a single
#'   time series.
#' @param value_cols Character vector of value columns to deseasonalize.
#' @param methods Subset of c("x13", "stl"). Defaults to both.
#' @param utils_deseasonalize_path Optional explicit path to the dashboard's
#'   utils_deseasonalize.R; if NULL, derived via tar_dashboard_root().
#' @return The input data.table (invisibly), with new columns added.
deseasonalize_long_table <- function(data,
                                     time_col,
                                     group_cols,
                                     value_cols,
                                     methods = c("x13", "stl"),
                                     utils_deseasonalize_path = NULL) {
  stopifnot(data.table::is.data.table(data))
  required <- c(time_col, group_cols, value_cols)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    stop("deseasonalize_long_table: missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  load_deseasonalize_helpers(utils_deseasonalize_path)

  for (m in methods) {
    fn <- switch(m,
                 x13 = get("deseasonalize_x13", envir = globalenv()),
                 stl = get("deseasonalize_stl", envir = globalenv()),
                 stop("Unknown method: ", m, call. = FALSE))

    for (vc in value_cols) {
      new_col <- paste0(vc, "_", m)
      data[, (new_col) := {
        values <- get(vc)
        # Groups shorter than 24 obs: deseasonalize_x13/_stl already
        # return `values` unchanged (utils_deseasonalize.R:48 and :167-170).
        # Short-circuit here to skip per-group order/as.Date/fn-dispatch
        # overhead. Byte-identical output.
        if (.N < 24L) values
        else {
          time_v <- get(time_col)
          ord <- order(time_v)
          v_ord <- values[ord]
          t_ord <- time_v[ord]
          # time is integer YYYYMM; build first-of-month dates
          d_ord <- as.Date(sprintf("%d-%02d-01", t_ord %/% 100L, t_ord %% 100L))
          adj_ord <- suppressWarnings(fn(v_ord, d_ord))
          if (length(adj_ord) != length(v_ord)) values
          else {
            out <- rep(NA_real_, length(values))
            out[ord] <- adj_ord
            out
          }
        }
      }, by = group_cols]
    }
  }

  invisible(data)
}
