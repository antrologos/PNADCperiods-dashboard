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

# Read worker count from option (precedence) or env var. Default 1 (serial).
.x13_workers <- function() {
  w <- getOption("pnadc.x13.workers",
                 as.integer(Sys.getenv("PNADC_X13_WORKERS", "1")))
  if (is.null(w) || is.na(w) || w < 1L) 1L else as.integer(w)
}

#' Add X-13 and STL deseasonalized columns to a long-format data.table
#'
#' For each name in `value_cols`, two new columns are appended: `<col>_x13`
#' and `<col>_stl`. NAs come back when the group is too short (< 24 obs) — in
#' practice the underlying `deseasonalize_x13` / `deseasonalize_stl` fall back
#' to the original values instead, which keeps the chart readable.
#'
#' Parallelism is opt-in. Set option `pnadc.x13.workers` or env var
#' `PNADC_X13_WORKERS` to a value > 1 to enable PSOCK parallelism over
#' groups. Each worker is initialised with an isolated `TMPDIR` (so the
#' X-13 Fortran binary's temp files don't collide) and its own sourced
#' copy of `utils_deseasonalize.R`. Default = serial; output is
#' byte-identical to the serial path (X-13/STL are deterministic given
#' the same input).
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
  if (is.null(utils_deseasonalize_path)) {
    utils_deseasonalize_path <- file.path(tar_dashboard_root(), "R",
                                          "utils_deseasonalize.R")
  }
  load_deseasonalize_helpers(utils_deseasonalize_path)

  workers <- .x13_workers()

  if (workers == 1L) {
    # Serial path (B6 short-circuit on .N < 24L).
    for (m in methods) {
      fn <- switch(m,
                   x13 = get("deseasonalize_x13", envir = globalenv()),
                   stl = get("deseasonalize_stl", envir = globalenv()),
                   stop("Unknown method: ", m, call. = FALSE))
      for (vc in value_cols) {
        new_col <- paste0(vc, "_", m)
        data[, (new_col) := {
          values <- get(vc)
          if (.N < 24L) values
          else {
            time_v <- get(time_col)
            ord <- order(time_v)
            v_ord <- values[ord]
            t_ord <- time_v[ord]
            d_ord <- as.Date(sprintf("%d-%02d-01",
                                      t_ord %/% 100L, t_ord %% 100L))
            adj_ord <- suppressWarnings(fn(v_ord, d_ord))
            if (length(adj_ord) != length(values)) values
            else {
              out <- rep(NA_real_, length(values))
              out[ord] <- adj_ord
              out
            }
          }
        }, by = group_cols]
      }
    }
    return(invisible(data))
  }

  # Parallel path: PSOCK over groups. Each group's (method, vc) outputs are
  # computed in a worker; the master writes them back preserving the
  # original row order via a temporary row-index column.
  ROW_IDX <- "..__deseason_row_idx__"
  data[, (ROW_IDX) := seq_len(.N)]
  on.exit({
    if (ROW_IDX %in% names(data)) data[, (ROW_IDX) := NULL]
  }, add = TRUE)

  needed_cols <- unique(c(time_col, group_cols, value_cols, ROW_IDX))
  groups <- split(data[, ..needed_cols], by = group_cols,
                  sorted = FALSE, drop = TRUE)
  n_workers <- min(workers, length(groups))
  message("deseasonalize_long_table: ", length(groups),
          " groups across ", n_workers, " PSOCK workers")

  cl <- parallel::makePSOCKcluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(cl,
    varlist = c("utils_deseasonalize_path", "methods",
                "value_cols", "time_col", "ROW_IDX"),
    envir = environment())
  parallel::clusterEvalQ(cl, {
    # Isolated TMPDIR per worker — X-13 binary writes temp files there.
    worker_tmp <- tempfile("x13_w_")
    dir.create(worker_tmp, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(TMPDIR = worker_tmp)
    suppressPackageStartupMessages(library(data.table))
    source(utils_deseasonalize_path)
    NULL
  })

  results <- parallel::parLapply(cl, groups, function(g) {
    n <- nrow(g)
    out <- data.table::data.table(
      ..__deseason_row_idx__ = g[[ROW_IDX]]
    )
    if (n < 24L) {
      for (m in methods) for (vc in value_cols) {
        out[, (paste0(vc, "_", m)) := g[[vc]]]
      }
      return(out)
    }
    time_v <- g[[time_col]]
    ord <- order(time_v)
    t_ord <- time_v[ord]
    d_ord <- as.Date(sprintf("%d-%02d-01", t_ord %/% 100L, t_ord %% 100L))
    for (m in methods) {
      fn <- switch(m,
                   x13 = deseasonalize_x13,
                   stl = deseasonalize_stl,
                   stop("Unknown method: ", m))
      for (vc in value_cols) {
        new_col <- paste0(vc, "_", m)
        values <- g[[vc]]
        v_ord <- values[ord]
        adj_ord <- suppressWarnings(fn(v_ord, d_ord))
        if (length(adj_ord) != length(values)) {
          out[, (new_col) := values]
        } else {
          new_vals <- rep(NA_real_, length(values))
          new_vals[ord] <- adj_ord
          out[, (new_col) := new_vals]
        }
      }
    }
    out
  })

  combined <- data.table::rbindlist(results, use.names = TRUE)
  data.table::setnames(combined, "..__deseason_row_idx__", ROW_IDX)
  data.table::setorderv(combined, ROW_IDX)

  for (m in methods) for (vc in value_cols) {
    new_col <- paste0(vc, "_", m)
    data[, (new_col) := combined[[new_col]]]
  }
  data[, (ROW_IDX) := NULL]

  invisible(data)
}
