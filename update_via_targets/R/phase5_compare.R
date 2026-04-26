# ==============================================================================
# Phase 5 — Equivalence test engine
#
# Compares LEGACY (data/) vs NEW (data/_new/) dashboard assets and emits a
# PASS/WARN/FAIL verdict per asset and overall, using the 5-axis decision
# table from the plan at .claude/plans/2026-04-26_phase5-equivalence-test.md.
#
# Public functions:
#   - run_phase5_equivalence(...)    top-level dispatcher
#   - compare_rds(...)               per-asset .rds comparator
#   - compare_sf(...)                structural sf comparator
#   - compare_fst_aggregates(...)    aggregate-only comparator (no row key)
#   - format_report_md/csv/json(...) report writers
#   - aggregate_overall_status(...)  worst-of aggregator
#
# Configured by scripts/phase5_specs.yaml.
# ==============================================================================

# -----------------------------------------------------------------------------
# Status aggregation (worst-of, ignoring SKIP)
# -----------------------------------------------------------------------------

# Status order: PASS < WARN < FAIL. SKIP is not part of the order.
.status_rank <- c(PASS = 0L, WARN = 1L, FAIL = 2L)

aggregate_overall_status <- function(statuses) {
  s <- statuses[statuses %in% names(.status_rank)]
  if (!length(s)) return("PASS")
  names(.status_rank)[max(.status_rank[s]) + 1L]
}

# -----------------------------------------------------------------------------
# Tolerance helpers
# -----------------------------------------------------------------------------

# Resolve a tolerance class ("rate", "count", ...) into the numeric (abs, rel)
# pair from spec$defaults$tolerances. Missing rel returns NA (only abs check).
.resolve_tol <- function(class_name, defaults) {
  tol <- defaults$tolerances[[class_name]]
  list(abs = tol$abs %||% NA_real_, rel = tol$rel %||% NA_real_)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Returns logical vector: TRUE where row exceeds the column tolerance.
.exceeds_tol <- function(abs_diff, rel_diff, tol) {
  ok_abs <- if (!is.na(tol$abs)) abs_diff <= tol$abs else rep(TRUE, length(abs_diff))
  ok_rel <- if (!is.na(tol$rel)) rel_diff <= tol$rel else rep(TRUE, length(rel_diff))
  # PASS if EITHER bound holds (abs OR rel). Exceeds if BOTH fail.
  !(ok_abs | ok_rel)
}

# -----------------------------------------------------------------------------
# Allowlist application
# -----------------------------------------------------------------------------

# Returns logical vector of length nrow(matched_dt) — TRUE where row is
# covered by an allowlist entry that loosens its column to the entry's tol.
# Side-effects: appends entry to env$hits when matched (for report).
.apply_allowlist_to_column <- function(matched_dt, asset_id, col_name,
                                       abs_diff, rel_diff, allowlist, hits_env) {
  if (!length(allowlist)) {
    return(list(exceeds = NULL, hits_added = 0L))
  }

  effective_tol <- list(abs = NA_real_, rel = NA_real_)
  matched <- rep(FALSE, nrow(matched_dt))

  for (entry in allowlist) {
    if (!identical(entry$asset, asset_id)) next

    # Scope: column / columns_match / where (any combination).
    # NOTE: use [[ ]] not $ to avoid R's partial-match gotcha where
    # `entry$scope$column` would silently match `columns_match` by prefix.
    scope <- entry$scope %||% list()
    col_match <- TRUE
    if (!is.null(scope[["column"]])) {
      col_match <- col_name %in% scope[["column"]]
    } else if (!is.null(scope[["columns_match"]])) {
      col_match <- grepl(scope[["columns_match"]], col_name, perl = TRUE)
    }
    if (!col_match) next

    # Row mask from where-clause (evaluated against matched_dt)
    row_mask <- if (!is.null(scope[["where"]])) {
      expr <- parse(text = scope[["where"]])
      tryCatch(eval(expr[[1L]], envir = matched_dt),
               error = function(e) rep(FALSE, nrow(matched_dt)))
    } else {
      rep(TRUE, nrow(matched_dt))
    }
    if (!any(row_mask)) next

    # Apply rule: loosen tolerance
    rule <- entry$rule
    if (identical(rule$type, "abs_tol")) {
      effective_tol$abs <- max(effective_tol$abs, rule$threshold, na.rm = TRUE)
    } else if (identical(rule$type, "rel_tol")) {
      effective_tol$rel <- max(effective_tol$rel, rule$threshold, na.rm = TRUE)
    }
    matched <- matched | row_mask

    # Record hit
    hits_env$hits[[length(hits_env$hits) + 1L]] <- list(
      asset = asset_id, column = col_name,
      reason = entry$reason %||% "",
      approved_by = entry$approved_by %||% "",
      approved_on = entry$approved_on %||% "",
      n_rows = sum(row_mask)
    )
  }

  if (!any(matched)) return(list(exceeds = NULL, hits_added = 0L))

  exceeds <- .exceeds_tol(abs_diff[matched], rel_diff[matched], effective_tol)
  list(matched_idx = which(matched), exceeds_among_matched = exceeds,
       effective_tol = effective_tol)
}

# -----------------------------------------------------------------------------
# Schema / levels axes
# -----------------------------------------------------------------------------

.compare_schema <- function(legacy_dt, new_dt) {
  l_cols <- names(legacy_dt)
  n_cols <- names(new_dt)
  missing_in_new <- setdiff(l_cols, n_cols)
  added_in_new <- setdiff(n_cols, l_cols)

  if (length(missing_in_new) > 0L) {
    return(list(status = "FAIL",
                missing_in_new = missing_in_new,
                added_in_new = added_in_new))
  }
  if (length(added_in_new) > 0L) {
    return(list(status = "WARN",
                missing_in_new = character(0),
                added_in_new = added_in_new))
  }
  list(status = "PASS",
       missing_in_new = character(0), added_in_new = character(0))
}

.compare_levels <- function(legacy_dt, new_dt, key_cols) {
  # For each key/character column, compare unique-value sets.
  # Levels missing from new are FAIL; new-only levels are WARN.
  worst <- "PASS"
  details <- list()
  for (col in intersect(key_cols, names(legacy_dt))) {
    if (!is.character(legacy_dt[[col]]) && !is.factor(legacy_dt[[col]])) next
    if (is.null(new_dt[[col]])) next
    l_lvl <- unique(as.character(legacy_dt[[col]]))
    n_lvl <- unique(as.character(new_dt[[col]]))
    missing_in_new <- setdiff(l_lvl, n_lvl)
    added_in_new <- setdiff(n_lvl, l_lvl)
    if (length(missing_in_new) > 0L) worst <- "FAIL"
    else if (length(added_in_new) > 0L && worst != "FAIL") worst <- "WARN"
    details[[col]] <- list(missing_in_new = missing_in_new,
                           added_in_new = added_in_new)
  }
  list(status = worst, details = details)
}

# -----------------------------------------------------------------------------
# compare_rds — main per-asset .rds comparator
# -----------------------------------------------------------------------------

compare_rds <- function(legacy_path, new_path, spec) {
  asset_id <- spec$asset %||% spec$file %||% "unknown"

  # Axis A: existence
  if (!file.exists(legacy_path)) {
    return(.skeleton_result(asset_id, "FAIL", existence = "FAIL",
                            note = "legacy file missing"))
  }
  if (!file.exists(new_path)) {
    return(.skeleton_result(asset_id, "FAIL", existence = "FAIL",
                            note = "new file missing"))
  }

  legacy <- as.data.table(readRDS(legacy_path))
  new <- as.data.table(readRDS(new_path))

  # Axis B: schema
  sch <- .compare_schema(legacy, new)

  # Axis C: levels (on key cols only, where they're categorical)
  lvl <- .compare_levels(legacy, new, spec$key_cols)

  # Overlap restriction
  overlap_max <- NA_integer_
  if (!is.null(spec$overlap_col) && spec$overlap_col %in% names(legacy) &&
      spec$overlap_col %in% names(new)) {
    o <- spec$overlap_col
    overlap_max <- as.integer(min(max(legacy[[o]], na.rm = TRUE),
                                  max(new[[o]], na.rm = TRUE)))
    legacy_overlap <- legacy[get(o) <= overlap_max]
    new_overlap <- new[get(o) <= overlap_max]
  } else {
    legacy_overlap <- legacy
    new_overlap <- new
  }
  new_only_post_overlap <- nrow(new) - nrow(new_overlap)

  # Join on key
  key <- intersect(spec$key_cols, intersect(names(legacy_overlap),
                                            names(new_overlap)))
  if (!length(key)) {
    return(.skeleton_result(asset_id, "FAIL", existence = "PASS",
                            schema = sch$status,
                            note = "no shared key columns"))
  }
  setkeyv(legacy_overlap, key); setkeyv(new_overlap, key)
  joined <- merge(legacy_overlap, new_overlap, by = key, all = TRUE,
                  suffixes = c(".old", ".new"))
  matched_mask <- !apply(joined[, paste0(setdiff(names(legacy_overlap), key)[1L],
                                         ".old"), with = FALSE], 1L,
                         function(x) all(is.na(x))) &
                  !apply(joined[, paste0(setdiff(names(new_overlap), key)[1L],
                                         ".new"), with = FALSE], 1L,
                         function(x) all(is.na(x)))
  matched <- joined[matched_mask]
  legacy_only <- nrow(legacy_overlap) - nrow(matched)
  new_only <- nrow(new_overlap) - nrow(matched) + new_only_post_overlap

  # Axis D: row-count
  rc_status <- .compare_rowcount(nrow(legacy_overlap), nrow(matched),
                                 spec$defaults$rowcount)

  # Axis E: numeric
  hits_env <- new.env(parent = emptyenv()); hits_env$hits <- list()
  col_summary <- list()
  excess_per_col <- numeric(0)

  for (col_name in names(spec$value_cols)) {
    class_name <- spec$value_cols[[col_name]]
    tol <- .resolve_tol(class_name, spec$defaults)

    old_vec <- matched[[paste0(col_name, ".old")]]
    new_vec <- matched[[paste0(col_name, ".new")]]
    if (is.null(old_vec) || is.null(new_vec)) next
    abs_diff <- abs(new_vec - old_vec)
    rel_diff <- abs_diff / pmax(abs(old_vec), abs(new_vec), 1e-9)

    # Default exceeds
    exceeds <- .exceeds_tol(abs_diff, rel_diff, tol)

    # Allowlist override
    al <- .apply_allowlist_to_column(matched, asset_id, col_name,
                                     abs_diff, rel_diff,
                                     spec$allowlist, hits_env)
    if (!is.null(al$matched_idx)) {
      # Replace exceeds[matched_idx] with the allowlist-loose-tol result
      exceeds[al$matched_idx] <- al$exceeds_among_matched
    }

    n_diff <- sum(abs_diff > 1e-12, na.rm = TRUE)
    n_excess <- sum(exceeds, na.rm = TRUE)
    share_excess <- if (length(exceeds)) n_excess / length(exceeds) else 0
    excess_per_col <- c(excess_per_col, share_excess)

    col_summary[[col_name]] <- data.table(
      column = col_name,
      tol_class = class_name,
      max_abs = if (length(abs_diff)) max(abs_diff, na.rm = TRUE) else NA_real_,
      max_rel = if (length(rel_diff)) max(rel_diff, na.rm = TRUE) else NA_real_,
      p50_abs = stats::quantile(abs_diff, 0.50, na.rm = TRUE, names = FALSE),
      p95_abs = stats::quantile(abs_diff, 0.95, na.rm = TRUE, names = FALSE),
      p99_abs = stats::quantile(abs_diff, 0.99, na.rm = TRUE, names = FALSE),
      tol_abs = tol$abs, tol_rel = tol$rel,
      n_diff = as.integer(n_diff),
      share_excess = share_excess,
      status = if (share_excess <= spec$defaults$numeric$pass_share_excess) "PASS"
               else if (share_excess <= spec$defaults$numeric$warn_share_excess) "WARN"
               else "FAIL"
    )
  }

  num_status <- if (!length(col_summary)) "PASS"
                else aggregate_overall_status(
                  vapply(col_summary, function(x) x$status, character(1L)))

  axes <- list(
    existence = "PASS",
    schema = sch$status,
    levels = lvl$status,
    rowcount = rc_status,
    numeric = num_status
  )
  status <- aggregate_overall_status(unlist(axes))

  list(
    asset = asset_id,
    status = status,
    axes = axes,
    matched_rows = nrow(matched),
    legacy_only = as.integer(legacy_only),
    new_only = as.integer(new_only),
    overlap_max = overlap_max,
    allowlist_hits = hits_env$hits,
    column_summary = if (length(col_summary)) rbindlist(col_summary) else data.table(),
    schema_details = sch,
    levels_details = lvl
  )
}

# -----------------------------------------------------------------------------
# Row-count axis
# -----------------------------------------------------------------------------

.compare_rowcount <- function(legacy_n, matched_n, rc_cfg) {
  if (legacy_n == 0L) return("PASS")
  delta <- abs(legacy_n - matched_n) / legacy_n
  if (delta <= rc_cfg$pass_threshold) "PASS"
  else if (delta <= rc_cfg$warn_threshold) "WARN"
  else "FAIL"
}

# -----------------------------------------------------------------------------
# compare_sf — structural-only sf comparator
# -----------------------------------------------------------------------------

compare_sf <- function(legacy_path, new_path, spec) {
  asset_id <- spec$asset %||% "brazil_states_sf"

  if (!file.exists(legacy_path) || !file.exists(new_path)) {
    return(.skeleton_result(asset_id, "FAIL", existence = "FAIL",
                            note = "missing file"))
  }

  legacy <- readRDS(legacy_path)
  new <- readRDS(new_path)

  if (!inherits(legacy, "sf") || !inherits(new, "sf")) {
    return(.skeleton_result(asset_id, "FAIL", existence = "PASS",
                            schema = "FAIL",
                            note = "not sf objects"))
  }

  # Same uf_code set
  k <- spec$key_cols[1L]
  legacy_keys <- sort(as.character(legacy[[k]]))
  new_keys <- sort(as.character(new[[k]]))
  keys_match <- identical(legacy_keys, new_keys)

  # Attribute exact match per uf_code
  attr_match <- TRUE
  for (col in spec$attribute_cols) {
    if (!col %in% names(legacy) || !col %in% names(new)) {
      attr_match <- FALSE; break
    }
    legacy_dt <- data.table(k = as.character(legacy[[k]]),
                            v = as.character(legacy[[col]]))
    new_dt <- data.table(k = as.character(new[[k]]),
                         v = as.character(new[[col]]))
    setkey(legacy_dt, k); setkey(new_dt, k)
    j <- merge(legacy_dt, new_dt, by = "k", suffixes = c(".old", ".new"))
    if (!identical(j$v.old, j$v.new)) { attr_match <- FALSE; break }
  }

  status <- if (keys_match && attr_match) "PASS" else "FAIL"

  list(
    asset = asset_id,
    status = status,
    axes = list(existence = "PASS",
                schema = if (attr_match) "PASS" else "FAIL",
                levels = if (keys_match) "PASS" else "FAIL",
                rowcount = "PASS",
                numeric = "PASS"),
    matched_rows = length(legacy_keys),
    legacy_only = 0L, new_only = 0L,
    overlap_max = NA_integer_,
    allowlist_hits = list(),
    column_summary = data.table(),
    structural = list(keys_match = keys_match, attr_match = attr_match)
  )
}

# -----------------------------------------------------------------------------
# compare_fst_aggregates — aggregate-only comparator (no row key)
# -----------------------------------------------------------------------------

compare_fst_aggregates <- function(legacy_path, new_path, spec) {
  asset_id <- spec$asset %||% "prepared_microdata"

  if (!file.exists(legacy_path) || !file.exists(new_path)) {
    return(.skeleton_result(asset_id, "FAIL", existence = "FAIL",
                            note = "missing file"))
  }

  legacy <- as.data.table(fst::read_fst(legacy_path))
  new <- as.data.table(fst::read_fst(new_path))

  # Schema
  sch <- .compare_schema(legacy, new)

  # Aggregates configurable
  aggs <- spec$aggregates %||% c("n_rows", "weighted_pop_total_by_quarter",
                                 "weighted_income_total_by_quarter")

  hits_env <- new.env(parent = emptyenv()); hits_env$hits <- list()
  rows_summary <- list()

  if ("n_rows" %in% aggs) {
    abs_diff <- abs(nrow(new) - nrow(legacy))
    rel_diff <- abs_diff / max(nrow(legacy), 1)
    tol <- .resolve_tol("count", spec$defaults)
    exceeds <- .exceeds_tol(abs_diff, rel_diff, tol)
    mock_dt <- data.table(dummy = 1L)
    al <- .apply_allowlist_to_column(mock_dt, asset_id, "n_rows",
                                     abs_diff, rel_diff,
                                     spec$allowlist, hits_env)
    if (!is.null(al$matched_idx)) exceeds <- al$exceeds_among_matched
    se <- if (length(exceeds)) as.numeric(any(exceeds)) else 0
    rows_summary[[length(rows_summary) + 1L]] <- data.table(
      column = "n_rows", tol_class = "count",
      max_abs = abs_diff, max_rel = rel_diff,
      p50_abs = NA_real_, p95_abs = NA_real_, p99_abs = NA_real_,
      tol_abs = tol$abs, tol_rel = tol$rel,
      n_diff = as.integer(nrow(new) != nrow(legacy)),
      share_excess = se,
      status = if (se <= spec$defaults$numeric$pass_share_excess) "PASS"
               else if (se <= spec$defaults$numeric$warn_share_excess) "WARN"
               else "FAIL"
    )
  }

  # Weighted pop totals per quarter (Ano, Trimestre)
  if ("weighted_pop_total_by_quarter" %in% aggs &&
      all(c("Ano", "Trimestre", "weight_monthly") %in% names(legacy))) {
    leg_q <- legacy[, .(pop = sum(weight_monthly, na.rm = TRUE)),
                    by = .(Ano, Trimestre)]
    new_q <- new[, .(pop = sum(weight_monthly, na.rm = TRUE)),
                 by = .(Ano, Trimestre)]
    setkey(leg_q, Ano, Trimestre); setkey(new_q, Ano, Trimestre)
    j <- merge(leg_q, new_q, by = c("Ano", "Trimestre"),
               suffixes = c(".old", ".new"))
    abs_diff <- abs(j$pop.new - j$pop.old)
    rel_diff <- abs_diff / pmax(abs(j$pop.old), abs(j$pop.new), 1e-9)
    tol <- .resolve_tol("population", spec$defaults)
    exceeds <- .exceeds_tol(abs_diff, rel_diff, tol)
    mock_dt <- data.table(dummy = seq_along(rel_diff))
    al <- .apply_allowlist_to_column(mock_dt, asset_id,
                                     "weighted_pop_total_by_quarter",
                                     abs_diff, rel_diff,
                                     spec$allowlist, hits_env)
    if (!is.null(al$matched_idx)) {
      exceeds[al$matched_idx] <- al$exceeds_among_matched
    }
    se <- if (length(exceeds)) sum(exceeds) / length(exceeds) else 0
    rows_summary[[length(rows_summary) + 1L]] <- data.table(
      column = "weighted_pop_total_by_quarter", tol_class = "population",
      max_abs = if (length(abs_diff)) max(abs_diff) else NA_real_,
      max_rel = if (length(rel_diff)) max(rel_diff) else NA_real_,
      p50_abs = stats::quantile(abs_diff, 0.50, names = FALSE),
      p95_abs = stats::quantile(abs_diff, 0.95, names = FALSE),
      p99_abs = stats::quantile(abs_diff, 0.99, names = FALSE),
      tol_abs = tol$abs, tol_rel = tol$rel,
      n_diff = as.integer(sum(abs_diff > 0)),
      share_excess = se,
      status = if (se <= spec$defaults$numeric$pass_share_excess) "PASS"
               else if (se <= spec$defaults$numeric$warn_share_excess) "WARN"
               else "FAIL"
    )
  }

  # Weighted income aggregates per quarter (per income column matching ^renda or hhinc)
  if ("weighted_income_total_by_quarter" %in% aggs) {
    income_cols <- intersect(grep("^(renda|hhinc)", names(legacy),
                                  value = TRUE),
                             names(new))
    for (col in income_cols) {
      leg_i <- legacy[, .(tot = sum(get(col) * weight_monthly, na.rm = TRUE)),
                      by = .(Ano, Trimestre)]
      new_i <- new[, .(tot = sum(get(col) * weight_monthly, na.rm = TRUE)),
                   by = .(Ano, Trimestre)]
      j <- merge(leg_i, new_i, by = c("Ano", "Trimestre"),
                 suffixes = c(".old", ".new"))
      abs_diff <- abs(j$tot.new - j$tot.old)
      rel_diff <- abs_diff / pmax(abs(j$tot.old), abs(j$tot.new), 1e-9)
      # Per-column allowlist
      mock_dt <- data.table(dummy = seq_along(rel_diff))
      al <- .apply_allowlist_to_column(mock_dt, asset_id, col,
                                       abs_diff, rel_diff,
                                       spec$allowlist, hits_env)
      tol <- .resolve_tol("currency", spec$defaults)
      exceeds <- .exceeds_tol(abs_diff, rel_diff, tol)
      if (!is.null(al$matched_idx)) {
        exceeds[al$matched_idx] <- al$exceeds_among_matched
      }
      se <- if (length(exceeds)) sum(exceeds) / length(exceeds) else 0
      rows_summary[[length(rows_summary) + 1L]] <- data.table(
        column = paste0("sum(", col, "*w) per quarter"),
        tol_class = "currency",
        max_abs = if (length(abs_diff)) max(abs_diff) else NA_real_,
        max_rel = if (length(rel_diff)) max(rel_diff) else NA_real_,
        p50_abs = stats::quantile(abs_diff, 0.50, names = FALSE),
        p95_abs = stats::quantile(abs_diff, 0.95, names = FALSE),
        p99_abs = stats::quantile(abs_diff, 0.99, names = FALSE),
        tol_abs = tol$abs, tol_rel = tol$rel,
        n_diff = as.integer(sum(abs_diff > 0)),
        share_excess = se,
        status = if (se <= spec$defaults$numeric$pass_share_excess) "PASS"
                 else if (se <= spec$defaults$numeric$warn_share_excess) "WARN"
                 else "FAIL"
      )
    }
  }

  num_status <- if (!length(rows_summary)) "PASS"
                else aggregate_overall_status(
                  vapply(rows_summary, function(x) x$status, character(1L)))

  axes <- list(existence = "PASS", schema = sch$status, levels = "PASS",
               rowcount = "PASS", numeric = num_status)
  status <- aggregate_overall_status(unlist(axes))

  list(
    asset = asset_id, status = status, axes = axes,
    matched_rows = nrow(legacy), legacy_only = 0L,
    new_only = max(0L, nrow(new) - nrow(legacy)),
    overlap_max = NA_integer_,
    allowlist_hits = hits_env$hits,
    column_summary = if (length(rows_summary)) rbindlist(rows_summary)
                     else data.table()
  )
}

# -----------------------------------------------------------------------------
# Skeleton result for short-circuit cases
# -----------------------------------------------------------------------------

.skeleton_result <- function(asset_id, status, existence = "PASS",
                             schema = "PASS", levels = "PASS",
                             rowcount = "PASS", numeric = "PASS",
                             note = "") {
  list(
    asset = asset_id, status = status,
    axes = list(existence = existence, schema = schema, levels = levels,
                rowcount = rowcount, numeric = numeric),
    matched_rows = 0L, legacy_only = 0L, new_only = 0L,
    overlap_max = NA_integer_,
    allowlist_hits = list(),
    column_summary = data.table(),
    note = note
  )
}

# -----------------------------------------------------------------------------
# Top-level dispatcher
# -----------------------------------------------------------------------------

run_phase5_equivalence <- function(legacy_dir, new_dir,
                                   legacy_proc = NULL, new_proc = NULL,
                                   config_path,
                                   report_dir = dirname(config_path),
                                   strict = FALSE) {
  cfg <- yaml::read_yaml(config_path)
  defaults <- cfg$defaults
  allowlist <- cfg$allowlist %||% list()
  results <- list()

  for (asset_id in names(cfg$assets)) {
    spec <- cfg$assets[[asset_id]]
    spec$asset <- asset_id
    spec$defaults <- defaults
    spec$allowlist <- allowlist
    mode <- spec$mode %||% "rds"

    res <- if (identical(mode, "skip")) {
      list(asset = asset_id, status = "SKIP",
           axes = list(existence = "SKIP", schema = "SKIP", levels = "SKIP",
                       rowcount = "SKIP", numeric = "SKIP"),
           matched_rows = 0L, legacy_only = 0L, new_only = 0L,
           overlap_max = NA_integer_, allowlist_hits = list(),
           column_summary = data.table(), note = spec$reason %||% "skip")
    } else if (identical(mode, "rds")) {
      compare_rds(file.path(legacy_dir, spec$file),
                  file.path(new_dir, spec$file), spec)
    } else if (identical(mode, "sf")) {
      compare_sf(file.path(legacy_dir, spec$file),
                 file.path(new_dir, spec$file), spec)
    } else if (identical(mode, "aggregate_only")) {
      compare_fst_aggregates(file.path(legacy_proc, spec$file_proc),
                             file.path(new_proc, spec$file_proc), spec)
    } else {
      stop("unknown mode: ", mode)
    }
    results[[asset_id]] <- res
  }

  overall <- aggregate_overall_status(
    vapply(results, function(x) x$status, character(1L)))

  run_ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(format_report_md(results, run_ts, overall),
             file.path(report_dir, "phase5_report.md"))
  data.table::fwrite(format_report_csv(results),
                     file.path(report_dir, "phase5_report.csv"))
  jsonlite::write_json(format_report_json(results, run_ts, overall),
                       file.path(report_dir, "phase5_report.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  list(overall = overall, results = results, run_ts = run_ts,
       report_dir = report_dir,
       exit_code = if (strict && overall != "PASS") 1L else 0L)
}

# -----------------------------------------------------------------------------
# Report writers
# -----------------------------------------------------------------------------

format_report_md <- function(results, run_ts, overall = NULL) {
  if (is.null(overall)) {
    overall <- aggregate_overall_status(
      vapply(results, function(x) x$status, character(1L)))
  }

  lines <- c(
    "# Phase 5 Equivalence Report",
    "",
    sprintf("**Run:** %s | **Verdict:** **%s**", run_ts, overall),
    "",
    "## Summary",
    "",
    "| asset | status | matched | legacy_only | new_only | allowlist_hits |",
    "|-|-|-|-|-|-|"
  )
  for (r in results) {
    lines <- c(lines, sprintf("| %s | **%s** | %d | %d | %d | %d |",
                              r$asset, r$status,
                              r$matched_rows %||% 0L,
                              r$legacy_only %||% 0L,
                              r$new_only %||% 0L,
                              length(r$allowlist_hits %||% list())))
  }
  lines <- c(lines, "")

  for (r in results) {
    lines <- c(lines, "",
               sprintf("## %s — %s", r$asset, r$status),
               "")
    if (!is.null(r$note) && nchar(r$note)) {
      lines <- c(lines, sprintf("*Note:* %s", r$note), "")
    }
    if (!is.null(r$overlap_max) && !is.na(r$overlap_max)) {
      lines <- c(lines, sprintf("Overlap: ..%s | matched: %s | legacy_only: %s | new_only: %s",
                                r$overlap_max, r$matched_rows,
                                r$legacy_only, r$new_only), "")
    }
    if (!is.null(r$axes)) {
      lines <- c(lines,
                 sprintf("Axes: existence=%s, schema=%s, levels=%s, rowcount=%s, numeric=%s",
                         r$axes$existence, r$axes$schema, r$axes$levels,
                         r$axes$rowcount, r$axes$numeric),
                 "")
    }
    if (!is.null(r$column_summary) && nrow(r$column_summary)) {
      cs <- r$column_summary
      lines <- c(lines,
                 "| column | tol_class | max_abs | max_rel | P50 | P95 | P99 | n_diff | %excess | status |",
                 "|-|-|-|-|-|-|-|-|-|-|")
      for (i in seq_len(nrow(cs))) {
        lines <- c(lines, sprintf("| %s | %s | %.3g | %.3g | %.3g | %.3g | %.3g | %d | %.1f%% | **%s** |",
                                   cs$column[i], cs$tol_class[i],
                                   cs$max_abs[i], cs$max_rel[i],
                                   cs$p50_abs[i], cs$p95_abs[i], cs$p99_abs[i],
                                   cs$n_diff[i], 100 * cs$share_excess[i],
                                   cs$status[i]))
      }
      lines <- c(lines, "")
    }
    if (length(r$allowlist_hits %||% list()) > 0L) {
      lines <- c(lines, "Allowlist hits:")
      for (h in r$allowlist_hits) {
        lines <- c(lines, sprintf("- %s.%s — %d rows — %s (approved by %s on %s)",
                                  h$asset %||% "", h$column %||% "",
                                  h$n_rows %||% 0L, h$reason %||% "",
                                  h$approved_by %||% "", h$approved_on %||% ""))
      }
      lines <- c(lines, "")
    }
  }
  paste(lines, collapse = "\n")
}

format_report_csv <- function(results) {
  parts <- list()
  for (r in results) {
    if (is.null(r$column_summary) || !nrow(r$column_summary)) {
      parts[[length(parts) + 1L]] <- data.table(
        asset = r$asset, status = r$status, column = NA_character_,
        tol_class = NA_character_, max_abs = NA_real_, max_rel = NA_real_,
        p50_abs = NA_real_, p95_abs = NA_real_, p99_abs = NA_real_,
        tol_abs = NA_real_, tol_rel = NA_real_, n_diff = NA_integer_,
        share_excess = NA_real_, col_status = NA_character_)
      next
    }
    cs <- copy(r$column_summary)
    cs[, asset := r$asset]
    cs[, asset_status := r$status]
    setnames(cs, "status", "col_status")
    setcolorder(cs, c("asset", "asset_status", "column", "tol_class",
                      "max_abs", "max_rel", "p50_abs", "p95_abs", "p99_abs",
                      "tol_abs", "tol_rel", "n_diff", "share_excess",
                      "col_status"))
    parts[[length(parts) + 1L]] <- cs
  }
  rbindlist(parts, fill = TRUE)
}

format_report_json <- function(results, run_ts, overall) {
  list(
    run_ts = run_ts,
    overall = overall,
    assets = lapply(results, function(r) {
      list(
        asset = r$asset, status = r$status, axes = r$axes,
        matched_rows = r$matched_rows %||% 0L,
        legacy_only = r$legacy_only %||% 0L,
        new_only = r$new_only %||% 0L,
        overlap_max = r$overlap_max,
        allowlist_hits = r$allowlist_hits,
        column_summary = if (!is.null(r$column_summary) &&
                              nrow(r$column_summary)) {
          as.list(r$column_summary)
        } else list()
      )
    })
  )
}
