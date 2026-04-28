# ==== Layer 2 cache + Layer 3 dashboard assets ================================
# Pure transforms over the acervo manifest + on-disk .fst. Migrated from
# scripts/precompute_*.R, factored to be re-runnable and idempotent in targets.
#
# prepared_microdata.fst pipeline (~161 MB):
#   1. Stack quarterly .fst → quarterly_stacked (in tar-stack.R)
#   2. pnadc_identify_periods → crosswalk_target
#   3. Stack + harmonize annual visits → annual_stacked (tar-stack.R)
#   4. recode_annual: pnadc_apply_periods + V2005 filter + deflate +
#      per-capita components + demographic groupings (tar-recode.R)
#   5. build_prepared_microdata: select dashboard cols, write .fst

#' Thin writer: select dashboard-relevant columns from `annual_recoded` and
#' write `prepared_microdata.fst`.
#'
#' PR4: this function used to load the annual stack, apply_periods, V2005
#' filter, deflate, build pc_income components, build demographic groupings.
#' All of that moved to dedicated targets:
#'   stack_annual → annual_stacked  (tar-stack.R)
#'   recode_annual → annual_recoded (tar-recode.R)
#' This is now just a writer that materialises the .fst cache for external
#' consumers (legacy scripts, dashboard offline mode, Phase 5 equivalence).
#'
#' @param annual_recoded data.table from `recode_annual`
#' @param dest_path character path for the .fst
#' @return character dest_path
build_prepared_microdata <- function(annual_recoded, dest_path) {
  set_fst_threads(2L)
  keep_cols <- c(
    "Ano", "Trimestre", "UF", "ref_month_yyyymm", "ref_month_in_quarter",
    "ref_month_in_year", "weight_monthly",
    "hhinc_pc", "rendaTrab_ha_pc", "rendaPrevid_pc", "rendaBPC_pc",
    "rendaBolsaFam_pc", "rendaOutProgs_pc", "rendaSegDesemp_pc",
    "rendaAlugueis_pc", "rendaOutros_pc", "hhinc_pc_components",
    "income_module_complete",
    "sexo", "raca", "faixa_educ", "regiao", "uf_abbrev",
    "urbano", "faixa_idade", "V2009"
  )
  keep_cols <- intersect(keep_cols, names(annual_recoded))
  d_final <- annual_recoded[, ..keep_cols][!is.na(ref_month_yyyymm)]
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest_path, ".tmp")
  fst::write_fst(d_final, tmp)
  atomic_rename(tmp, dest_path)
  dest_path
}

# PR4: load_annual_with_income_harmonization moved to tar-stack.R as the
# helper implementing stack_annual().

# ------------------------------------------------------------------------------
# Deflation step (CO2 + INPC to target date)
# ------------------------------------------------------------------------------

#' @param deflator_dt data.table from `read_deflator_xls` (in `tar-network.R`),
#'   pre-parsed once per tar_make in the `deflator_dt` target. Columns
#'   (Ano, Trimestre, UF, CO2, CO2e, CO3) all numeric.
#' @param inpc_factor numeric scalar — INPC factor for mid-2024 → real_date.
#'   Pre-computed by `compute_inpc_factors` (in `tar-network.R`) to avoid
#'   re-hitting the IPEA API inside this builder.
deflate_incomes <- function(d, deflator_dt, inpc_factor) {
  if (!is.numeric(inpc_factor) || length(inpc_factor) != 1L || !is.finite(inpc_factor))
    stop("inpc_factor must be a single finite numeric (got: ",
         paste(class(inpc_factor), collapse = "/"), ")", call. = FALSE)
  deflator <- data.table::copy(deflator_dt)  # avoid mutating shared target
  d[, UF := as.numeric(UF)]

  # Refuse to silently propagate NA when the deflator XLS doesn't cover the
  # microdata's full (Ano, Trimestre, UF) range. IBGE updates the deflator
  # yearly and the merge keys on (Ano, Trimestre, UF) — partial UF coverage
  # for a quarter would also leak NA into hhinc_pc.
  d_keys <- unique(d[, .(Ano, Trimestre, UF)])
  defl_keys <- unique(deflator[, .(Ano, Trimestre, UF)])
  uncovered <- d_keys[!defl_keys, on = .(Ano, Trimestre, UF)]
  if (nrow(uncovered)) {
    sample <- utils::head(uncovered, 10L)
    stop(sprintf(
      "Deflator does not cover %d microdata key(s) on (Ano, Trimestre, UF). First %d: %s. Update the deflator XLS via tar_invalidate(\"deflator_download\") + tar_make().",
      nrow(uncovered),
      nrow(sample),
      paste(sprintf("%d-Q%d-UF%02d",
                    sample$Ano, sample$Trimestre, sample$UF),
            collapse = ", ")
    ), call. = FALSE)
  }

  data.table::setkeyv(deflator, c("Ano", "Trimestre", "UF"))
  data.table::setkeyv(d, c("Ano", "Trimestre", "UF"))
  d <- deflator[d]

  # Backwards compatibility: callers that pre-date the simplified-module
  # detector (legacy unit tests, ad-hoc scripts) won't have the flag. Treat
  # absence as "full module" so they keep the legacy NA -> 0 behaviour.
  if (!"income_module_complete" %in% names(d)) {
    d[, income_module_complete := TRUE]
  }

  message(sprintf("INPC adjustment factor (mid-2024 -> %s): %.4f",
                  deflation_target_date, inpc_factor))

  # Years with the full income module: keep legacy behaviour (NA -> 0).
  # Years flagged as simplified (e.g. PNADC anual 2025 visita 1) lack
  # VD5008 entirely; propagate NA so hhinc_pc is NA, weighted_gini /
  # fgt_all return NA, and the downstream filter excludes the year.
  d[income_module_complete == TRUE,
    hhinc_pc_nominal := data.table::fifelse(is.na(vd5008), 0, as.numeric(vd5008))]
  d[income_module_complete == FALSE,
    hhinc_pc_nominal := NA_real_]
  d[, hhinc_pc := hhinc_pc_nominal * CO2 * inpc_factor]
  d[, vd4019_num := data.table::fifelse(is.na(vd4019), 0, as.numeric(vd4019))]
  d[, vd4020_num := data.table::fifelse(is.na(vd4020), 0, as.numeric(vd4020))]
  for (v in c("v5001a2", "v5002a2", "v5003a2", "v5004a2",
              "v5005a2", "v5006a2", "v5007a2", "v5008a2")) {
    if (v %in% names(d)) {
      nv <- paste0(v, "_num")
      d[, (nv) := data.table::fifelse(is.na(get(v)), 0, as.numeric(get(v)))]
    }
  }
  d[, inpc_factor := inpc_factor]
  d
}

# ------------------------------------------------------------------------------
# Per-capita income components (8 sources)
# ------------------------------------------------------------------------------

build_pc_income_components <- function(d) {
  d[, id_dom := paste(UPA, V1008, V1014, sep = "_")]
  # n_members is computed inside the household-level aggregation below
  # (`hh[, n_members := .N, ...]`) and broadcast back via merge. No
  # individual-level `n_members` column is needed at this point.

  d[, renda_trab_ha := vd4019_num * CO2 * inpc_factor]
  d[, renda_trab_ef := vd4020_num * CO2 * inpc_factor]

  src_to_col <- list(
    renda_bpc        = "v5001a2_num",
    renda_bf         = "v5002a2_num",
    renda_outprogs   = "v5003a2_num",
    renda_previd     = "v5004a2_num",
    renda_segdesemp  = "v5005a2_num",
    renda_doacoes    = "v5006a2_num",
    renda_alugueis   = "v5007a2_num",
    renda_bolsas     = "v5008a2_num"
  )
  for (out in names(src_to_col)) {
    src <- src_to_col[[out]]
    if (src %in% names(d)) {
      d[, (out) := get(src) * CO2e * inpc_factor]
    } else {
      d[, (out) := 0]
    }
  }

  # Sum each component independently before combining: a row with one of
  # `renda_doacoes` or `renda_bolsas` NA (e.g. deflator merge gap) would
  # otherwise drop its surviving sibling because `NA + value = NA` is then
  # discarded by `na.rm = TRUE`.
  hh <- d[, .(
    hh_trab_ha    = sum(renda_trab_ha, na.rm = TRUE),
    hh_trab_ef    = sum(renda_trab_ef, na.rm = TRUE),
    hh_previd     = sum(renda_previd, na.rm = TRUE),
    hh_bpc        = sum(renda_bpc, na.rm = TRUE),
    hh_bf         = sum(renda_bf, na.rm = TRUE),
    hh_outprogs   = sum(renda_outprogs, na.rm = TRUE),
    hh_segdesemp  = sum(renda_segdesemp, na.rm = TRUE),
    hh_alugueis   = sum(renda_alugueis, na.rm = TRUE),
    hh_outros     = sum(renda_doacoes, na.rm = TRUE) +
                    sum(renda_bolsas,  na.rm = TRUE),
    n_members     = .N
  ), by = .(Ano, Trimestre, id_dom)]

  hh[, `:=`(
    rendaTrab_ha_pc   = hh_trab_ha / n_members,
    rendaTrab_ef_pc   = hh_trab_ef / n_members,
    rendaPrevid_pc    = hh_previd / n_members,
    rendaBPC_pc       = hh_bpc / n_members,
    rendaBolsaFam_pc  = hh_bf / n_members,
    rendaOutProgs_pc  = hh_outprogs / n_members,
    rendaSegDesemp_pc = hh_segdesemp / n_members,
    rendaAlugueis_pc  = hh_alugueis / n_members,
    rendaOutros_pc    = hh_outros / n_members
  )]

  pc_cols <- c("rendaTrab_ha_pc", "rendaTrab_ef_pc", "rendaPrevid_pc",
               "rendaBPC_pc", "rendaBolsaFam_pc", "rendaOutProgs_pc",
               "rendaSegDesemp_pc", "rendaAlugueis_pc", "rendaOutros_pc")
  d <- merge(
    d,
    hh[, c("Ano", "Trimestre", "id_dom", pc_cols), with = FALSE],
    by = c("Ano", "Trimestre", "id_dom"),
    all.x = TRUE
  )
  d[, hhinc_pc_components := rendaTrab_ha_pc + rendaPrevid_pc + rendaBPC_pc +
      rendaBolsaFam_pc + rendaOutProgs_pc + rendaSegDesemp_pc +
      rendaAlugueis_pc + rendaOutros_pc]
  d
}

# ------------------------------------------------------------------------------
# Demographic grouping variables (sourced from utils_inequality.R)
# ------------------------------------------------------------------------------

build_demographic_groupings <- function(d) {
  d[, sexo := sex_label(v2007)]
  d[, raca := race_label(v2010)]
  d[, faixa_educ := if ("vd3004" %in% names(d)) education_group(vd3004) else NA_character_]
  d[, regiao := uf_to_region(UF)]
  d[, uf_abbrev := uf_to_abbrev(UF)]
  d[, urbano := if ("v1022" %in% names(d)) urban_rural_label(v1022) else NA_character_]
  d[, faixa_idade := age_group(V2009)]
  d
}

# ------------------------------------------------------------------------------
# Layer 3 — inequality outputs (4 .rds)
# ------------------------------------------------------------------------------

build_inequality_outputs <- function(prepared_microdata_path,
                                     dest_dir,
                                     measures_inequality_path) {
  # Source into globalenv (default): compute_breakdowns/shares/lorenz/decomp
  # live in globalenv (sourced by _targets.R) and lexically reference
  # weighted_gini/lorenz_points/income_shares/etc. via globalenv. Same
  # rationale as Plan 5 fix for build_prepared_microdata.
  source(measures_inequality_path)
  d <- fst::read_fst(prepared_microdata_path, as.data.table = TRUE)
  # Years flagged as simplified by detect_simplified_annual_year() (e.g.
  # PNADC anual 2025 visita 1 — published without VD5008 + V5*A2) cannot
  # produce a meaningful hhinc_pc. Drop them so inequality_data,
  # income_shares_data, lorenz_data, and income_decomposition_data omit
  # the year entirely (rather than emitting NA / degenerate Lorenz lines).
  d <- d[income_module_complete == TRUE]

  breakdown_specs <- list(
    list(type = "overall",      col = NULL),
    list(type = "sex",          col = "sexo"),
    list(type = "race",         col = "raca"),
    list(type = "education",    col = "faixa_educ"),
    list(type = "region",       col = "regiao"),
    list(type = "uf",           col = "uf_abbrev"),
    list(type = "urban_rural",  col = "urbano"),
    list(type = "age_group",    col = "faixa_idade")
  )
  income_vars <- c("rendaTrab_ha_pc", "rendaPrevid_pc", "rendaBPC_pc",
                   "rendaBolsaFam_pc", "rendaOutProgs_pc", "rendaSegDesemp_pc",
                   "rendaAlugueis_pc", "rendaOutros_pc")
  income_source_labels <- data.table::data.table(
    var = income_vars,
    source_id = c("labor", "pension", "bpc", "bolsa_familia",
                  "other_programs", "unemployment_insurance", "rental", "other")
  )

  measures_fn <- function(x, w) {
    list(
      gini           = weighted_gini(x, w),
      palma          = palma_ratio(x, w),
      p90p10         = percentile_ratio(x, w, 0.9, 0.1),
      p90p50         = percentile_ratio(x, w, 0.9, 0.5),
      p50p10         = percentile_ratio(x, w, 0.5, 0.1),
      top1_share     = top_share(x, w, 1),
      top5_share     = top_share(x, w, 5),
      top10_share    = top_share(x, w, 10),
      bottom50_share = bottom_share(x, w, 50),
      mean_income    = if (sum(w, na.rm = TRUE) > 0)
        sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) else NA_real_,
      median_income  = weighted_quantile(x, w, probs = 0.5)
    )
  }

  # PR6: vectorized via data.table by-group sweeps. The 4 sub-functions
  # (compute_breakdowns, compute_shares, compute_lorenz, compute_gini_decomp)
  # below replace ~6,500 nested-loop iterations with ~1 grouped pass each.

  # A. Time series
  ineq <- compute_breakdowns(d, breakdown_specs, measures_fn)
  ineq[, period := as.Date(sprintf("%d-%02d-15",
                                   ref_month_yyyymm %/% 100,
                                   ref_month_yyyymm %% 100))]
  inequality_path <- file.path(dest_dir, "inequality_data.rds")
  saveRDS_atomic(ineq, inequality_path)

  # B. Income shares (skip uf — too many)
  shares_specs <- breakdown_specs[vapply(breakdown_specs, function(s) s$type != "uf", logical(1L))]
  shares <- compute_shares(d, shares_specs)
  shares[, period := as.Date(sprintf("%d-%02d-15",
                                     ref_month_yyyymm %/% 100,
                                     ref_month_yyyymm %% 100))]
  shares_path <- file.path(dest_dir, "income_shares_data.rds")
  saveRDS_atomic(shares, shares_path)

  # C. Lorenz (overall + race + region)
  lorenz_specs <- list(
    list(type = "overall", col = NULL),
    list(type = "race",    col = "raca"),
    list(type = "region",  col = "regiao")
  )
  lorenz <- compute_lorenz(d, lorenz_specs)
  lorenz_path <- file.path(dest_dir, "lorenz_data.rds")
  saveRDS_atomic(lorenz, lorenz_path)

  # D. Gini decomposition (overall, monthly)
  decomp <- compute_gini_decomp(d, income_vars, income_source_labels)
  decomp[, period := as.Date(sprintf("%d-%02d-15",
                                     ref_month_yyyymm %/% 100,
                                     ref_month_yyyymm %% 100))]
  decomp_path <- file.path(dest_dir, "income_decomposition_data.rds")
  saveRDS_atomic(decomp, decomp_path)

  paths <- c(inequality_path, shares_path, lorenz_path, decomp_path)
  names(paths) <- c("inequality_data", "income_shares_data",
                    "lorenz_data", "income_decomposition_data")
  attr(paths, "latest_ref_month") <- as.character(max(d$ref_month_yyyymm, na.rm = TRUE))
  attr(paths, "n_rows") <- c(
    inequality_data           = nrow(ineq),
    income_shares_data        = nrow(shares),
    lorenz_data               = nrow(lorenz),
    income_decomposition_data = nrow(decomp)
  )
  paths
}

# PR6 vectorized helpers (replaces ~6,500 nested-loop iterations with ~40
# data.table by-group sweeps). Each sub-function: 1 sweep per breakdown_spec,
# rbinded. Same numeric output (modulo floating-point summation order) as the
# legacy nested-loop version.

compute_breakdowns <- function(d, specs, measure_fn) {
  parts <- lapply(specs, function(sp) {
    if (sp$type == "overall") {
      res <- d[, {
        if (.N < 30L) NULL else {
          meas <- measure_fn(hhinc_pc, weight_monthly)
          list(measure = names(meas), value = unlist(meas, use.names = FALSE),
               n_obs = .N)
        }
      }, by = .(ref_month_yyyymm)]
      if (!nrow(res)) return(NULL)
      res[, `:=`(breakdown_type = "overall", breakdown_value = "Nacional")]
      res
    } else {
      bcol <- sp$col
      res <- d[!is.na(get(bcol)), {
        if (.N < 30L) NULL else {
          meas <- measure_fn(hhinc_pc, weight_monthly)
          list(measure = names(meas), value = unlist(meas, use.names = FALSE),
               n_obs = .N)
        }
      }, by = c("ref_month_yyyymm", bcol)]
      if (!nrow(res)) return(NULL)
      data.table::setnames(res, bcol, "breakdown_value")
      res[, breakdown_type := sp$type]
      res[, breakdown_value := as.character(breakdown_value)]
      res
    }
  })
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

compute_shares <- function(d, specs) {
  parts <- list()
  for (n_groups in c(5L, 10L)) {
    type_label <- if (n_groups == 5L) "quintile" else "decile"
    for (sp in specs) {
      if (sp$type == "overall") {
        res <- d[, {
          if (.N < 50L) NULL else {
            sh <- income_shares(hhinc_pc, weight_monthly, groups = n_groups)
            list(group_label = sh$group_label, share = sh$share)
          }
        }, by = .(ref_month_yyyymm)]
        if (nrow(res)) {
          res[, `:=`(breakdown_type = "overall", breakdown_value = "Nacional",
                     group_type = type_label)]
          parts[[length(parts) + 1L]] <- res
        }
      } else {
        bcol <- sp$col
        res <- d[!is.na(get(bcol)), {
          if (.N < 50L) NULL else {
            sh <- income_shares(hhinc_pc, weight_monthly, groups = n_groups)
            list(group_label = sh$group_label, share = sh$share)
          }
        }, by = c("ref_month_yyyymm", bcol)]
        if (nrow(res)) {
          data.table::setnames(res, bcol, "breakdown_value")
          res[, `:=`(breakdown_type = sp$type, group_type = type_label)]
          res[, breakdown_value := as.character(breakdown_value)]
          parts[[length(parts) + 1L]] <- res
        }
      }
    }
  }
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

compute_lorenz <- function(d, specs) {
  parts <- lapply(specs, function(sp) {
    if (sp$type == "overall") {
      res <- d[, {
        if (.N < 50L) NULL else lorenz_points(hhinc_pc, weight_monthly, n = 100L)
      }, by = .(ref_month_yyyymm)]
      if (!nrow(res)) return(NULL)
      res[, `:=`(breakdown_type = "overall", breakdown_value = "Nacional")]
      res
    } else {
      bcol <- sp$col
      res <- d[!is.na(get(bcol)), {
        if (.N < 50L) NULL else lorenz_points(hhinc_pc, weight_monthly, n = 100L)
      }, by = c("ref_month_yyyymm", bcol)]
      if (!nrow(res)) return(NULL)
      data.table::setnames(res, bcol, "breakdown_value")
      res[, breakdown_type := sp$type]
      res[, breakdown_value := as.character(breakdown_value)]
      res
    }
  })
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

compute_gini_decomp <- function(d, income_vars, labels_dt) {
  res <- d[, {
    if (.N < 100L) NULL else {
      decomp <- gini_decomposition(
        dt = .SD,
        income_vars = income_vars,
        total_var = "hhinc_pc",
        weight_var = "weight_monthly"
      )
      list(income_source = decomp$income_source,
           concentration_coeff = decomp$concentration_coeff,
           income_share = decomp$income_share,
           contribution_to_gini = decomp$contribution_to_gini)
    }
  }, by = .(ref_month_yyyymm)]
  if (!nrow(res)) return(res)
  res <- merge(res, labels_dt,
               by.x = "income_source", by.y = "var", all.x = TRUE)
  res[, income_source := source_id]
  res[, source_id := NULL]
  res
}

# ------------------------------------------------------------------------------
# Layer 3 — poverty outputs (1 .rds)
# ------------------------------------------------------------------------------

#' @param inpc_factor_table data.table(nominal_date, factor) — pre-computed
#'   in `compute_inpc_factors` (in `tar-network.R`). Replaces the 3 internal
#'   `deflateBR::inpc` calls (WB lines @ 2021-07, MW quarter/half @ year-mid).
build_poverty_outputs <- function(prepared_microdata_path,
                                  inpc_factor_table,
                                  dest_dir,
                                  measures_poverty_path) {
  # Source into globalenv (default): protects against future refactors that
  # move the j-expression callers into globalenv helpers (same lesson as
  # Plan 5 fix for build_prepared_microdata).
  source(measures_poverty_path)
  d <- fst::read_fst(prepared_microdata_path, as.data.table = TRUE)
  # Same simplified-module filter as build_inequality_outputs: drop years
  # where hhinc_pc is NA by design (e.g. PNADC anual 2025 visita 1)
  # to avoid emitting fgt0 = 1.0 (everyone below line) / NA poverty rates.
  d <- d[income_module_complete == TRUE]

  # WB poverty lines: deflate from 2021-07 PPP-anchored values.
  # Replaces `get_wb_poverty_lines(reference_date = ...)` to avoid a
  # network call inside this builder.
  ppp_factor <- 2.45
  days_to_month <- 365 / 12
  factor_2021 <- inpc_factor_at(inpc_factor_table, as.Date("2021-07-01"))
  wb_lines <- data.table::data.table(
    line_id = c("wb_300", "wb_420", "wb_830"),
    label_en = c("Extreme Poverty ($3.00/day)",
                 "Lower-Middle ($4.20/day)",
                 "Upper-Middle ($8.30/day)"),
    label_pt = c("Pobreza Extrema ($3,00/dia)",
                 "Renda Media-Baixa ($4,20/dia)",
                 "Renda Media-Alta ($8,30/dia)"),
    usd_per_day = c(3.00, 4.20, 8.30)
  )
  wb_lines[, brl_monthly_2021 := usd_per_day * ppp_factor * days_to_month]
  wb_lines[, brl_monthly_ref := brl_monthly_2021 * factor_2021]

  # MW table: deflate per-year MW values via lookup (one merge, no API calls).
  mw_table <- get_historical_minimum_wage()
  mw_table[, mw_quarter := salario_minimo / 4]
  mw_table[, mw_half := salario_minimo / 2]
  mw_table[, nominal_date := as.Date(sprintf("%d-07-01", ano))]
  mw_table <- merge(mw_table, inpc_factor_table,
                    by = "nominal_date", all.x = TRUE)
  if (any(is.na(mw_table$factor))) {
    missing_yrs <- mw_table[is.na(factor), ano]
    stop(sprintf("INPC factor missing for MW years: %s",
                 paste(sort(unique(missing_yrs)), collapse = ", ")),
         call. = FALSE)
  }
  mw_table[, mw_quarter_target := mw_quarter * factor]
  mw_table[, mw_half_target := mw_half * factor]

  poverty_lines <- list()
  for (i in seq_len(nrow(wb_lines))) {
    poverty_lines[[wb_lines$line_id[i]]] <- list(
      line_id = wb_lines$line_id[i],
      type = "fixed",
      value = wb_lines$brl_monthly_ref[i]
    )
  }
  poverty_lines[["br_quarter_mw"]] <- list(
    line_id = "br_quarter_mw", type = "by_year",
    values = mw_table[, .(ano, value = mw_quarter_target)]
  )
  poverty_lines[["br_half_mw"]] <- list(
    line_id = "br_half_mw", type = "by_year",
    values = mw_table[, .(ano, value = mw_half_target)]
  )

  breakdown_specs <- list(
    list(type = "overall",      col = NULL),
    list(type = "sex",          col = "sexo"),
    list(type = "race",         col = "raca"),
    list(type = "education",    col = "faixa_educ"),
    list(type = "region",       col = "regiao"),
    list(type = "uf",           col = "uf_abbrev"),
    list(type = "urban_rural",  col = "urbano"),
    list(type = "age_group",    col = "faixa_idade")
  )

  # PR5 vectorization: 5 lines × 8 breakdowns = 40 grouped sweeps via
  # data.table `by`, instead of ~10,500 nested-loop iterations. Same numeric
  # output (modulo floating-point summation order) as the legacy nested loop.
  out <- vector("list", length(poverty_lines) * length(breakdown_specs))
  k <- 0L
  for (line_name in names(poverty_lines)) {
    pline <- poverty_lines[[line_name]]

    # Materialize per-row z column. For "fixed", z is constant; for "by_year",
    # merge year-keyed value into d (rows for years not in the lookup are
    # dropped — matches the legacy `next` behaviour).
    if (pline$type == "fixed") {
      d_z <- data.table::copy(d)
      d_z[, z := pline$value]
    } else {
      year_z <- pline$values[, .(Ano = as.numeric(ano), z = value)]
      d_z <- merge(d, year_z, by = "Ano", all.x = FALSE)
    }
    if (!nrow(d_z)) next

    for (sp in breakdown_specs) {
      if (sp$type == "overall") {
        res <- d_z[, {
          fgt <- fgt_all(hhinc_pc, z, weight_monthly)
          list(fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2,
               n_poor = fgt$n_poor, total_pop = fgt$total_pop,
               mean_income_poor = fgt$mean_income_poor,
               poverty_line_value = z[1L], n_obs = .N)
        }, by = .(ref_month_yyyymm)]
        res[, breakdown_type := "overall"]
        res[, breakdown_value := "Nacional"]
      } else {
        bcol <- sp$col
        res <- d_z[!is.na(get(bcol)), {
          fgt <- fgt_all(hhinc_pc, z, weight_monthly)
          list(fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2,
               n_poor = fgt$n_poor, total_pop = fgt$total_pop,
               mean_income_poor = fgt$mean_income_poor,
               poverty_line_value = z[1L], n_obs = .N)
        }, by = c("ref_month_yyyymm", bcol)]
        data.table::setnames(res, bcol, "breakdown_value")
        res[, breakdown_type := sp$type]
        res[, breakdown_value := as.character(breakdown_value)]
      }
      res[, poverty_line_id := line_name]
      res <- res[n_obs >= 10L]   # legacy filter
      k <- k + 1L
      out[[k]] <- res
    }
  }
  out <- out[seq_len(k)]
  poverty_data <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  # Match legacy column order so external consumers (Phase 5 equivalence)
  # see a stable schema.
  data.table::setcolorder(poverty_data, c(
    "ref_month_yyyymm", "poverty_line_id", "poverty_line_value",
    "breakdown_type", "breakdown_value",
    "fgt0", "fgt1", "fgt2",
    "n_poor", "total_pop", "mean_income_poor", "n_obs"
  ))
  poverty_data[, period := as.Date(sprintf("%d-%02d-15",
                                            ref_month_yyyymm %/% 100,
                                            ref_month_yyyymm %% 100))]
  data.table::setorder(poverty_data,
                        poverty_line_id, breakdown_type, breakdown_value,
                        ref_month_yyyymm)
  poverty_data[, `:=`(
    fgt0_smooth = data.table::frollmean(fgt0, 3, align = "center"),
    fgt1_smooth = data.table::frollmean(fgt1, 3, align = "center"),
    fgt2_smooth = data.table::frollmean(fgt2, 3, align = "center")
  ), by = .(poverty_line_id, breakdown_type, breakdown_value)]

  dest <- file.path(dest_dir, "poverty_data.rds")
  saveRDS_atomic(poverty_data, dest)
  attr(dest, "latest_ref_month") <- as.character(max(poverty_data$ref_month_yyyymm, na.rm = TRUE))
  attr(dest, "n_rows") <- nrow(poverty_data)
  dest
}

# ------------------------------------------------------------------------------
# Layer 3 — state_monthly_data.rds (geographic from microdata)
# ------------------------------------------------------------------------------

#' Aggregate state-monthly indicators from the recoded quarterly stack.
#'
#' PR3: prelude (read 56 .fst, type coercion, derived flags, apply_periods,
#' V2009 filter) was extracted to `recode_quarterly` (in `tar-recode.R`).
#' This function is now a thin aggregator: takes the recoded data.table,
#' aggregates by (ref_month_yyyymm, UF), computes rates, melts to long form,
#' and writes `state_monthly_data.rds`.
build_state_monthly <- function(quarterly_recoded, dest_path) {
  pnadc <- quarterly_recoded

  count_vars <- c(
    "pop14mais", "pea", "employed", "unemployed", "fora_forca",
    "subocuphoras", "forcapotencial", "desalentado", "contribuinte",
    "empregpriv", "empregpubl", "domestico", "empregador",
    "contapropria", "trabfamauxiliar", "informal",
    "agropecuaria", "industria", "construcao", "comercio", "servicos"
  )
  base <- pnadc[, lapply(.SD, function(x) sum(x * weight_monthly, na.rm = TRUE)),
                by = .(ref_month_yyyymm, UF), .SDcols = count_vars]
  data.table::setnames(base, c("ref_month_yyyymm", "UF"), c("ref_month", "uf_code"))
  base[, uf_code := as.character(uf_code)]
  base[, `:=`(
    taxadesocup = unemployed / pea * 100,
    taxapartic  = pea / pop14mais * 100,
    nivelocup   = employed / pop14mais * 100,
    taxasubocuphoras = subocuphoras / employed * 100,
    taxainformal     = informal / employed * 100,
    taxacontribprev  = contribuinte / employed * 100
  )]

  rate_indicators <- c("taxadesocup", "taxapartic", "nivelocup",
                       "taxasubocuphoras", "taxainformal", "taxacontribprev")
  level_indicators <- c("pop14mais", "pea", "employed", "unemployed", "fora_forca",
                        "empregpriv", "empregpubl", "domestico", "empregador",
                        "contapropria",
                        "agropecuaria", "industria", "construcao", "comercio", "servicos")
  long <- data.table::melt(
    base, id.vars = c("ref_month", "uf_code"),
    measure.vars = c(rate_indicators, level_indicators),
    variable.name = "indicator", value.name = "value"
  )
  long[, indicator := as.character(indicator)]
  long[indicator %in% rate_indicators, value := round(value, 2)]
  long[indicator %in% level_indicators, value := round(value, 0)]
  data.table::setorder(long, ref_month, uf_code, indicator)

  saveRDS_atomic(long, dest_path)
  attr(dest_path, "latest_ref_month") <- as.character(max(long$ref_month, na.rm = TRUE))
  attr(dest_path, "n_rows") <- nrow(long)
  dest_path
}

# ------------------------------------------------------------------------------
# Layer 3 — brazil_states_sf.rds (one-off; cached after first run)
# ------------------------------------------------------------------------------

#' @param brazil_states_sf_raw sf object — raw output from
#'   `fetch_brazil_states_sf` (in `tar-network.R`). Network call hoisted to L1.
build_brazil_states_sf <- function(brazil_states_sf_raw, dest_path) {
  states_simple <- rmapshaper::ms_simplify(brazil_states_sf_raw,
                                           keep = 0.01, keep_shapes = TRUE)
  states_simple <- sf::st_transform(states_simple, 4326)
  states_simple <- states_simple[, c("code_state", "abbrev_state",
                                     "name_state", "geom")]
  names(states_simple) <- c("uf_code", "uf_abbrev", "uf_name", "geometry")
  states_simple$uf_code <- as.character(as.integer(states_simple$uf_code))
  sf::st_geometry(states_simple) <- "geometry"

  saveRDS_atomic(states_simple, dest_path)
  attr(dest_path, "n_rows") <- nrow(states_simple)
  dest_path
}

# `build_geographic_fallback` removed — see NOTE in tar-network.R about why
# the SIDRA-based geographic fallback was deleted entirely.

# ------------------------------------------------------------------------------
# Atomic saveRDS
# ------------------------------------------------------------------------------

saveRDS_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(object, tmp)
  atomic_rename(tmp, path)
  invisible(path)
}
