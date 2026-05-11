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
#' write `prepared_microdata.fst`. Materialises the .fst cache for external
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
    # Phase 2-2: 4 household income variants + alias hhinc_pc
    "hhinc_pc",
    "hhinc_total_efe", "hhinc_pc_efe",
    "hhinc_total_hab", "hhinc_pc_hab",
    # Per-capita components (rendaTrab_ef_pc added Phase 2-2 for the
    # efetiva decomposition)
    "rendaTrab_ha_pc", "rendaTrab_ef_pc",
    "rendaPrevid_pc", "rendaBPC_pc",
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
# Deflation step (Phase 2: IPCA-based, replaces the IBGE per-UF deflator)
# ------------------------------------------------------------------------------

#' Coerce nominal income variables to numeric and apply IPCA-based deflation.
#'
#' Replaces the previous (CO1/CO2/CO3 + inpc_factor) logic with a single
#' national IPCA index. Two flavours of deflator are applied:
#'
#'   - "habitual" (numerator IPCA[T_ref], denominator IPCA[m_obs]) for
#'     habitual labor income (VD4019) and the household composites whose
#'     dominant component is habitual labor (VD5007/VD5008).
#'   - "efetivo"  (denominator IPCA[m_obs - 1]) for effective labor income
#'     (VD4020) and the household composites with effective labor
#'     (VD5001/VD5002), plus all 7 non-labor sources (V500*A2 — IBGE
#'     classifies non-labor income as effective recall).
#'
#' T_ref is the most recent month present in `d$ref_month_yyyymm` and is
#' the same scalar for every row in a given tar_make() run. Stored as
#' attributes "T_ref_habitual" and "T_ref_efetivo" on the returned data.table
#' so downstream code (microdata_log writer) can record it.
#'
#' @param d data.table after pnadc_apply_periods (has ref_month_yyyymm).
#' @param ipca_table data.table from `ipca_index_table` target.
#' @return d, modified by reference, with:
#'   - hhinc_pc_nominal, hhinc_pc (RDPC habitual = VD5008 deflated)
#'   - vd4019_num, vd4020_num, v500*a2_num (numeric, NA -> 0 for full module)
#'   - inpc_factor (scalar, kept as a marker for downstream consumers; equal
#'     to deflator_habitual at T_ref)
deflate_incomes <- function(d, ipca_table) {
  if (!is.data.frame(d))
    stop("deflate_incomes: d must be a data.table", call. = FALSE)
  if (!data.table::is.data.table(d)) data.table::setDT(d)
  d[, UF := as.numeric(UF)]

  # Backwards compatibility: callers that pre-date the simplified-module
  # detector won't have the flag. Treat absence as "full module".
  if (!"income_module_complete" %in% names(d)) {
    d[, income_module_complete := TRUE]
  }

  # Compute the two deflator vectors once, reuse for every nominal column.
  res_hab <- deflate_ipca(d$ref_month_yyyymm, ipca_table, kind = "habitual")
  res_efe <- deflate_ipca(d$ref_month_yyyymm, ipca_table, kind = "efetivo")
  d[, ipca_deflator_hab := res_hab$deflator]
  d[, ipca_deflator_efe := res_efe$deflator]
  data.table::setattr(d, "T_ref_habitual", res_hab$T_ref)
  data.table::setattr(d, "T_ref_efetivo",  res_efe$T_ref)

  message("IPCA deflation: T_ref habitual=", res_hab$T_ref,
          ", efetivo=", res_efe$T_ref)

  # Years with the full income module: keep legacy behaviour (NA -> 0).
  # Years flagged as simplified (e.g. PNADC anual 2025 visita 1) lack
  # VD500* entirely; propagate NA so all real columns are NA,
  # weighted_gini / fgt_all return NA, and the downstream filter
  # excludes the year.
  hh_nominal <- list(
    hhinc_total_efe_nominal = list(src = "vd5001", kind = "efetivo"),
    hhinc_pc_efe_nominal    = list(src = "vd5002", kind = "efetivo"),
    hhinc_total_hab_nominal = list(src = "vd5007", kind = "habitual"),
    hhinc_pc_hab_nominal    = list(src = "vd5008", kind = "habitual")
  )
  for (out in names(hh_nominal)) {
    src <- hh_nominal[[out]]$src
    if (src %in% names(d)) {
      d[income_module_complete == TRUE,
        (out) := data.table::fifelse(is.na(get(src)), 0, as.numeric(get(src)))]
      d[income_module_complete == FALSE, (out) := NA_real_]
    } else {
      d[, (out) := NA_real_]
    }
  }

  # 4 deflated household income variants. Phase 2 deflation: VD5001/VD5002
  # use the efetivo deflator (their work component is effective labor),
  # VD5007/VD5008 use the habitual deflator.
  d[, hhinc_total_efe := hhinc_total_efe_nominal * ipca_deflator_efe]
  d[, hhinc_pc_efe    := hhinc_pc_efe_nominal    * ipca_deflator_efe]
  d[, hhinc_total_hab := hhinc_total_hab_nominal * ipca_deflator_hab]
  d[, hhinc_pc_hab    := hhinc_pc_hab_nominal    * ipca_deflator_hab]

  # Backwards-compatible aliases used by existing builders/tests:
  #   hhinc_pc_nominal points to the VD5008 (habitual per capita) nominal
  #   hhinc_pc        points to the VD5008-deflated value
  d[, hhinc_pc_nominal := hhinc_pc_hab_nominal]
  d[, hhinc_pc         := hhinc_pc_hab]

  d[, vd4019_num := data.table::fifelse(is.na(vd4019), 0, as.numeric(vd4019))]
  d[, vd4020_num := data.table::fifelse(is.na(vd4020), 0, as.numeric(vd4020))]
  for (v in c("v5001a2", "v5002a2", "v5003a2", "v5004a2",
              "v5005a2", "v5006a2", "v5007a2", "v5008a2")) {
    if (v %in% names(d)) {
      nv <- paste0(v, "_num")
      d[, (nv) := data.table::fifelse(is.na(get(v)), 0, as.numeric(get(v)))]
    }
  }
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

  # Phase 2 deflation:
  #   - habitual labor income (VD4019) → habitual deflator
  #   - effective labor income (VD4020) → efetivo deflator (denominator
  #     IPCA[m_obs - 1])
  #   - 7 non-labor sources (V500*A2) → efetivo deflator (IBGE classifies
  #     non-labor income as effective recall — recipient reports the
  #     amount received in the previous month)
  d[, renda_trab_ha := vd4019_num * ipca_deflator_hab]
  d[, renda_trab_ef := vd4020_num * ipca_deflator_efe]

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
      d[, (out) := get(src) * ipca_deflator_efe]
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
# Layer 3 — quarterly individual labor income aggregates
# ------------------------------------------------------------------------------
#
# Produces a long-format data.table mirroring the schema of inequality_data
# (ref_month_yyyymm, measure, value, n_obs, breakdown_type, breakdown_value,
#  period, value_x13, value_stl), but with `measure` taking 8 codes built
# from {mean, median} × {indiv_hab_princ, indiv_efe_princ, indiv_hab_todos,
# indiv_efe_todos}. Series are computed per (month × breakdown × var) over
# people with positive deflated labor income from the mensalized quarterly
# stack (renda_*_*_pc are computed in recode_quarterly when deflator_dt and
# inpc_factor are supplied).
build_quarterly_income_outputs <- function(quarterly_recoded,
                                           dest_dir,
                                           measures_inequality_path,
                                           utils_deseasonalize_path = NULL) {
  # measures_inequality.R provides weighted_gini, palma_ratio,
  # percentile_ratio, top_share, bottom_share, weighted_quantile,
  # income_shares, lorenz_points.
  source(measures_inequality_path)

  d <- quarterly_recoded

  # Sanity: required income columns
  inc_cols <- c("renda_hab_princ", "renda_efe_princ",
                "renda_hab_todos", "renda_efe_todos")
  missing_cols <- setdiff(inc_cols, names(d))
  if (length(missing_cols) > 0L) {
    stop("build_quarterly_income_outputs: missing income columns ",
         paste(missing_cols, collapse = ", "),
         ". Did recode_quarterly receive ipca_table and labels_path?",
         call. = FALSE)
  }

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

  # Phase 2-6: full set of measures (matches build_inequality_outputs).
  # Sample restricted to people with positive labor income for THIS
  # specific income_var via the pre-filter `d_iv` below — Gini/Palma
  # of "individual labor income" naturally exclude non-workers.
  measures_fn <- function(x, w) {
    keep <- !is.na(x) & !is.na(w) & w > 0
    x_k <- x[keep]; w_k <- w[keep]
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
      mean           = if (sum(w, na.rm = TRUE) > 0)
        sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) else NA_real_,
      min            = if (length(x_k) > 0) min(x_k)    else NA_real_,
      p10            = weighted_quantile(x, w, probs = 0.1),
      p25            = weighted_quantile(x, w, probs = 0.25),
      median         = weighted_quantile(x, w, probs = 0.5),
      p75            = weighted_quantile(x, w, probs = 0.75),
      p90            = weighted_quantile(x, w, probs = 0.9),
      max            = if (length(x_k) > 0) max(x_k)    else NA_real_
    )
  }

  shares_specs <- breakdown_specs[
    vapply(breakdown_specs, function(s) s$type != "uf", logical(1L))]
  lorenz_specs <- list(
    list(type = "overall", col = NULL),
    list(type = "race",    col = "raca"),
    list(type = "region",  col = "regiao")
  )

  ineq_parts   <- list()
  shares_parts <- list()
  lorenz_parts <- list()
  for (vname in inc_cols) {
    var_short <- sub("^renda_", "", vname)        # e.g. "hab_princ"
    income_var_code <- paste0("indiv_", var_short)
    # Restrict the sample to people with positive labor income for
    # THIS income_var. Other indiv vars filter independently in their
    # own iteration.
    d_iv <- d[!is.na(get(vname)) & get(vname) > 0]
    if (!nrow(d_iv)) next

    one_ineq <- compute_breakdowns(d_iv, breakdown_specs, measures_fn,
                                   income_col = vname)
    if (nrow(one_ineq)) {
      one_ineq[, income_var := income_var_code]
      ineq_parts[[length(ineq_parts) + 1L]] <- one_ineq
    }

    one_shares <- compute_shares(d_iv, shares_specs, income_col = vname)
    if (nrow(one_shares)) {
      one_shares[, income_var := income_var_code]
      shares_parts[[length(shares_parts) + 1L]] <- one_shares
    }

    one_lorenz <- compute_lorenz(d_iv, lorenz_specs, income_col = vname)
    if (nrow(one_lorenz)) {
      one_lorenz[, income_var := income_var_code]
      lorenz_parts[[length(lorenz_parts) + 1L]] <- one_lorenz
    }
  }

  q_inc <- data.table::rbindlist(ineq_parts, use.names = TRUE, fill = TRUE)
  q_inc[, period := as.Date(sprintf("%d-%02d-15",
                                    ref_month_yyyymm %/% 100,
                                    ref_month_yyyymm %% 100))]

  q_shares <- data.table::rbindlist(shares_parts,
                                    use.names = TRUE, fill = TRUE)
  q_shares[, period := as.Date(sprintf("%d-%02d-15",
                                       ref_month_yyyymm %/% 100,
                                       ref_month_yyyymm %% 100))]

  q_lorenz <- data.table::rbindlist(lorenz_parts,
                                    use.names = TRUE, fill = TRUE)

  # Append X-13 / STL deseasonalized columns to the inequality table.
  # Per (income_var × measure × breakdown_type × breakdown_value) group.
  # Shares and lorenz are not deseasonalized (matches build_inequality_
  # outputs behaviour).
  if (!is.null(utils_deseasonalize_path)) {
    deseasonalize_long_table(
      data = q_inc,
      time_col = "ref_month_yyyymm",
      group_cols = c("income_var", "measure",
                     "breakdown_type", "breakdown_value"),
      value_cols = "value",
      methods = c("x13", "stl"),
      utils_deseasonalize_path = utils_deseasonalize_path
    )
  }

  ineq_path   <- file.path(dest_dir, "quarterly_income_data.rds")
  saveRDS_atomic(q_inc, ineq_path)

  shares_path <- file.path(dest_dir, "quarterly_income_shares_data.rds")
  saveRDS_atomic(q_shares, shares_path)

  lorenz_path <- file.path(dest_dir, "quarterly_lorenz_data.rds")
  saveRDS_atomic(q_lorenz, lorenz_path)

  paths <- c(ineq_path, shares_path, lorenz_path)
  names(paths) <- c("quarterly_income_data",
                    "quarterly_income_shares_data",
                    "quarterly_lorenz_data")
  attr(paths, "latest_ref_month") <-
    as.character(max(q_inc$ref_month_yyyymm, na.rm = TRUE))
  attr(paths, "n_rows") <- c(
    quarterly_income_data        = nrow(q_inc),
    quarterly_income_shares_data = nrow(q_shares),
    quarterly_lorenz_data        = nrow(q_lorenz)
  )
  paths
}

# ------------------------------------------------------------------------------
# Layer 3 — inequality outputs (4 .rds)
# ------------------------------------------------------------------------------

build_inequality_outputs <- function(prepared_microdata_path,
                                     dest_dir,
                                     measures_inequality_path,
                                     utils_deseasonalize_path = NULL) {
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

  # Phase 2-3+2-4: rename mean_income/median_income → mean/median (uniform
  # key names across the annual and quarterly income assets). Phase 2-4
  # also adds 6 percentile measures: min (p≈0), p10, p25, p75, p90, max
  # (p≈1). All use the same weighted_quantile helper so n_obs filters
  # remain meaningful.
  measures_fn <- function(x, w) {
    keep <- !is.na(x) & !is.na(w) & w > 0
    x_k <- x[keep]; w_k <- w[keep]
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
      mean           = if (sum(w, na.rm = TRUE) > 0)
        sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) else NA_real_,
      min            = if (length(x_k) > 0) min(x_k)    else NA_real_,
      p10            = weighted_quantile(x, w, probs = 0.1),
      p25            = weighted_quantile(x, w, probs = 0.25),
      median         = weighted_quantile(x, w, probs = 0.5),
      p75            = weighted_quantile(x, w, probs = 0.75),
      p90            = weighted_quantile(x, w, probs = 0.9),
      max            = if (length(x_k) > 0) max(x_k)    else NA_real_
    )
  }

  # PR6: vectorized via data.table by-group sweeps. The 4 sub-functions
  # (compute_breakdowns, compute_shares, compute_lorenz, compute_gini_decomp)
  # below replace ~6,500 nested-loop iterations with ~1 grouped pass each.

  # Phase 2-2: 4 household income variants (column "income_var" tags
  # which variable produced each row). Same set of measures applied to
  # each, then rbinded into a single long-format table.
  hh_income_vars <- list(
    list(code = "hh_total_efe", col = "hhinc_total_efe"),
    list(code = "hh_pc_efe",    col = "hhinc_pc_efe"),
    list(code = "hh_total_hab", col = "hhinc_total_hab"),
    list(code = "hh_pc_hab",    col = "hhinc_pc_hab")
  )

  ineq_parts <- lapply(hh_income_vars, function(iv) {
    one <- compute_breakdowns(d, breakdown_specs, measures_fn,
                              income_col = iv$col)
    if (!nrow(one)) return(NULL)
    one[, income_var := iv$code]
    one
  })
  ineq <- data.table::rbindlist(ineq_parts, use.names = TRUE, fill = TRUE)
  ineq[, period := as.Date(sprintf("%d-%02d-15",
                                   ref_month_yyyymm %/% 100,
                                   ref_month_yyyymm %% 100))]
  # Append X-13 / STL deseasonalized columns: value_x13, value_stl. Per
  # (income_var, measure, breakdown_type, breakdown_value) group; groups
  # too short for X-11 / STL fall back to the original values inside the
  # helpers.
  deseasonalize_long_table(
    data = ineq,
    time_col = "ref_month_yyyymm",
    group_cols = c("income_var", "measure",
                   "breakdown_type", "breakdown_value"),
    value_cols = "value",
    methods = c("x13", "stl"),
    utils_deseasonalize_path = utils_deseasonalize_path
  )
  inequality_path <- file.path(dest_dir, "inequality_data.rds")
  saveRDS_atomic(ineq, inequality_path)

  # B. Income shares (skip uf — too many). Phase 2-2: 4 hh income variants.
  shares_specs <- breakdown_specs[vapply(breakdown_specs, function(s) s$type != "uf", logical(1L))]
  shares_parts <- lapply(hh_income_vars, function(iv) {
    one <- compute_shares(d, shares_specs, income_col = iv$col)
    if (!nrow(one)) return(NULL)
    one[, income_var := iv$code]
    one
  })
  shares <- data.table::rbindlist(shares_parts, use.names = TRUE, fill = TRUE)
  shares[, period := as.Date(sprintf("%d-%02d-15",
                                     ref_month_yyyymm %/% 100,
                                     ref_month_yyyymm %% 100))]
  shares_path <- file.path(dest_dir, "income_shares_data.rds")
  saveRDS_atomic(shares, shares_path)

  # C. Lorenz (overall + race + region) × 4 hh income variants
  lorenz_specs <- list(
    list(type = "overall", col = NULL),
    list(type = "race",    col = "raca"),
    list(type = "region",  col = "regiao")
  )
  lorenz_parts <- lapply(hh_income_vars, function(iv) {
    one <- compute_lorenz(d, lorenz_specs, income_col = iv$col)
    if (!nrow(one)) return(NULL)
    one[, income_var := iv$code]
    one
  })
  lorenz <- data.table::rbindlist(lorenz_parts, use.names = TRUE, fill = TRUE)
  lorenz_path <- file.path(dest_dir, "lorenz_data.rds")
  saveRDS_atomic(lorenz, lorenz_path)

  # D. Gini decomposition. Phase 2-2: 2 underlying decompositions —
  # "habitual" (renda do trabalho habitual + 7 não-trabalho efetivas) and
  # "efetiva" (trabalho efetivo + 7 não-trabalho efetivas). The dashboard
  # surfaces these as the decomposition for the 4 hh income variants
  # (per-capita and total share the same decomposition since gini
  # decomposition is scale-invariant).
  decomp_specs <- list(
    habitual = list(
      total_var   = "hhinc_pc_hab",
      income_vars = c("rendaTrab_ha_pc",
                      "rendaPrevid_pc", "rendaBPC_pc", "rendaBolsaFam_pc",
                      "rendaOutProgs_pc", "rendaSegDesemp_pc",
                      "rendaAlugueis_pc", "rendaOutros_pc"),
      labels      = data.table::data.table(
        var = c("rendaTrab_ha_pc",
                "rendaPrevid_pc", "rendaBPC_pc", "rendaBolsaFam_pc",
                "rendaOutProgs_pc", "rendaSegDesemp_pc",
                "rendaAlugueis_pc", "rendaOutros_pc"),
        source_id = c("labor", "pension", "bpc", "bolsa_familia",
                      "other_programs", "unemployment_insurance",
                      "rental", "other")
      )
    ),
    efetiva = list(
      total_var   = "hhinc_pc_efe",
      income_vars = c("rendaTrab_ef_pc",
                      "rendaPrevid_pc", "rendaBPC_pc", "rendaBolsaFam_pc",
                      "rendaOutProgs_pc", "rendaSegDesemp_pc",
                      "rendaAlugueis_pc", "rendaOutros_pc"),
      labels      = data.table::data.table(
        var = c("rendaTrab_ef_pc",
                "rendaPrevid_pc", "rendaBPC_pc", "rendaBolsaFam_pc",
                "rendaOutProgs_pc", "rendaSegDesemp_pc",
                "rendaAlugueis_pc", "rendaOutros_pc"),
        source_id = c("labor", "pension", "bpc", "bolsa_familia",
                      "other_programs", "unemployment_insurance",
                      "rental", "other")
      )
    )
  )
  decomp_parts <- lapply(names(decomp_specs), function(kind) {
    spec <- decomp_specs[[kind]]
    one <- compute_gini_decomp(d, spec$income_vars, spec$labels,
                               total_var = spec$total_var)
    if (!nrow(one)) return(NULL)
    one[, decomp_kind := kind]
    one
  })
  decomp <- data.table::rbindlist(decomp_parts, use.names = TRUE, fill = TRUE)
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

compute_breakdowns <- function(d, specs, measure_fn,
                               income_col = "hhinc_pc") {
  parts <- lapply(specs, function(sp) {
    if (sp$type == "overall") {
      res <- d[, {
        if (.N < 30L) NULL else {
          meas <- measure_fn(get(income_col), weight_monthly)
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
          meas <- measure_fn(get(income_col), weight_monthly)
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

compute_shares <- function(d, specs, income_col = "hhinc_pc") {
  parts <- list()
  for (n_groups in c(5L, 10L)) {
    type_label <- if (n_groups == 5L) "quintile" else "decile"
    for (sp in specs) {
      if (sp$type == "overall") {
        res <- d[, {
          if (.N < 50L) NULL else {
            sh <- income_shares(get(income_col), weight_monthly,
                                groups = n_groups)
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
            sh <- income_shares(get(income_col), weight_monthly,
                                groups = n_groups)
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

compute_lorenz <- function(d, specs, income_col = "hhinc_pc") {
  parts <- lapply(specs, function(sp) {
    if (sp$type == "overall") {
      res <- d[, {
        if (.N < 50L) NULL else lorenz_points(get(income_col), weight_monthly,
                                              n = 100L)
      }, by = .(ref_month_yyyymm)]
      if (!nrow(res)) return(NULL)
      res[, `:=`(breakdown_type = "overall", breakdown_value = "Nacional")]
      res
    } else {
      bcol <- sp$col
      res <- d[!is.na(get(bcol)), {
        if (.N < 50L) NULL else lorenz_points(get(income_col), weight_monthly,
                                              n = 100L)
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

compute_gini_decomp <- function(d, income_vars, labels_dt,
                                total_var = "hhinc_pc") {
  res <- d[, {
    if (.N < 100L) NULL else {
      decomp <- gini_decomposition(
        dt = .SD,
        income_vars = income_vars,
        total_var = total_var,
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

#' Phase 2-5: replaced the IBGE-XLS-based deflation (CO1/CO2/CO3 + INPC
#' factor at 2021-07-01) with the new IPCA-based formula. World Bank
#' lines are deflated as:
#'
#'    Linha_R$[T] = (USD/day × 365.25/12 × 2.45_PPP) ×
#'                  (IPCA[T] / mean(IPCA[2021-01..2021-12]))
#'
#' where T = max(ref_month_yyyymm) of the income_var being measured (in
#' practice the annual stack T_ref). 2021 is the WB private-consumption
#' PPP reference year — not a moving target. Minimum-wage lines
#' (br_quarter_mw, br_half_mw) are dropped — the dashboard no longer
#' shows them. FGT is computed twice per row, once over hhinc_pc_hab
#' (RDPC habitual) and once over hhinc_pc_efe (RDPC efetiva), tagged
#' with `income_var ∈ {hh_pc_hab, hh_pc_efe}`.
build_poverty_outputs <- function(prepared_microdata_path,
                                  ipca_table,
                                  dest_dir,
                                  measures_poverty_path,
                                  utils_deseasonalize_path = NULL) {
  source(measures_poverty_path)
  d <- fst::read_fst(prepared_microdata_path, as.data.table = TRUE)
  d <- d[income_module_complete == TRUE]

  # ---- Phase 2-5: WB poverty lines, IPCA-deflated ----
  if (!data.table::is.data.table(ipca_table))
    ipca_table <- data.table::as.data.table(ipca_table)
  ipca_2021 <- ipca_table[yyyymm >= 202101 & yyyymm <= 202112, ipca_index]
  if (length(ipca_2021) != 12L) {
    stop(sprintf(
      "build_poverty_outputs: expected 12 IPCA values for 2021, got %d",
      length(ipca_2021)), call. = FALSE)
  }
  ipca_2021_avg <- mean(ipca_2021)
  T_ref <- as.integer(max(d$ref_month_yyyymm, na.rm = TRUE))
  ipca_T_hits <- ipca_table[yyyymm == T_ref, ipca_index]
  if (!length(ipca_T_hits)) {
    stop(sprintf("build_poverty_outputs: IPCA[T_ref=%d] not in table",
                 T_ref), call. = FALSE)
  }
  line_deflator <- ipca_T_hits[1L] / ipca_2021_avg

  ppp_factor <- 2.45
  days_to_month <- 365.25 / 12
  wb_lines <- data.table::data.table(
    line_id     = c("wb_300", "wb_420", "wb_830"),
    usd_per_day = c(3.00, 4.20, 8.30)
  )
  wb_lines[, brl_monthly_2021 := usd_per_day * ppp_factor * days_to_month]
  wb_lines[, brl_monthly_ref  := brl_monthly_2021 * line_deflator]

  poverty_lines <- list()
  for (i in seq_len(nrow(wb_lines))) {
    poverty_lines[[wb_lines$line_id[i]]] <- list(
      line_id = wb_lines$line_id[i],
      type    = "fixed",
      value   = wb_lines$brl_monthly_ref[i]
    )
  }

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

  # ---- Phase 2-5: 2 income_var (hh_pc_hab, hh_pc_efe) × 3 lines × 8 breakdowns
  income_specs <- list(
    list(code = "hh_pc_hab", col = "hhinc_pc_hab"),
    list(code = "hh_pc_efe", col = "hhinc_pc_efe")
  )

  out <- list()
  k <- 0L
  if (!nrow(d)) {
    poverty_lines <- list()  # short-circuit; downstream still rbinds empty
  }
  for (iv in income_specs) {
    income_col <- iv$col
    for (line_name in names(poverty_lines)) {
      z_val <- poverty_lines[[line_name]]$value
      # fgt_all() accepts a scalar z and broadcasts to length(x), so the
      # original `d_z <- copy(d); d_z[, z := z_val]` is unnecessary —
      # 48 copies of d (~280 MB each) avoided per build_poverty_outputs run.
      for (sp in breakdown_specs) {
        if (sp$type == "overall") {
          res <- d[, {
            fgt <- fgt_all(get(income_col), z_val, weight_monthly)
            list(fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2,
                 n_poor = fgt$n_poor, total_pop = fgt$total_pop,
                 mean_income_poor = fgt$mean_income_poor,
                 poverty_line_value = z_val, n_obs = .N)
          }, by = .(ref_month_yyyymm)]
          res[, breakdown_type := "overall"]
          res[, breakdown_value := "Nacional"]
        } else {
          bcol <- sp$col
          res <- d[!is.na(get(bcol)), {
            fgt <- fgt_all(get(income_col), z_val, weight_monthly)
            list(fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2,
                 n_poor = fgt$n_poor, total_pop = fgt$total_pop,
                 mean_income_poor = fgt$mean_income_poor,
                 poverty_line_value = z_val, n_obs = .N)
          }, by = c("ref_month_yyyymm", bcol)]
          data.table::setnames(res, bcol, "breakdown_value")
          res[, breakdown_type := sp$type]
          res[, breakdown_value := as.character(breakdown_value)]
        }
        res[, poverty_line_id := line_name]
        res[, income_var      := iv$code]
        res <- res[n_obs >= 10L]
        k <- k + 1L
        out[[k]] <- res
      }
    }
  }
  poverty_data <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(poverty_data, c(
    "income_var", "ref_month_yyyymm", "poverty_line_id",
    "poverty_line_value", "breakdown_type", "breakdown_value",
    "fgt0", "fgt1", "fgt2",
    "n_poor", "total_pop", "mean_income_poor", "n_obs"
  ))
  poverty_data[, period := as.Date(sprintf("%d-%02d-15",
                                            ref_month_yyyymm %/% 100,
                                            ref_month_yyyymm %% 100))]
  data.table::setorder(poverty_data,
                       income_var, poverty_line_id,
                       breakdown_type, breakdown_value,
                       ref_month_yyyymm)
  poverty_data[, `:=`(
    fgt0_smooth = data.table::frollmean(fgt0, 3, align = "center"),
    fgt1_smooth = data.table::frollmean(fgt1, 3, align = "center"),
    fgt2_smooth = data.table::frollmean(fgt2, 3, align = "center")
  ), by = .(income_var, poverty_line_id, breakdown_type, breakdown_value)]

  # X-13 / STL deseasonalized columns for each FGT measure. Group by
  # (income_var, poverty_line_id, breakdown_type, breakdown_value).
  deseasonalize_long_table(
    data = poverty_data,
    time_col = "ref_month_yyyymm",
    group_cols = c("income_var", "poverty_line_id",
                   "breakdown_type", "breakdown_value"),
    value_cols = c("fgt0", "fgt1", "fgt2"),
    methods = c("x13", "stl"),
    utils_deseasonalize_path = utils_deseasonalize_path
  )

  data.table::setattr(poverty_data, "T_ref_lines", T_ref)
  data.table::setattr(poverty_data, "ipca_2021_avg", ipca_2021_avg)
  data.table::setattr(poverty_data, "line_deflator", line_deflator)

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
