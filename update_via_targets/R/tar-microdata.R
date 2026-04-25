# ==============================================================================
# tar-microdata.R — Layer 2 (cache) and Layer 3 (dashboard assets) builders
#
# These functions are pure transforms over the acervo manifest + on-disk .fst.
# They are migrated from PNADCperiods-dashboard/scripts/precompute_*.R and
# code/precompute_geographic_microdata.R, but factored to be re-runnable and
# idempotent inside a targets DAG.
# ==============================================================================

# ------------------------------------------------------------------------------
# Layer 2 — prepared_microdata.fst (~161 MB)
#
# Pipeline:
#   1. Read all available quarterly .fst (filter to quarterly_required_vars)
#   2. build_crosswalk()
#   3. Read annual visits per get_default_visit() rule, harmonize income vars
#   4. apply_periods_annual()
#   5. Filter household members (V2005 <= 14 | V2005 == 16)
#   6. Deflate incomes (CO2/CO2e + INPC to deflation_target_date)
#   7. Construct 8 per-capita income components
#   8. Create demographic grouping variables
#   9. Write to data/processed/prepared_microdata.fst
# ------------------------------------------------------------------------------

#' Build prepared_microdata.fst from the acervo.
#'
#' @param acervo_manifest data.table from Layer 1 (must include OK / DOWNLOADED_NEW
#'   rows for all expected quarterly + annual files)
#' @param deflator_path character path to the IBGE deflator .xls
#' @param dest_path character path of the output .fst (Layer 2 cache)
#' @param utils_inequality_path path to PNADCperiods-dashboard/R/utils_inequality.R
#'   (sourced for sex_label, race_label, education_group, uf_to_region, ...)
#' @return character dest_path
build_prepared_microdata <- function(acervo_manifest,
                                     deflator_path,
                                     dest_path,
                                     utils_inequality_path) {
  set_fst_threads(2L)
  source(utils_inequality_path, local = TRUE)

  # Quarterly: all rows with file_type == "quarterly" and a usable status
  qf <- acervo_manifest[
    file_type == "quarterly" &
      status %in% c("OK", "DOWNLOADED_NEW", "REDOWNLOADED_REWEIGHT") &
      !is.na(local_path)
  ]
  if (!nrow(qf)) stop("No quarterly files available in manifest.")

  message(sprintf("Loading %d quarterly files for crosswalk...", nrow(qf)))
  quarterly_data <- data.table::rbindlist(
    lapply(qf$local_path, function(p) {
      avail <- intersect(quarterly_required_vars, names(fst::read_fst(p, from = 1L, to = 1L)))
      fst::read_fst(p, columns = avail, as.data.table = TRUE)
    }),
    fill = TRUE
  )
  gc()

  message(sprintf("Quarterly stack: %s rows", format(nrow(quarterly_data), big.mark = ",")))
  crosswalk <- build_crosswalk(quarterly_data)
  rm(quarterly_data); gc()

  det_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]
  message(sprintf("Crosswalk determination rate: %.1f%%", 100 * det_rate))

  # Annual: filter to chosen visit
  af <- acervo_manifest[
    file_type == "annual" &
      status %in% c("OK", "DOWNLOADED_NEW", "REDOWNLOADED_REWEIGHT") &
      !is.na(local_path)
  ]
  af[, default_visit := get_default_visit(year)]
  af <- af[period == default_visit]
  if (!nrow(af)) stop("No annual files available in manifest after visit filter.")

  message(sprintf("Loading %d annual visits...", nrow(af)))
  annual_data <- load_annual_with_income_harmonization(af$local_path)
  gc()

  # Standardize join keys to uppercase (matches pnadc_apply_periods expectations)
  key_mappings <- c(
    "ano" = "Ano", "trimestre" = "Trimestre",
    "upa" = "UPA", "v1008" = "V1008", "v1014" = "V1014",
    "v1032" = "V1032", "uf" = "UF", "v2009" = "V2009"
  )
  for (old in names(key_mappings)) {
    if (old %in% names(annual_data)) {
      data.table::setnames(annual_data, old, key_mappings[[old]])
    }
  }

  d <- apply_periods_annual(annual_data, crosswalk)
  rm(annual_data, crosswalk); gc()

  # Filter household members
  d <- d[v2005 <= 14 | v2005 == 16]

  # Deflate
  d <- deflate_incomes(d, deflator_path)
  d <- build_pc_income_components(d)
  d <- build_demographic_groupings(d)

  # Final select
  keep_cols <- c(
    "Ano", "Trimestre", "UF", "ref_month_yyyymm", "ref_month_in_quarter",
    "ref_month_in_year", "weight_monthly",
    "hhinc_pc", "rendaTrab_ha_pc", "rendaPrevid_pc", "rendaBPC_pc",
    "rendaBolsaFam_pc", "rendaOutProgs_pc", "rendaSegDesemp_pc",
    "rendaAlugueis_pc", "rendaOutros_pc", "hhinc_pc_components",
    "sexo", "raca", "faixa_educ", "regiao", "uf_abbrev",
    "urbano", "faixa_idade", "V2009"
  )
  keep_cols <- intersect(keep_cols, names(d))
  d_final <- d[, ..keep_cols][!is.na(ref_month_yyyymm)]

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest_path, ".tmp")
  fst::write_fst(d_final, tmp)
  atomic_rename(tmp, dest_path)
  dest_path
}

# ------------------------------------------------------------------------------
# Annual loader with income variable harmonization (pre-2015 vs post-2015)
# ------------------------------------------------------------------------------

load_annual_with_income_harmonization <- function(paths) {
  vars_pre <- c("v500111", "v500211", "v500311", "v500411",
                "v500511", "v500611", "v500711", "v500811",
                "v500911", "v501011", "v501111", "v501211", "v501311")
  vars_post <- c("v5001a2", "v5002a2", "v5003a2", "v5005a2",
                 "v5008a2", "v5004a2", "v5006a2", "v5007a2")
  base <- c("ano", "trimestre", "upa", "v1008", "v1014",
            "v2005", "v2007", "v2009", "v2010", "vd3004", "v1022",
            "uf", "estrato",
            "v1032", "posest", "posest_sxi",
            "vd5008", "vd4019", "vd4020")

  Sum <- function(...) {
    X <- cbind(...)
    all_na <- rowSums(!is.na(X)) == 0L
    res <- rowSums(X, na.rm = TRUE)
    res[all_na] <- NA
    res
  }

  data.table::rbindlist(lapply(paths, function(f) {
    dt <- fst::read_fst(f, as.data.table = TRUE)
    data.table::setnames(dt, tolower(names(dt)))
    yr <- if ("ano" %in% names(dt)) as.integer(dt$ano[1L]) else
      as.integer(sub(".*pnadc_(\\d{4}).*", "\\1", basename(f)))

    income_vars <- if (yr < 2015) vars_pre
                   else if (yr == 2015) c(vars_pre, vars_post)
                   else vars_post
    cols <- intersect(c(base, income_vars), names(dt))
    dt <- dt[, ..cols]

    if (yr <= 2014L) {
      dt[, v5001a2 := data.table::fifelse(is.na(v500911), NA_real_, as.numeric(v500911))]
      dt[, v5002a2 := data.table::fifelse(is.na(v501011), NA_real_, as.numeric(v501011))]
      dt[, v5003a2 := data.table::fifelse(is.na(v501111), NA_real_, as.numeric(v501111))]
      dt[, v5005a2 := data.table::fifelse(is.na(v500811), NA_real_, as.numeric(v500811))]
      dt[, v5008a2 := Sum(v500311, v500411, v501211, v501311)]
      dt[, v5004a2 := Sum(v500111, v500211)]
      dt[, v5006a2 := Sum(v500711, v500511)]
      dt[, v5007a2 := data.table::fifelse(is.na(v500611), NA_real_, as.numeric(v500611))]
      old <- intersect(vars_pre, names(dt))
      if (length(old)) dt[, (old) := NULL]
    } else if (yr == 2015L) {
      dt[, v5001a2 := Sum(v5001a2, v500911)]
      dt[, v5002a2 := Sum(v5002a2, v501011)]
      dt[, v5003a2 := Sum(v5003a2, v501111)]
      dt[, v5005a2 := Sum(v5005a2, v500811)]
      dt[, v5008a2 := Sum(v5008a2, v500311, v500411, v501211, v501311)]
      dt[, v5004a2 := Sum(v5004a2, v500111, v500211)]
      dt[, v5006a2 := Sum(v5006a2, v500711, v500511)]
      dt[, v5007a2 := Sum(v5007a2, v500611)]
      old <- intersect(vars_pre, names(dt))
      if (length(old)) dt[, (old) := NULL]
    }
    dt
  }), fill = TRUE)
}

# ------------------------------------------------------------------------------
# Deflation step (CO2 + INPC to target date)
# ------------------------------------------------------------------------------

deflate_incomes <- function(d, deflator_path) {
  if (!file.exists(deflator_path))
    stop("Deflator file not found: ", deflator_path, call. = FALSE)
  deflator <- readxl::read_excel(deflator_path)
  data.table::setDT(deflator)
  deflator <- deflator[, .(Ano = ano, Trimestre = trim, UF = uf, CO2, CO2e, CO3)]
  deflator[, `:=`(Ano = as.numeric(Ano),
                  Trimestre = as.numeric(Trimestre),
                  UF = as.numeric(UF))]
  d[, UF := as.numeric(UF)]
  data.table::setkeyv(deflator, c("Ano", "Trimestre", "UF"))
  data.table::setkeyv(d, c("Ano", "Trimestre", "UF"))
  d <- deflator[d]

  inpc_factor <- deflateBR::inpc(
    1, nominal_dates = as.Date("2024-07-01"),
    real_date = deflation_target_date
  )
  message(sprintf("INPC adjustment factor (mid-2024 -> %s): %.4f",
                  deflation_target_date, inpc_factor))

  d[, hhinc_pc_nominal := data.table::fifelse(is.na(vd5008), 0, as.numeric(vd5008))]
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
  d[, n_members := .N, by = .(Ano, Trimestre, id_dom)]

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

  hh <- d[, .(
    hh_trab_ha    = sum(renda_trab_ha, na.rm = TRUE),
    hh_trab_ef    = sum(renda_trab_ef, na.rm = TRUE),
    hh_previd     = sum(renda_previd, na.rm = TRUE),
    hh_bpc        = sum(renda_bpc, na.rm = TRUE),
    hh_bf         = sum(renda_bf, na.rm = TRUE),
    hh_outprogs   = sum(renda_outprogs, na.rm = TRUE),
    hh_segdesemp  = sum(renda_segdesemp, na.rm = TRUE),
    hh_alugueis   = sum(renda_alugueis, na.rm = TRUE),
    hh_outros     = sum(renda_doacoes + renda_bolsas, na.rm = TRUE),
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
                                     utils_inequality_path) {
  source(utils_inequality_path, local = TRUE)
  d <- fst::read_fst(prepared_microdata_path, as.data.table = TRUE)

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

  months <- sort(unique(d$ref_month_yyyymm))

  # A. Time series
  ineq <- compute_breakdowns(d, months, breakdown_specs, function(sub) {
    if (nrow(sub) < 30L) return(NULL)
    measures_fn(sub$hhinc_pc, sub$weight_monthly)
  })
  ineq[, period := as.Date(sprintf("%d-%02d-15",
                                   ref_month_yyyymm %/% 100,
                                   ref_month_yyyymm %% 100))]
  inequality_path <- file.path(dest_dir, "inequality_data.rds")
  saveRDS_atomic(ineq, inequality_path)

  # B. Income shares (skip uf — too many)
  shares_specs <- breakdown_specs[vapply(breakdown_specs, function(s) s$type != "uf", logical(1L))]
  shares <- compute_shares(d, months, shares_specs)
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
  lorenz <- compute_lorenz(d, months, lorenz_specs)
  lorenz_path <- file.path(dest_dir, "lorenz_data.rds")
  saveRDS_atomic(lorenz, lorenz_path)

  # D. Gini decomposition (overall, monthly)
  decomp <- compute_gini_decomp(d, months, income_vars, income_source_labels)
  decomp[, period := as.Date(sprintf("%d-%02d-15",
                                     ref_month_yyyymm %/% 100,
                                     ref_month_yyyymm %% 100))]
  decomp_path <- file.path(dest_dir, "income_decomposition_data.rds")
  saveRDS_atomic(decomp, decomp_path)

  c(inequality_path, shares_path, lorenz_path, decomp_path)
}

# helpers used above
compute_breakdowns <- function(d, months, specs, measure_fn) {
  out <- vector("list")
  k <- 0L
  for (sp in specs) {
    if (sp$type == "overall") {
      for (m in months) {
        sub <- d[ref_month_yyyymm == m]
        meas <- measure_fn(sub)
        if (is.null(meas)) next
        for (mn in names(meas)) {
          k <- k + 1L
          out[[k]] <- data.table::data.table(
            ref_month_yyyymm = m,
            breakdown_type = "overall",
            breakdown_value = "Nacional",
            measure = mn,
            value = meas[[mn]],
            n_obs = nrow(sub)
          )
        }
      }
    } else {
      grps <- d[!is.na(get(sp$col)), unique(get(sp$col))]
      grps <- grps[!is.na(grps)]
      for (g in grps) {
        for (m in months) {
          sub <- d[ref_month_yyyymm == m & get(sp$col) == g]
          meas <- measure_fn(sub)
          if (is.null(meas)) next
          for (mn in names(meas)) {
            k <- k + 1L
            out[[k]] <- data.table::data.table(
              ref_month_yyyymm = m,
              breakdown_type = sp$type,
              breakdown_value = g,
              measure = mn,
              value = meas[[mn]],
              n_obs = nrow(sub)
            )
          }
        }
      }
    }
  }
  data.table::rbindlist(out)
}

compute_shares <- function(d, months, specs) {
  out <- vector("list"); k <- 0L
  for (sp in specs) {
    if (sp$type == "overall") {
      for (m in months) {
        sub <- d[ref_month_yyyymm == m]
        if (nrow(sub) < 50L) next
        for (n_groups in c(5L, 10L)) {
          sh <- income_shares(sub$hhinc_pc, sub$weight_monthly, groups = n_groups)
          k <- k + 1L
          out[[k]] <- data.table::data.table(
            ref_month_yyyymm = m,
            breakdown_type = "overall",
            breakdown_value = "Nacional",
            group_type = if (n_groups == 5L) "quintile" else "decile",
            group_label = sh$group_label,
            share = sh$share
          )
        }
      }
    } else {
      grps <- d[!is.na(get(sp$col)), unique(get(sp$col))]
      grps <- grps[!is.na(grps)]
      for (g in grps) for (m in months) {
        sub <- d[ref_month_yyyymm == m & get(sp$col) == g]
        if (nrow(sub) < 50L) next
        for (n_groups in c(5L, 10L)) {
          sh <- income_shares(sub$hhinc_pc, sub$weight_monthly, groups = n_groups)
          k <- k + 1L
          out[[k]] <- data.table::data.table(
            ref_month_yyyymm = m,
            breakdown_type = sp$type,
            breakdown_value = g,
            group_type = if (n_groups == 5L) "quintile" else "decile",
            group_label = sh$group_label,
            share = sh$share
          )
        }
      }
    }
  }
  data.table::rbindlist(out)
}

compute_lorenz <- function(d, months, specs) {
  out <- vector("list"); k <- 0L
  for (sp in specs) {
    if (sp$type == "overall") {
      for (m in months) {
        sub <- d[ref_month_yyyymm == m]
        if (nrow(sub) < 50L) next
        lp <- lorenz_points(sub$hhinc_pc, sub$weight_monthly, n = 100L)
        lp[, ref_month_yyyymm := m]
        lp[, breakdown_type := "overall"]
        lp[, breakdown_value := "Nacional"]
        k <- k + 1L; out[[k]] <- lp
      }
    } else {
      grps <- d[!is.na(get(sp$col)), unique(get(sp$col))]
      grps <- grps[!is.na(grps)]
      for (g in grps) for (m in months) {
        sub <- d[ref_month_yyyymm == m & get(sp$col) == g]
        if (nrow(sub) < 50L) next
        lp <- lorenz_points(sub$hhinc_pc, sub$weight_monthly, n = 100L)
        lp[, ref_month_yyyymm := m]
        lp[, breakdown_type := sp$type]
        lp[, breakdown_value := g]
        k <- k + 1L; out[[k]] <- lp
      }
    }
  }
  data.table::rbindlist(out)
}

compute_gini_decomp <- function(d, months, income_vars, labels_dt) {
  out <- vector("list"); k <- 0L
  for (m in months) {
    sub <- d[ref_month_yyyymm == m]
    if (nrow(sub) < 100L) next
    decomp <- gini_decomposition(
      dt = sub,
      income_vars = income_vars,
      total_var = "hhinc_pc",
      weight_var = "weight_monthly"
    )
    decomp <- merge(decomp, labels_dt,
                    by.x = "income_source", by.y = "var", all.x = TRUE)
    decomp[, income_source := source_id]
    decomp[, source_id := NULL]
    decomp[, ref_month_yyyymm := m]
    k <- k + 1L; out[[k]] <- decomp
  }
  data.table::rbindlist(out)
}

# ------------------------------------------------------------------------------
# Layer 3 — poverty outputs (1 .rds)
# ------------------------------------------------------------------------------

build_poverty_outputs <- function(prepared_microdata_path,
                                  dest_dir,
                                  utils_inequality_path) {
  source(utils_inequality_path, local = TRUE)
  d <- fst::read_fst(prepared_microdata_path, as.data.table = TRUE)

  wb_lines <- get_wb_poverty_lines(reference_date = deflation_target_date)
  mw_table <- get_historical_minimum_wage()
  mw_table[, mw_quarter := salario_minimo / 4]
  mw_table[, mw_half := salario_minimo / 2]
  mw_table[, nominal_date := as.Date(sprintf("%d-07-01", ano))]
  mw_table[, mw_quarter_target := deflateBR::inpc(
    mw_quarter, nominal_dates = nominal_date,
    real_date = deflation_target_date
  )]
  mw_table[, mw_half_target := deflateBR::inpc(
    mw_half, nominal_dates = nominal_date,
    real_date = deflation_target_date
  )]

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
  months <- sort(unique(d$ref_month_yyyymm))

  out <- vector("list"); k <- 0L
  for (line_name in names(poverty_lines)) {
    pline <- poverty_lines[[line_name]]
    for (sp in breakdown_specs) {
      grps <- if (sp$type == "overall") list("Nacional")
              else as.list(d[!is.na(get(sp$col)), unique(get(sp$col))])
      grps <- grps[!vapply(grps, is.na, logical(1L))]
      for (g in grps) for (m in months) {
        sub <- if (sp$type == "overall") d[ref_month_yyyymm == m]
               else d[ref_month_yyyymm == m & get(sp$col) == g]
        if (nrow(sub) < 10L) next
        z <- if (pline$type == "fixed") pline$value
             else {
               yr <- m %/% 100L
               vr <- pline$values[ano == yr]
               if (!nrow(vr)) next
               vr$value[1L]
             }
        fgt <- fgt_all(sub$hhinc_pc, z, sub$weight_monthly)
        k <- k + 1L
        out[[k]] <- data.table::data.table(
          ref_month_yyyymm = m,
          poverty_line_id = line_name,
          poverty_line_value = z,
          breakdown_type = sp$type,
          breakdown_value = as.character(g),
          fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2,
          n_poor = fgt$n_poor, total_pop = fgt$total_pop,
          mean_income_poor = fgt$mean_income_poor,
          n_obs = nrow(sub)
        )
      }
    }
  }
  poverty_data <- data.table::rbindlist(out)
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
  dest
}

# ------------------------------------------------------------------------------
# Layer 3 — state_monthly_data.rds (geographic from microdata)
# ------------------------------------------------------------------------------

build_state_monthly <- function(acervo_manifest, dest_path) {
  set_fst_threads(2L)
  qf <- acervo_manifest[
    file_type == "quarterly" &
      status %in% c("OK", "DOWNLOADED_NEW", "REDOWNLOADED_REWEIGHT") &
      !is.na(local_path)
  ]
  if (!nrow(qf)) stop("No quarterly files available for state_monthly.")

  cols_needed <- c(
    "Ano", "Trimestre", "UF",
    "UPA", "Estrato", "V1008", "V1014", "V2003",
    "V2008", "V20081", "V20082", "V2009",
    "V1028", "posest", "posest_sxi",
    "VD4001", "VD4002", "VD4003", "VD4004", "VD4004A",
    "VD4005", "VD4009", "VD4010", "VD4012", "V4019"
  )
  pnadc <- data.table::rbindlist(lapply(qf$local_path, function(p) {
    avail <- intersect(cols_needed, names(fst::read_fst(p, from = 1L, to = 1L)))
    fst::read_fst(p, columns = avail, as.data.table = TRUE)
  }), fill = TRUE)

  numeric_cols <- c("V2009", "V1028", "VD4001", "VD4002", "VD4003", "VD4004",
                    "VD4004A", "VD4005", "VD4009", "VD4010", "VD4012", "V4019")
  for (col in numeric_cols) {
    if (col %in% names(pnadc) && !is.numeric(pnadc[[col]])) {
      pnadc[, (col) := as.numeric(get(col))]
    }
  }

  pnadc[, `:=`(
    pop14mais = 1L,
    pea = data.table::fifelse(VD4001 == 1, 1L, 0L),
    employed = data.table::fifelse(VD4002 == 1, 1L, 0L),
    unemployed = data.table::fifelse(VD4001 == 1 & VD4002 == 2, 1L, 0L),
    fora_forca = data.table::fifelse(VD4001 == 2, 1L, 0L),
    subocuphoras = data.table::fifelse(
      !is.na(VD4004A) & VD4004A == 1, 1L,
      data.table::fifelse(!is.na(VD4004) & VD4004 == 1, 1L, 0L)
    ),
    forcapotencial = data.table::fifelse(VD4003 == 1, 1L, 0L),
    desalentado = data.table::fifelse(VD4005 == 1, 1L, 0L),
    contribuinte = data.table::fifelse(VD4002 == 1 & VD4012 == 1, 1L, 0L)
  )]

  has_v4019 <- "V4019" %in% names(pnadc)
  pnadc[, `:=`(
    empregprivcomcart = data.table::fifelse(VD4009 == 1, 1L, 0L),
    empregprivsemcart = data.table::fifelse(VD4009 == 2, 1L, 0L),
    domesticocomcart  = data.table::fifelse(VD4009 == 3, 1L, 0L),
    domesticosemcart  = data.table::fifelse(VD4009 == 4, 1L, 0L),
    empregpublcomcart = data.table::fifelse(VD4009 == 5, 1L, 0L),
    empregpublsemcart = data.table::fifelse(VD4009 == 6, 1L, 0L),
    estatutmilitar    = data.table::fifelse(VD4009 == 7, 1L, 0L),
    empregador        = data.table::fifelse(VD4009 == 8, 1L, 0L),
    contapropria      = data.table::fifelse(VD4009 == 9, 1L, 0L),
    trabfamauxiliar   = data.table::fifelse(VD4009 == 10, 1L, 0L)
  )]
  if (has_v4019) {
    pnadc[, `:=`(
      contapropriacomcnpj = data.table::fifelse(VD4009 == 9 & V4019 == 1, 1L, 0L),
      contapropriasemcnpj = data.table::fifelse(VD4009 == 9 & V4019 == 2, 1L, 0L)
    )]
    pnadc[, informal := empregprivsemcart + domesticosemcart +
            contapropriasemcnpj + trabfamauxiliar]
  } else {
    pnadc[, informal := empregprivsemcart + domesticosemcart + contapropria + trabfamauxiliar]
  }
  pnadc[, `:=`(
    empregpriv = empregprivcomcart + empregprivsemcart,
    domestico = domesticocomcart + domesticosemcart,
    empregpubl = empregpublcomcart + empregpublsemcart + estatutmilitar,
    agropecuaria = data.table::fifelse(VD4010 == 1, 1L, 0L),
    industria    = data.table::fifelse(VD4010 == 2, 1L, 0L),
    construcao   = data.table::fifelse(VD4010 == 3, 1L, 0L),
    comercio     = data.table::fifelse(VD4010 == 4, 1L, 0L),
    transporte   = data.table::fifelse(VD4010 == 5, 1L, 0L),
    alojaliment  = data.table::fifelse(VD4010 == 6, 1L, 0L),
    infcomfinimobadm = data.table::fifelse(VD4010 == 7, 1L, 0L),
    adminpublica = data.table::fifelse(VD4010 == 8, 1L, 0L),
    outroservico = data.table::fifelse(VD4010 == 9, 1L, 0L),
    servicodomestico = data.table::fifelse(VD4010 == 10, 1L, 0L)
  )]
  pnadc[, servicos := transporte + alojaliment + infcomfinimobadm +
          adminpublica + outroservico + servicodomestico]

  crosswalk <- build_crosswalk(pnadc)
  pnadc <- apply_periods_quarterly(pnadc, crosswalk)
  pnadc <- pnadc[!is.na(weight_monthly) & !is.na(UF) & V2009 >= 14]

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
  dest_path
}

# ------------------------------------------------------------------------------
# Layer 3 — brazil_states_sf.rds (one-off; cached after first run)
# ------------------------------------------------------------------------------

build_brazil_states_sf <- function(dest_path) {
  states <- geobr::read_state(year = 2020, simplified = TRUE)
  states_simple <- rmapshaper::ms_simplify(states, keep = 0.01, keep_shapes = TRUE)
  states_simple <- sf::st_transform(states_simple, 4326)
  states_simple <- states_simple[, c("code_state", "abbrev_state",
                                     "name_state", "geom")]
  names(states_simple) <- c("uf_code", "uf_abbrev", "uf_name", "geometry")
  states_simple$uf_code <- as.character(as.integer(states_simple$uf_code))
  sf::st_geometry(states_simple) <- "geometry"

  saveRDS_atomic(states_simple, dest_path)
  dest_path
}

# ------------------------------------------------------------------------------
# Layer 3 — geographic_data.rds (SIDRA fallback; small, fast, no microdata)
# ------------------------------------------------------------------------------

build_geographic_fallback <- function(dest_path) {
  geographic_series <- list(
    taxadesocup = "/t/4093/n3/all/v/4099/p/all/d/v4099%201",
    taxapartic  = "/t/4092/n3/all/v/4096/p/all/d/v4096%201",
    nivelocup   = "/t/4094/n3/all/v/4097/p/all/d/v4097%201"
  )
  base_url <- "https://apisidra.ibge.gov.br/values"

  fetch_one <- function(name, path) {
    url <- paste0(base_url, path)
    response <- tryCatch(jsonlite::fromJSON(url, flatten = TRUE),
                        error = function(e) NULL)
    if (is.null(response) || !is.data.frame(response) || nrow(response) <= 1L)
      return(NULL)
    header <- as.character(response[1L, ])
    dat <- response[-1L, , drop = FALSE]
    names(dat) <- header
    dt <- data.table::as.data.table(dat)
    uf_col <- names(dt)[grepl("Unidade.*Codigo|UF.*Codigo", names(dt),
                              ignore.case = TRUE)][1L]
    p_col  <- names(dt)[grepl("Trimestre.*Codigo|Periodo.*Codigo",
                              names(dt), ignore.case = TRUE)][1L]
    if (is.na(uf_col) || is.na(p_col) || !"Valor" %in% names(dt)) return(NULL)
    out <- data.table::data.table(
      uf_code = as.integer(dt[[uf_col]]),
      anomesfinaltrimmovel = as.integer(dt[[p_col]]),
      value = as.numeric(gsub(",", ".", dt$Valor)),
      indicator = name
    )
    out[!is.na(value) & !is.na(uf_code)]
  }

  parts <- Map(fetch_one, names(geographic_series), geographic_series)
  parts <- parts[!vapply(parts, is.null, logical(1L))]
  if (!length(parts)) {
    warning("No SIDRA data for geographic fallback; writing empty stub.")
    geo <- data.table::data.table(
      uf_code = integer(), anomesfinaltrimmovel = integer(),
      value = numeric(), indicator = character()
    )
  } else {
    geo <- data.table::rbindlist(parts, fill = TRUE)
    geo <- geo[uf_code >= 11L & uf_code <= 53L]
    data.table::setorder(geo, indicator, anomesfinaltrimmovel, uf_code)
  }
  saveRDS_atomic(geo, dest_path)
  dest_path
}

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
