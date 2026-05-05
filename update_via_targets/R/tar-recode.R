# ==============================================================================
# tar-recode.R — Layer 2 once-per-dataset recoding
#
# Responsibilities (PR3 of DAG re-architecture):
#  - recode_quarterly: ALL quarterly recoding in one place — type coercion,
#    labor-market flags (employed/informal/etc.), VD4004/VD4004A cutoff,
#    apply_periods_quarterly, V2009>=14 filter. The result feeds
#    state_monthly_asset (and is available for any future quarterly
#    aggregator) without re-doing the same recodings ad hoc.
#
# This realises principle 8 of the user's vision: per-dataset recoding
# done ONCE, not duplicated across builders.
# ==============================================================================

#' Recode the quarterly stack: type coercion + labor flags + crosswalk apply.
#'
#' Extracted from the prep prelude of the legacy `build_state_monthly`. Logic
#' is bit-identical; only the surrounding I/O moves out. After PR3,
#' `build_state_monthly` is a thin aggregator that consumes this target and
#' produces state_monthly_data.rds.
#'
#' @param quarterly_stacked data.table from `stack_quarterly`
#' @param crosswalk data.table from `crosswalk_target`
#' @return data.table with all derived labor flags + ref_month_yyyymm +
#'   weight_monthly, filtered to V2009>=14 and non-NA UF/weight.
recode_quarterly <- function(quarterly_stacked, crosswalk,
                             deflator_dt = NULL, inpc_factor = NULL,
                             labels_path = NULL) {
  pnadc <- data.table::copy(quarterly_stacked)

  numeric_cols <- c("Ano", "Trimestre",
                    "V2009", "V1028", "VD4001", "VD4002", "VD4003", "VD4004",
                    "VD4004A", "VD4005", "VD4009", "VD4010", "VD4012", "V4019",
                    "VD4016", "VD4017", "VD4019", "VD4020")
  for (col in numeric_cols) {
    if (col %in% names(pnadc) && !is.numeric(pnadc[[col]])) {
      pnadc[, (col) := as.numeric(get(col))]
    }
  }

  # VD4004 vs VD4004A: SIDRA backfills tables 6438/6785 to 201203 using VD4004
  # for 2012-Q1 to 2015-Q3 and VD4004A from 2015-Q4 onward. Mirror that exact
  # cutoff via `vd4004_split_yyyymm` (single source of truth in tar-config.R).
  # Pre-Q4-2015 microdata has no VD4004A column, so VD4004 is the only choice;
  # from 2015-Q4 onward VD4004A is canonical (efetivamente vs. habitualmente
  # trabalhadas — IBGE treats them as distinct indicators).
  pnadc_yyyymm_q <- pnadc$Ano * 100L + pnadc$Trimestre * 3L  # last month of quarter
  is_pre_split <- pnadc_yyyymm_q <= vd4004_split_yyyymm

  pnadc[, `:=`(
    pop14mais = 1L,
    pea = data.table::fifelse(VD4001 == 1, 1L, 0L),
    employed = data.table::fifelse(VD4001 == 1 & VD4002 == 1, 1L, 0L),
    unemployed = data.table::fifelse(VD4001 == 1 & VD4002 == 2, 1L, 0L),
    fora_forca = data.table::fifelse(VD4001 == 2, 1L, 0L),
    subocuphoras = data.table::fifelse(
      is_pre_split,
      data.table::fifelse(!is.na(VD4004) & VD4004 == 1, 1L, 0L),
      data.table::fifelse(!is.na(VD4004A) & VD4004A == 1, 1L, 0L)
    ),
    forcapotencial = data.table::fifelse(VD4003 == 1, 1L, 0L),
    desalentado = data.table::fifelse(VD4005 == 1, 1L, 0L),
    contribuinte = data.table::fifelse(VD4002 == 1 & VD4012 == 1, 1L, 0L)
  )]

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

  # Per-row CNPJ logic for self-employed informality.
  # When V4019 is absent OR NA on a given row, conservatively treat the
  # self-employed person as "sem CNPJ" (informal). This is robust to mixed
  # stacks where some quarters have V4019 and others don't (rbindlist fills
  # NA for missing columns).
  if ("V4019" %in% names(pnadc)) {
    pnadc[, contapropriasemcnpj := data.table::fifelse(
      VD4009 == 9,
      data.table::fifelse(is.na(V4019) | V4019 == 2, 1L, 0L),
      0L
    )]
  } else {
    pnadc[, contapropriasemcnpj := contapropria]
  }
  pnadc[, informal := empregprivsemcart + domesticosemcart +
          contapropriasemcnpj + trabfamauxiliar]
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

  pnadc <- PNADCperiods::pnadc_apply_periods(
    pnadc, crosswalk,
    weight_var = "V1028", anchor = "quarter",
    calibrate = TRUE, verbose = TRUE
  )
  pnadc <- pnadc[!is.na(weight_monthly) & !is.na(UF) & V2009 >= 14]

  # ---- Individual labor income (deflated) + demographic groupings ----
  # When deflator_dt + inpc_factor + labels_path are supplied, attach
  # 4 deflated labor-income columns (renda_hab_princ, renda_efe_princ,
  # renda_hab_todos, renda_efe_todos) and the demographic grouping
  # columns (sexo, raca, faixa_educ, regiao, uf_abbrev, urbano,
  # faixa_idade) so quarterly_income_aggregates can roll them up by
  # (month × breakdown_type × breakdown_value). Keeping these args
  # optional preserves callers (state_monthly_asset) that don't need
  # the income columns.
  inc_vars <- c("VD4016", "VD4017", "VD4019", "VD4020")
  inc_out  <- c("renda_hab_princ", "renda_efe_princ",
                "renda_hab_todos", "renda_efe_todos")
  has_income_vars <- all(inc_vars %in% names(pnadc))

  if (!is.null(deflator_dt) && is.numeric(inpc_factor) && has_income_vars) {
    pnadc[, UF := as.numeric(UF)]
    deflator <- data.table::copy(deflator_dt)
    data.table::setkeyv(deflator, c("Ano", "Trimestre", "UF"))
    data.table::setkeyv(pnadc,    c("Ano", "Trimestre", "UF"))
    pnadc <- deflator[pnadc]
    for (i in seq_along(inc_vars)) {
      v   <- inc_vars[i]
      out <- inc_out[i]
      pnadc[, (out) := data.table::fifelse(
        is.na(get(v)),
        NA_real_,
        get(v) * CO2 * inpc_factor
      )]
    }
    pnadc[, inpc_factor := inpc_factor]
  } else {
    for (out in inc_out) pnadc[, (out) := NA_real_]
  }

  if (!is.null(labels_path)) {
    # Rename demographic vars to lowercase to match build_demographic_groupings
    demog_old <- c("V2007", "V2010", "V1022", "VD3004")
    demog_new <- c("v2007", "v2010", "v1022", "vd3004")
    for (i in seq_along(demog_old)) {
      if (demog_old[i] %in% names(pnadc)) {
        data.table::setnames(pnadc, demog_old[i], demog_new[i])
      }
    }
    source(labels_path)  # build_demographic_groupings sources sex_label etc.
    pnadc <- build_demographic_groupings(pnadc)
  }

  pnadc
}


# ==============================================================================
# Annual recode (PR4)
# ==============================================================================

#' Recode the annual stack: apply_periods + V2005 filter + deflation +
#' per-capita components + demographic groupings.
#'
#' Extracted from `build_prepared_microdata` (PR4). After this target,
#' the data has `ref_month_yyyymm`, `weight_monthly`, deflated income
#' columns, the 8 per-capita components, demographic labels — everything
#' needed by Layer 3 builders. `build_prepared_microdata` becomes a thin
#' writer that selects + writes prepared_microdata.fst.
#'
#' Sources `labels_path` so `build_demographic_groupings` (in tar-microdata.R,
#' globalenv) can call sex_label / race_label / etc. via lexical scoping.
#'
#' @param annual_stacked data.table from `stack_annual`
#' @param crosswalk data.table from `crosswalk_target`
#' @param deflator_dt data.table from `read_deflator_xls` (in tar-network.R)
#' @param inpc_factor numeric scalar for mid-2024 → real_date
#' @param labels_path path to R/labels.R
#' @return data.table with all derived income/demographic columns; rows with
#'   NA `ref_month_yyyymm` retained (filter happens in the thin writer).
recode_annual <- function(annual_stacked, crosswalk, deflator_dt,
                          inpc_factor, labels_path) {
  source(labels_path)  # into globalenv: build_demographic_groupings needs labels

  det_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]
  message(sprintf("Crosswalk determination rate: %.1f%%", 100 * det_rate))

  # Standardize join keys to uppercase (matches pnadc_apply_periods expectations)
  annual_data <- data.table::copy(annual_stacked)
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

  d <- PNADCperiods::pnadc_apply_periods(
    annual_data, crosswalk,
    weight_var = "V1032", anchor = "year",
    calibrate = TRUE, calibration_unit = "month",
    smooth = TRUE, verbose = TRUE
  )
  rm(annual_data); gc()

  # Filter to IBGE's household-income membership: keep all residents,
  # excluding only V2005 in {17, 18, 19} (pensionista, empregado domestico,
  # parente do empregado domestico). Matches IBGE VD2003 / VD3003.
  d <- d[!v2005 %in% c(17L, 18L, 19L)]

  # Deflate (deflator_dt + inpc_factor pre-computed in L1; no I/O here)
  d <- deflate_incomes(d, deflator_dt, inpc_factor)
  d <- build_pc_income_components(d)
  d <- build_demographic_groupings(d)
  d
}
