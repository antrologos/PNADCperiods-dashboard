# ==============================================================================
# tar-network.R — Centralized Layer 1 network operations
#
# Responsibilities (PR1 of DAG re-architecture):
#  - INPC factor lookup table: ONE deflateBR::inpc call per tar_make()
#    consolidating the 4 scattered calls in deflate_incomes / poverty MW /
#    WB poverty lines.
#  - geobr::read_state hoisted from L3 builder to its own L1 target so a
#    network failure surfaces with a clear target name, not mid-run.
#  - SIDRA geographic fallback (3 HTTP calls) hoisted similarly.
#
# Downstream targets consume the resulting in-memory data.tables / sf objects
# instead of making fresh API calls.
# ==============================================================================

# ------------------------------------------------------------------------------
# INPC factor lookup table (single API call)
# ------------------------------------------------------------------------------

#' Build INPC factor lookup table.
#'
#' Single API call to `deflateBR::inpc` covering ALL nominal_dates needed
#' downstream:
#'   - 2024-07-01 (deflate_incomes for hhinc_pc; constant)
#'   - 2021-07-01 (WB poverty lines; constant)
#'   - YYYY-07-01 for each year in the historical MW table (1990..current-1)
#'
#' @param real_date character "MM/YYYY" (validated via
#'   `validate_deflation_target_date`)
#' @param mw_years integer vector of MW reference years (default: 1990 through
#'   current calendar year - 1)
#' @return data.table(nominal_date Date, factor numeric)
compute_inpc_factors <- function(
    real_date,
    mw_years = 1990L:(as.integer(format(Sys.Date(), "%Y")) - 1L)) {
  validate_deflation_target_date(real_date)
  mw_dates <- as.Date(sprintf("%d-07-01", mw_years))
  fixed_dates <- as.Date(c("2021-07-01", "2024-07-01"))
  nominal_dates <- sort(unique(c(fixed_dates, mw_dates)))

  message(sprintf("INPC: fetching factors for %d nominal dates -> %s",
                  length(nominal_dates), real_date))
  factors <- deflateBR::inpc(
    rep(1, length(nominal_dates)),
    nominal_dates = nominal_dates,
    real_date = real_date
  )
  data.table::data.table(nominal_date = nominal_dates, factor = factors)
}

#' Lookup helper for inpc_factor_table.
#'
#' Returns the factor at the given nominal_date. Errors clearly when the date
#' is missing from the table, surfacing the bug rather than silently returning
#' NA.
#'
#' @param inpc_factor_table data.table with (nominal_date, factor)
#' @param nominal_date Date or character convertible to Date
#' @return single numeric factor
inpc_factor_at <- function(inpc_factor_table, nominal_date) {
  target_date <- if (inherits(nominal_date, "Date")) nominal_date else as.Date(nominal_date)
  hits <- inpc_factor_table[nominal_date == target_date, factor]
  if (!length(hits)) {
    stop(sprintf("INPC factor for %s not found in factor table",
                 as.character(target_date)),
         call. = FALSE)
  }
  hits[1L]
}

# ------------------------------------------------------------------------------
# Deflator XLS parser (PR4: extracted from deflate_incomes)
# ------------------------------------------------------------------------------

#' Parse the IBGE annual deflator XLS into a tidy data.table.
#'
#' Extracted from `deflate_incomes` so the XLS parsing happens ONCE per
#' tar_make() (in the `deflator_dt` target) instead of every time
#' `deflate_incomes` is called.
#'
#' @param deflator_path character path to deflator_PNADC_<YYYY>.xls
#' @return data.table(Ano, Trimestre, UF, CO2, CO2e, CO3) — keys ready for
#'   merge against microdata on (Ano, Trimestre, UF).
read_deflator_xls <- function(deflator_path) {
  if (!file.exists(deflator_path))
    stop("Deflator file not found: ", deflator_path, call. = FALSE)
  deflator <- readxl::read_excel(deflator_path)
  data.table::setDT(deflator)
  deflator <- deflator[, .(Ano = ano, Trimestre = trim, UF = uf, CO2, CO2e, CO3)]
  deflator[, `:=`(Ano = as.numeric(Ano),
                  Trimestre = as.numeric(Trimestre),
                  UF = as.numeric(UF))]
  deflator
}

# ------------------------------------------------------------------------------
# IPCA index series (Phase 2: replaces the IBGE deflator XLS files)
# ------------------------------------------------------------------------------

#' Fetch IPCA monthly index from SIDRA (table 1737, variable 2266: base
#' dez/1993 = 100). Used by the Phase 2 IPCA-based deflation that
#' replaces the IBGE per-UF deflator (CO1/CO2/CO3) and the per-quarter
#' habitual/efetivo deflators.
#'
#' @return data.table(yyyymm integer, ipca_index numeric), one row per
#'   month from 199401 (Jan 1994) onward. The index is national (no UF
#'   variation) — same value applied to every observation in the
#'   downstream deflation step.
fetch_ipca_series <- function(max_retries = 3) {
  api_path <- "/t/1737/n1/all/v/2266/p/all/d/v2266%2013"
  attempt <- 1L
  raw <- NULL
  while (attempt <= max_retries) {
    raw <- tryCatch(
      sidrar::get_sidra(api = api_path),
      error = function(e) {
        message(sprintf("fetch_ipca_series: attempt %d failed: %s",
                        attempt, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(raw)) break
    attempt <- attempt + 1L
    Sys.sleep(2)
  }
  if (is.null(raw)) {
    stop("fetch_ipca_series: SIDRA fetch failed after ",
         max_retries, " attempts.", call. = FALSE)
  }

  data.table::setDT(raw)
  # SIDRA returns "Mês (Código)" with values like "199401" (string).
  mes_col <- grep("^M.s.*C.digo", names(raw), value = TRUE)
  if (!length(mes_col))
    stop("fetch_ipca_series: 'Mês (Código)' column not found", call. = FALSE)
  out <- data.table::data.table(
    yyyymm = as.integer(raw[[mes_col[1L]]]),
    ipca_index = as.numeric(raw[["Valor"]])
  )
  out <- out[!is.na(yyyymm) & !is.na(ipca_index)]
  data.table::setkey(out, yyyymm)
  out
}

# ------------------------------------------------------------------------------
# Brazil states geometries (geobr hoisted from L3)
# ------------------------------------------------------------------------------

#' Fetch raw Brazil states sf from geobr.
#'
#' Hoisted from `build_brazil_states_sf` (which now is a thin transform
#' consuming this raw target). Geobr handles its own caching internally; this
#' wrapper exists so targets can detect a network failure with a clear target
#' name and so subsequent runs reuse the cached value.
#'
#' @param year integer year passed to `geobr::read_state`
#' @return sf object with 27 features (states)
fetch_brazil_states_sf <- function(year = 2020L) {
  geobr::read_state(year = year, simplified = TRUE)
}

# ------------------------------------------------------------------------------
# (removed) SIDRA geographic fallback fetcher
# ------------------------------------------------------------------------------
#
# `fetch_sidra_geographic` was deleted along with the `sidra_geographic_raw`
# and `geographic_fallback_asset` targets. The 3 hardcoded SIDRA URLs (tables
# 4092/4093/4094) never worked completely — 2 of 3 always returned HTTP 400
# because the variable codes didn't exist on those tables. Hecksher's
# reference Stata code derives taxadesocup/taxapartic/nivelocup from
# microdata directly, and `state_monthly_data.rds` (state_monthly_asset)
# already provides these 3 indicators at UF-month granularity. Dashboard
# global.R prioritises state_monthly_data over the deleted fallback.
