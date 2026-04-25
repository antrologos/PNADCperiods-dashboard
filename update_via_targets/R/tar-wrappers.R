# ==============================================================================
# tar-wrappers.R — wrappers around PNADCperiods (CRAN-pinned)
#
# All calls into the package live here so that, when PNADCperiods changes,
# this is the single file that needs to be inspected.
#
# tar_option_set(imports = "PNADCperiods") in _targets.R causes targets to
# track the hash of exported PNADCperiods symbols and invalidate downstream
# targets automatically when the package updates.
# ==============================================================================

# ------------------------------------------------------------------------------
# Crosswalk + apply
# ------------------------------------------------------------------------------

#' Build the mensalization crosswalk from stacked quarterly microdata.
build_crosswalk <- function(quarterly_dt) {
  PNADCperiods::pnadc_identify_periods(
    quarterly_dt,
    verbose = TRUE,
    store_date_bounds = TRUE
  )
}

#' Apply crosswalk to annual microdata + calibrate monthly weights.
apply_periods_annual <- function(annual_dt, crosswalk) {
  PNADCperiods::pnadc_apply_periods(
    annual_dt,
    crosswalk,
    weight_var = "V1032",
    anchor = "year",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = TRUE,
    verbose = TRUE
  )
}

#' Apply crosswalk to quarterly microdata (state_monthly pipeline).
apply_periods_quarterly <- function(quarterly_dt, crosswalk) {
  PNADCperiods::pnadc_apply_periods(
    quarterly_dt,
    crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    verbose = TRUE
  )
}

# ------------------------------------------------------------------------------
# Validation
# ------------------------------------------------------------------------------

validate_pnadc_minimal <- function(dt, stop_on_error = FALSE) {
  PNADCperiods::validate_pnadc(dt, stop_on_error = stop_on_error)
}
