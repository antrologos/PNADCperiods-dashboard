# ==============================================================================
# Dashboard-side taxonomy for SIDRA series
# ==============================================================================
# The PNADCperiods package ships a canonical taxonomy of its 90 series via
# get_sidra_series_metadata(). For dashboard display we apply a small remap
# (theme/category renames + explicit row order) without touching the package.
#
# Remap rules (kept idempotent: re-applying does nothing):
#   - theme "social_protection"          -> "labor_market"
#   - theme_category "participation"     -> "participation_and_occupation"
#   - theme_category "unemployment"      -> "participation_and_occupation"
#   - theme_category "social_security"   -> "participation_and_occupation"
#
# Row order is set from .display_series_order below. Series whose names are
# not in that vector keep the package's natural order at the end (defensive
# behaviour for future package additions).
# ==============================================================================

# Display order for the 90 SIDRA series, as approved in
# d:/tmp/dashboard_series_inventory.xlsx (sheet 1_Series_Explorer).
.display_series_order <- c(
  # labor_market > participation_and_occupation > levels
  "popdesocup", "popforadaforca", "popnaforca", "popocup",
  # labor_market > participation_and_occupation > <NA>
  "contribuinteprev",
  # labor_market > participation_and_occupation > rates
  "taxapartic", "taxadesocup", "niveldesocup", "nivelocup",
  # labor_market > employment_type > domestic
  "domestico", "domesticocomcart", "domesticosemcart",
  # labor_market > employment_type > employees
  "empregado", "empregpriv", "empregprivcomcart", "empregprivsemcart",
  "empregpubl", "empregpublcomcart", "empregpublsemcart", "estatutmilitar",
  # labor_market > employment_type > employers
  "empregador", "empregadorcomcnpj", "empregadorsemcnpj",
  # labor_market > employment_type > family_workers
  "trabfamauxiliar",
  # labor_market > employment_type > self_employed
  "contapropria", "contapropriacomcnpj", "contapropriasemcnpj",
  # labor_market > underutilization > levels
  "desalentado", "forcaampliada", "forcapotencial", "subocuphoras",
  # labor_market > underutilization > rates
  "perccontribprev", "percdesalento", "taxacombdesopot", "taxacombdesosub",
  "taxacompsubutlz", "taxasubocuphoras",
  # labor_market > economic_sector > <NA>
  "adminpublica", "agropecuaria", "alojaliment", "comercio", "construcao",
  "industria", "infcomfinimobadm", "outroservico", "servicodomestico",
  "transporte",
  # earnings > average_usual > all_jobs
  "rendefetrealprinc", "rendhabnominaltodos", "rendhabrealprinc",
  "rendhabrealtodos",
  # earnings > average_usual > by_economic_sector
  "rhrpadminpublica", "rhrpagropecuaria", "rhrpalojaliment", "rhrpcomercio",
  "rhrpconstrucao", "rhrpindustria", "rhrpinfcomfinimobadm",
  "rhrpoutroservico", "rhrpservicodomestico", "rhrptransporte",
  # earnings > average_usual > by_employment_type
  "rhrpcontapropria", "rhrpcontapropriacomcnpj", "rhrpcontapropriasemcnpj",
  "rhrpdomestico", "rhrpdomesticocomcart", "rhrpdomesticosemcart",
  "rhrpempregado", "rhrpempregador", "rhrpempregadorcomcnpj",
  "rhrpempregadorsemcnpj", "rhrpempregpriv", "rhrpempregprivcomcart",
  "rhrpempregprivsemcart", "rhrpempregpubl", "rhrpempregpublcomcart",
  "rhrpempregpublsemcart", "rhrpestatutmilitar",
  # earnings > wage_mass > effective
  "massaefetnominaltodos", "massaefetrealtodos",
  # earnings > wage_mass > usual
  "massahabnominaltodos", "massahabrealtodos",
  # earnings > average_effective > all_jobs
  "rendefetnominaltodos", "rendefetrealtodos",
  # demographics > total
  "populacao",
  # demographics > working_age
  "pop14mais",
  # prices > deflators
  "inpc100dez1993", "inpcvarmensal", "ipca100dez1993", "ipcavarmensal"
)

#' Apply dashboard-specific taxonomy to SIDRA series metadata
#'
#' Renames a few theme/theme_category values and reorders the rows so that
#' the dropdowns in the Series Explorer reflect the organization defined in
#' `.display_series_order`. Defensive against missing input.
#'
#' @param metadata A data.table with at least columns theme, theme_category,
#'   subcategory, series_name (typically the output of
#'   PNADCperiods::get_sidra_series_metadata() loaded from .qs2).
#' @return A new data.table with remapped categories and reordered rows.
#'   Series not listed in `.display_series_order` keep the input's relative
#'   order at the tail.
#' @keywords internal
apply_dashboard_taxonomy <- function(metadata) {
  if (is.null(metadata)) return(metadata)
  if (!data.table::is.data.table(metadata)) {
    metadata <- data.table::as.data.table(metadata)
  } else {
    metadata <- data.table::copy(metadata)
  }

  required <- c("theme", "theme_category", "subcategory", "series_name")
  if (!all(required %in% names(metadata))) return(metadata)

  metadata[theme == "social_protection", theme := "labor_market"]
  metadata[theme_category == "participation",
           theme_category := "participation_and_occupation"]
  metadata[theme_category %in% c("unemployment", "social_security"),
           theme_category := "participation_and_occupation"]

  metadata[, .__order := match(series_name, .display_series_order)]
  data.table::setorder(metadata, .__order, na.last = TRUE)
  metadata[, .__order := NULL]

  metadata[]
}
