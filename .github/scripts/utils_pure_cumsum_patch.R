# =============================================================================
# Pure-cumsum monkey-patch for PNADCperiods <= 0.1.2
# =============================================================================
#
# TEMPORARY PATCH. Remove the source() of this file from
# fetch_sidra_daily.R when CRAN ships PNADCperiods >= 0.1.3.
#
# Tracking issue:
#   https://github.com/antrologos/PNADCperiods-dashboard/issues  (open new)
#   https://github.com/antrologos/PNADCperiods/issues             (paired)
#
# Background
# ----------
# In PNADCperiods <= 0.1.2, mensalize_sidra_series() runs a final adjustment
# step (.apply_final_adjustment) that re-anchors each (Jan, Feb, Mar)-style
# trio to its rolling-quarter mean using rq_lead1 / rq_lead2. Whenever IBGE
# publishes the rolling quarter ending in the third month of a trio, all
# three monthly values in that trio shift by the same delta
# (rq[t] - mean(y_jan, y_feb, y_mar)). For published series this delta is
# nonzero, so previously published months drift on every IBGE update.
#
# The dev branch of PNADCperiods (v0.1.3) drops that final-adjustment step;
# but v0.1.3 has no CRAN release date. Until then the dashboard reaches into
# the package namespace and replaces .apply_final_adjustment with the
# identity function, which makes mensalize_sidra_series produce the same
# pure-cumsum output v0.1.3 will produce by default.
#
# Safety
# ------
# .apply_final_adjustment is only called from .mensalize_single_series and
# from both halves of .mensalize_split_series; nothing else in the package
# depends on it. Running .compute_cumsum_by_mesnotrim and y0 + cum, which is
# the rest of the algorithm, is unchanged.
# =============================================================================

apply_pure_cumsum_patch <- function() {
  pkg_ver <- utils::packageVersion("PNADCperiods")
  if (pkg_ver >= "0.1.3") {
    message("[pure-cumsum patch] skipped: PNADCperiods ", pkg_ver,
            " already pure-cumsum by default")
    return(invisible("skipped"))
  }

  utils::assignInNamespace(
    ".apply_final_adjustment",
    function(y, rq, mesnotrim) y,
    ns = "PNADCperiods"
  )
  message("[pure-cumsum patch] active: PNADCperiods ", pkg_ver,
          " final adjustment bypassed (mensalization_method = pure_cumsum_patch_v1)")
  invisible("applied")
}
