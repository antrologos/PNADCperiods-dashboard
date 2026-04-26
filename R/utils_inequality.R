# ==============================================================================
# utils_inequality.R — backward-compatibility stub.
# ==============================================================================
#
# Real implementations split into 3 sibling files (PR2 of the DAG re-architecture):
#   - labels.R              (sex_label, race_label, uf_to_region, age_group, ...)
#   - measures_inequality.R (weighted_gini, lorenz_points, palma_ratio, ...)
#   - measures_poverty.R    (fgt, fgt_all, get_wb_poverty_lines, ...)
#
# The targets pipeline consumes the 3 sibling paths directly (labels_path,
# measures_inequality_path, measures_poverty_path) so a change in one does NOT
# invalidate builders that depend on others.
#
# This stub keeps `source("R/utils_inequality.R")` working for app.R and
# scripts/precompute_*.R, which load all symbols into globalenv at once.
# ==============================================================================

local({
  # Find own directory: walk sys.frames() for $ofile pointing at this file.
  here <- NULL
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && grepl("utils_inequality\\.R$", of, ignore.case = TRUE)) {
      here <- dirname(of); break
    }
  }
  # Fallbacks if $ofile is not set (e.g., test environments using sys.source()).
  if (is.null(here)) {
    candidates <- c("R", "PNADCperiods-dashboard/R",
                    "../R", "../../R", "../../../R")
    for (cand in candidates) {
      if (file.exists(file.path(cand, "labels.R"))) { here <- cand; break }
    }
  }
  if (is.null(here)) {
    stop("utils_inequality.R: cannot locate sibling R/ directory.",
         call. = FALSE)
  }
  for (f in c("labels.R", "measures_inequality.R", "measures_poverty.R")) {
    sys.source(file.path(here, f), envir = globalenv())
  }
})
