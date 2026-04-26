# ==============================================================================
# tar-deploy.R — last DAG step: deploy dashboard to shinyapps.io
#
# Used by `dashboard_deployed` target. Re-uses `scripts/deploy.R` from the
# dashboard root (which handles .Renviron loading + .qs2 pre-deploy + the
# `rsconnect::deployApp` call).
#
# Deploy is gated by THREE conditions, each producing an early-return with
# a `reason` string:
#  1. PIPELINE_MODE must be `live` (staging never publishes).
#  2. PNADC_AUTO_DEPLOY env var must be `"1"` (default; explicit "0" opts out).
#  3. SHINYAPPS_TOKEN must be present (loaded from .Renviron if needed).
#  4. pipeline_done must report 0 FAILED + 0 INVALID acervo files.
# ==============================================================================

#' Conditionally deploy the dashboard to shinyapps.io.
#'
#' @param pipeline_done value of the `pipeline_done` target (a list with
#'   `$mode`, `$n_failed`, `$n_invalid`)
#' @param dashboard_root absolute path to the dashboard root (where
#'   `scripts/deploy.R` and `.Renviron` live)
#' @param auto_deploy_env value of `Sys.getenv("PNADC_AUTO_DEPLOY", "1")`;
#'   "1" = enabled, anything else = skip
#' @param deploy_fn function(deploy_script_path) that performs the actual
#'   deploy. Default sources `scripts/deploy.R`. Tests inject a no-op stub.
#' @return list(deployed = TRUE/FALSE, reason = chr if FALSE,
#'              ts = ISO8601, url = chr if TRUE)
deploy_dashboard_if_eligible <- function(pipeline_done, dashboard_root,
                                         auto_deploy_env = "1",
                                         deploy_fn = function(p)
                                           source(p, local = TRUE)) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  skip <- function(reason) list(deployed = FALSE, reason = reason, ts = ts)

  # Gate 1: live mode
  if (!identical(pipeline_done$mode, "live")) return(skip("staging mode"))

  # Gate 2: opt-out
  if (!identical(auto_deploy_env, "1")) {
    return(skip("PNADC_AUTO_DEPLOY != 1"))
  }

  # Gate 3: pipeline integrity
  n_bad <- sum(pipeline_done$n_failed %||% 0L,
               pipeline_done$n_invalid %||% 0L,
               na.rm = TRUE)
  if (n_bad > 0L) return(skip("pipeline had failures/invalids"))

  # Gate 4: credentials (load .Renviron if needed)
  if (Sys.getenv("SHINYAPPS_TOKEN") == "") {
    renviron <- file.path(dashboard_root, ".Renviron")
    if (file.exists(renviron)) readRenviron(renviron)
  }
  if (Sys.getenv("SHINYAPPS_TOKEN") == "") {
    return(skip("no SHINYAPPS credentials"))
  }

  # Gate 5: deploy script must exist
  deploy_script <- file.path(dashboard_root, "scripts", "deploy.R")
  if (!file.exists(deploy_script)) return(skip("deploy.R not found"))

  # Run the deploy (mockable for tests)
  deploy_fn(deploy_script)

  list(
    deployed = TRUE,
    ts  = ts,
    url = sprintf("https://%s.shinyapps.io/PNADCperiods-dashboard/",
                  Sys.getenv("SHINYAPPS_NAME"))
  )
}

# Local-only fallback for the %||% operator if not already loaded from
# tar-network.R (which defines it). Defensive — tar-watch is sourced first.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
