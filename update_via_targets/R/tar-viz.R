# ==============================================================================
# tar-viz.R — visualization helpers
#
# `targets::tar_visnetwork()` colors nodes by `tar_outdated()` by default.
# `tar_outdated()` is a conservative static analysis: any descendant of a
# `cue = always` target is reported as "outdated" — even when the cue=always
# target produces the same value across runs and tar_make() actually skips
# the descendant. Visually, this makes most of the DAG look blue/outdated
# even though the pipeline is fully idempotent at execution time.
#
# `tar_visnetwork_honest()` shows the DAG colored only by `tar_progress()`
# (last-run status: completed / skipped / errored), which is the honest
# "what tar_make() actually did" view.
# ==============================================================================

#' Render the DAG colored by last-run progress, not by tar_outdated().
#'
#' Equivalent to `targets::tar_visnetwork(outdated = FALSE, ...)`. Use this
#' instead of `tar_visnetwork()` when the conservative outdated cascade
#' (driven by the cue=always `external_state_check` target) is misleading.
#'
#' @param ... forwarded to `targets::tar_visnetwork`
tar_visnetwork_honest <- function(...) {
  targets::tar_visnetwork(outdated = FALSE, ...)
}
