# ==============================================================================
# tar-watch.R — external state checker
#
# Single `cue = always` target body. Returns a snapshot of:
#   - calendar (year, quarter)             — drives year/quarter rollover
#   - ftp_catalog (full IBGE FTP listing)  — drives MISSING / OUTDATED
#                                            detection in acervo plans
#
# When the snapshot's hash is identical to the previous run, downstream
# stays cached. Network failure falls back to a sticky JSON cache so
# IBGE outages don't break tar_make.
# ==============================================================================

#' Compute the full external-state snapshot.
#'
#' @param acervo_root unused here today (reserved for future use); kept
#'   in signature for backwards compatibility with the dependency graph.
#' @param state_path path to the FTP catalog sticky cache
#' @param fetch_fn injectable (defaults to `fetch_ibge_ftp_catalog`) so
#'   unit tests can swap in a mock without hitting the network.
#' @return list(calendar, ftp_catalog)
compute_external_state <- function(acervo_root,
                                   state_path = NULL,
                                   fetch_fn = NULL) {
  if (is.null(fetch_fn)) fetch_fn <- fetch_ibge_ftp_catalog

  cat <- fetch_fn(state_path = state_path)
  # Strip wall-clock timestamps so the hashed return value is content-only
  # — otherwise every tar_make would invalidate the whole downstream
  # chain just because `fetched_at` advanced. The actual fetch instant
  # is still persisted in the sticky-cache JSON.
  cat$fetched_at <- NULL
  cat$fetched_at_cached <- NULL

  list(
    calendar = list(
      year    = as.integer(format(Sys.Date(), "%Y")),
      quarter = as.integer(ceiling(as.integer(format(Sys.Date(), "%m")) / 3))
    ),
    ftp_catalog = cat
  )
}
