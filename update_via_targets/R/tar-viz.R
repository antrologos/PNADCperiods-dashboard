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
# (last-run status: completed / skipped / errored). Plus it spreads nodes
# over the full canvas using a hierarchical Sugiyama layout, so the DAG
# uses the available drawing area instead of huddling in a corner.
# ==============================================================================

#' Render the DAG colored by last-run progress, with full-canvas layout.
#'
#' Improvements over `targets::tar_visnetwork()`:
#'   - `outdated = FALSE` so the cue=always cascade doesn't paint the whole
#'     DAG blue.
#'   - Hierarchical Sugiyama layout (left-to-right) so the graph spreads
#'     over the canvas instead of clumping.
#'   - Widget configured with `width = "100%"`, `height = "100%"` and
#'     scaled font so it fills the RStudio Viewer pane.
#'
#' @param level_separation horizontal spacing between layers (Sugiyama).
#' @param node_distance node spacing within a layer.
#' @param ... forwarded to `targets::tar_visnetwork`
tar_visnetwork_honest <- function(level_separation = 220,
                                  node_distance = 110, ...) {
  nw <- targets::tar_visnetwork(outdated = FALSE, ...)
  nw <- visNetwork::visIgraphLayout(nw, layout = "layout_with_sugiyama",
                                    physics = FALSE, smooth = FALSE)
  nw <- visNetwork::visHierarchicalLayout(
    nw,
    direction = "LR",
    sortMethod = "directed",
    levelSeparation = level_separation,
    nodeSpacing = node_distance,
    blockShifting = TRUE, edgeMinimization = TRUE,
    parentCentralization = TRUE
  )
  nw <- visNetwork::visNodes(nw, font = list(size = 18))
  nw <- visNetwork::visOptions(nw, highlightNearest = list(
    enabled = TRUE, degree = 1, hover = TRUE
  ))
  # Make the widget fill its container
  nw$width <- "100%"
  nw$height <- "100%"
  nw$sizingPolicy$defaultWidth <- "100%"
  nw$sizingPolicy$defaultHeight <- "100%"
  nw$sizingPolicy$browser$fill <- TRUE
  nw$sizingPolicy$viewer$fill <- TRUE
  nw
}

#' Save the DAG to a standalone HTML file and open it in a browser.
#'
#' Bypasses RStudio's Viewer-pane sizing constraints — the browser tab
#' uses full window real estate and zoom works naturally.
#'
#' @param path output HTML path
#' @param open whether to open the file in a browser after saving
#' @param ... forwarded to `tar_visnetwork_honest`
tar_visnetwork_save <- function(path = "dag.html", open = TRUE, ...) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("install.packages('htmlwidgets')")
  }
  nw <- tar_visnetwork_honest(...)
  htmlwidgets::saveWidget(nw, path, selfcontained = TRUE)
  if (isTRUE(open)) utils::browseURL(path)
  invisible(normalizePath(path))
}
