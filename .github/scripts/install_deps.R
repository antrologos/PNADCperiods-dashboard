# ==============================================================================
# Direct install of the R packages needed by fetch_sidra_daily.R.
# Used by the GitHub Actions workflow as a replacement for
# r-lib/actions/setup-r-dependencies (which kept solving conflicts against
# the noble binary repo for our particular extras combination).
# Cache lives at $R_LIBS_USER, restored/saved by actions/cache in the workflow.
# ==============================================================================

cat(R.version.string, "\n")

PKGS <- c("PNADCperiods", "qs2", "jsonlite", "data.table",
          "seasonal", "forecast", "httr2", "x13binary")

# RSPM Linux noble binary repo (ubuntu-latest = noble in 2026).
RSPM <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"

lib <- Sys.getenv("R_LIBS_USER")
if (lib == "") stop("R_LIBS_USER must be set by the workflow")
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(lib, .libPaths()))

# Install only what's missing or outdated; the actions/cache step keeps
# the lib persistent across runs, so this is fast on cache hits.
already <- rownames(installed.packages(lib.loc = lib))
missing <- setdiff(PKGS, already)

# Minimum required versions. Force upgrade when the cached lib is too
# old (e.g. cache pinned PNADCperiods 0.1.1 while CRAN now ships 0.1.2;
# the trailing-NA mask the dashboard relies on landed in 0.1.2).
MIN_VERSIONS <- list(PNADCperiods = "0.1.2")
upgrade <- character(0)
for (p in names(MIN_VERSIONS)) {
  if (p %in% already) {
    cur <- as.character(packageVersion(p, lib.loc = lib))
    if (utils::compareVersion(cur, MIN_VERSIONS[[p]]) < 0) {
      cat(sprintf("Upgrade required: %s %s -> >= %s\n",
                  p, cur, MIN_VERSIONS[[p]]))
      upgrade <- c(upgrade, p)
    }
  }
}

to_install <- unique(c(missing, upgrade))
if (length(to_install)) {
  cat("Installing:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, repos = RSPM, lib = lib, Ncpus = 2)
} else {
  cat("All packages already cached at required versions.\n")
}

cat("\nVersions:\n")
for (p in PKGS) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(sprintf("  %-15s %s\n", p,
              if (ok) as.character(packageVersion(p)) else "MISSING"))
  if (!ok) stop("Package ", p, " not installed")
}
