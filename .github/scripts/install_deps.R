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

# TEMPORARY: packages sourced from GitHub instead of RSPM.
#
# IBGE put apisidra.ibge.gov.br behind a Cloudflare challenge in September
# 2026, which broke every SIDRA fetch in PNADCperiods 0.1.2 (the version on
# CRAN). The fix -- fetching through the aggregated-data API v3 -- lives on
# the dev branch and ships in 0.1.3. Drop this and the MIN_VERSIONS bump
# back to RSPM once 0.1.3 is on CRAN.
GITHUB_PKGS <- c(PNADCperiods = "antrologos/PNADCperiods@dev")

lib <- Sys.getenv("R_LIBS_USER")
if (lib == "") stop("R_LIBS_USER must be set by the workflow")
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(lib, .libPaths()))

# Install only what's missing or outdated; the actions/cache step keeps
# the lib persistent across runs, so this is fast on cache hits.
already <- rownames(installed.packages(lib.loc = lib))
missing <- setdiff(PKGS, already)

# Minimum required versions. Force upgrade when the cached lib is too
# old (e.g. cache pinned PNADCperiods 0.1.2 while the SIDRA v3 migration
# the daily fetch now depends on only landed in 0.1.3).
MIN_VERSIONS <- list(PNADCperiods = "0.1.3")
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
from_github <- intersect(to_install, names(GITHUB_PKGS))
from_rspm   <- setdiff(to_install, from_github)

if (length(from_rspm)) {
  cat("Installing from RSPM:", paste(from_rspm, collapse = ", "), "\n")
  install.packages(from_rspm, repos = RSPM, lib = lib, Ncpus = 2)
}

if (length(from_github)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = RSPM, lib = lib, Ncpus = 2)
  }
  for (p in from_github) {
    cat("Installing from GitHub:", p, "<-", GITHUB_PKGS[[p]], "\n")
    # Dependencies still come from the RSPM binaries; only the package
    # itself is built from source, and it has no compiled code.
    remotes::install_github(GITHUB_PKGS[[p]], lib = lib, repos = RSPM,
                            upgrade = "never", dependencies = TRUE)
  }
}

if (!length(to_install)) {
  cat("All packages already cached at required versions.\n")
}

cat("\nVersions:\n")
for (p in PKGS) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(sprintf("  %-15s %s\n", p,
              if (ok) as.character(packageVersion(p)) else "MISSING"))
  if (!ok) stop("Package ", p, " not installed")
}
