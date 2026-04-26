# ==============================================================================
# tar-ftp.R — IBGE FTP listing primitives + catalog builder
#
# Functions:
#   - parse_ibge_directory_listing(html): extracts file rows (filename,
#     last_modified, size_bytes, is_dir) from an Apache index HTML page.
#   - parse_ibge_zip_name(filename): extracts (kind, year, period,
#     upstream_date) from a PNADC ZIP basename.
#   - parse_ibge_deflator_xls_name(filename): extracts year from a
#     `deflator_PNADC_YYYY.xls` basename.
#   - ibge_list_directory(url, timeout, retries): GET + parse with retry
#     and silent failure (returns NULL).
#   - fetch_ibge_ftp_catalog(state_path, timeout): top-level catalog
#     builder. Persists to state_path. Falls back to last good catalog
#     on network failure.
#
# Network calls use httr2; failures are silent (return NULL) so the DAG
# can continue with the sticky cache.
# ==============================================================================

# IBGE FTP root
.ibge_ftp_root <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua"

# ------------------------------------------------------------------------------
# HTML parser
# ------------------------------------------------------------------------------

#' Parse an Apache/Nginx HTML directory index.
#'
#' Returns a `data.table(filename, last_modified, size_bytes, is_dir)`.
#' `last_modified` is POSIXct (UTC, but assumed to be the IBGE server's
#' local time — this is fine for change-detection: only relative
#' comparison matters). `size_bytes` is NA for directories.
#'
#' Returns an empty data.table on parse failure (preserving the schema)
#' rather than NULL, so callers can rbind safely.
#'
#' @param html character(1), the page body
parse_ibge_directory_listing <- function(html) {
  empty <- data.table::data.table(
    filename = character(0),
    last_modified = as.POSIXct(character(0), tz = "UTC"),
    size_bytes = numeric(0),
    is_dir = logical(0)
  )
  if (!is.character(html) || length(html) != 1L || !nzchar(html)) return(empty)

  # IBGE FTP serves Latin-1; the relevant data (filenames, dates, sizes)
  # is ASCII but the surrounding chrome has accented chars. Convert to
  # UTF-8 (substituting invalid bytes) so regex works.
  html <- iconv(html, from = "latin1", to = "UTF-8", sub = "?")

  # Apache table-row format observed:
  #   <tr><td valign="top"><img src="/icons/X.gif" alt="[ALT]"></td>
  #   <td><a href="FILENAME">DISPLAY</a></td>
  #   <td align="right">YYYY-MM-DD HH:MM  </td>
  #   <td align="right">SIZE</td>
  #   <td>&nbsp;</td></tr>
  pat <- paste0(
    'alt="\\[([^\\]]*)\\]"></td>',
    '<td><a href="([^"]+)">[^<]*</a>',
    '\\s*</td>',
    '<td align="right">\\s*([0-9]{4}-[0-9]{2}-[0-9]{2}\\s+[0-9]{2}:[0-9]{2})?\\s*</td>',
    '<td align="right">\\s*([0-9.]+[KMG]?|-)?\\s*</td>'
  )
  m <- regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  if (!length(m)) return(empty)

  parsed <- regmatches(m, regexec(pat, m, perl = TRUE))
  alts  <- vapply(parsed, function(x) x[2L], character(1L))
  hrefs <- vapply(parsed, function(x) x[3L], character(1L))
  dates <- vapply(parsed, function(x) x[4L], character(1L))
  sizes <- vapply(parsed, function(x) x[5L], character(1L))

  # Drop parent dir
  keep <- alts != "PARENTDIR"
  if (!any(keep)) return(empty)
  alts  <- alts[keep]
  hrefs <- hrefs[keep]
  dates <- dates[keep]
  sizes <- sizes[keep]

  is_dir <- alts == "DIR"

  # Decode URL-encoded filenames; strip trailing "/" on directory hrefs
  hrefs <- utils::URLdecode(hrefs)
  hrefs <- sub("/$", "", hrefs)

  # Skip absolute hrefs (sometimes present for nav links)
  keep2 <- !grepl("^/", hrefs) & !grepl("^https?://", hrefs)
  if (!any(keep2)) return(empty)

  data.table::data.table(
    filename = hrefs[keep2],
    last_modified = .parse_apache_date(dates[keep2]),
    size_bytes = ifelse(is_dir[keep2], NA_real_,
                         .parse_apache_size(sizes[keep2])),
    is_dir = is_dir[keep2]
  )
}

# Parse Apache date "YYYY-MM-DD HH:MM" -> POSIXct
.parse_apache_date <- function(x) {
  out <- as.POSIXct(rep(NA_real_, length(x)), tz = "UTC")
  ok <- !is.na(x) & nzchar(x)
  if (any(ok)) {
    out[ok] <- as.POSIXct(trimws(x[ok]), format = "%Y-%m-%d %H:%M", tz = "UTC")
  }
  out
}

# Parse Apache human size "204M" / "108K" / "7.5K" / "293" -> numeric bytes
.parse_apache_size <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & x != "-" & nzchar(trimws(x))
  if (!any(ok)) return(out)
  s <- trimws(x[ok])
  unit <- toupper(sub("^[0-9.]+", "", s))
  num  <- as.numeric(sub("[KMG]$", "", s, ignore.case = TRUE))
  mult <- ifelse(unit == "K", 1024,
          ifelse(unit == "M", 1024^2,
          ifelse(unit == "G", 1024^3, 1)))
  out[ok] <- num * mult
  out
}

# ------------------------------------------------------------------------------
# Filename parsers
# ------------------------------------------------------------------------------

#' Parse a PNADC ZIP basename.
#'
#' Returns list(kind, year, period, upstream_date). `kind` is "quarterly"
#' or "annual". `period` is the quarter (1-4) for quarterly or the visit
#' (1-5) for annual. `upstream_date` is `YYYYMMDD` (string) when present
#' in the filename, NA otherwise.
parse_ibge_zip_name <- function(filename) {
  qre <- "^PNADC_([0-1][0-9])([0-9]{4})(?:_([0-9]{8}))?\\.zip$"
  vre <- "^PNADC_([0-9]{4})_visita([1-5])(?:_([0-9]{8}))?\\.zip$"

  if (grepl(qre, filename, ignore.case = TRUE)) {
    m <- regmatches(filename, regexec(qre, filename, ignore.case = TRUE))[[1]]
    return(list(
      kind = "quarterly",
      year = as.integer(m[3]),
      period = as.integer(m[2]),
      upstream_date = if (nchar(m[4])) m[4] else NA_character_
    ))
  }
  if (grepl(vre, filename, ignore.case = TRUE)) {
    m <- regmatches(filename, regexec(vre, filename, ignore.case = TRUE))[[1]]
    return(list(
      kind = "annual",
      year = as.integer(m[2]),
      period = as.integer(m[3]),
      upstream_date = if (nchar(m[4])) m[4] else NA_character_
    ))
  }
  NULL
}

#' Parse a deflator XLS basename `deflator_PNADC_YYYY.xls`.
#' Returns list(year, kind = "annual_deflator") or NULL if not matched.
parse_ibge_deflator_xls_name <- function(filename) {
  re <- "^deflator_PNADC_([0-9]{4})\\.xls$"
  if (!grepl(re, filename, ignore.case = TRUE)) return(NULL)
  m <- regmatches(filename, regexec(re, filename, ignore.case = TRUE))[[1]]
  list(kind = "annual_deflator", year = as.integer(m[2]))
}

# ------------------------------------------------------------------------------
# HTTP fetch with retry + silent failure
# ------------------------------------------------------------------------------

#' GET an IBGE FTP HTML directory page and parse it.
#'
#' Returns parsed data.table on success, NULL on persistent failure.
#' Uses httr2 with timeout and exponential backoff.
ibge_list_directory <- function(url, timeout = 30L, retries = 2L) {
  for (i in seq_len(retries + 1L)) {
    body <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_user_agent("PNADCperiods-pipeline-watcher") |>
        httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 400) |>
        httr2::req_perform()
      # IBGE FTP serves Latin-1 without explicit charset header
      httr2::resp_body_string(resp, encoding = "latin1")
    }, error = function(e) {
      message("ibge_list_directory failed (", url, "): ",
              conditionMessage(e))
      NULL
    })
    if (!is.null(body)) {
      parsed <- tryCatch(parse_ibge_directory_listing(body),
                         error = function(e) NULL)
      if (!is.null(parsed)) return(parsed)
    }
    if (i <= retries) Sys.sleep(2L^i)
  }
  NULL
}

# ------------------------------------------------------------------------------
# Catalog builder
# ------------------------------------------------------------------------------

#' Fetch the full IBGE FTP catalog: trimestral (data + deflator) + anual
#' (data per visit + deflator).
#'
#' Returns a structured list (see plan); NULL slots indicate listing
#' failure for that section. If the WHOLE thing fails, falls back to
#' the JSON at `state_path`. Persists current catalog to `state_path`
#' on any successful section so subsequent failures keep partial data.
#'
#' @param state_path path to a JSON cache file. Created if missing.
#' @param timeout per-request timeout in seconds.
fetch_ibge_ftp_catalog <- function(state_path = NULL, timeout = 30L) {
  fetched_ok <- list()
  out <- list(
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fetched_ok = list(),
    trimestral = list(dados = list(), deflator = NULL),
    anual = list(dados = list(), deflator = NULL)
  )

  # ---------------- Trimestral data ----------------
  # Top-level: list year folders
  trim_root <- file.path(.ibge_ftp_root, "Trimestral", "Microdados")
  trim_root_listing <- ibge_list_directory(trim_root, timeout = timeout)
  fetched_ok$trimestral_root <- !is.null(trim_root_listing)

  if (!is.null(trim_root_listing)) {
    year_dirs <- trim_root_listing[is_dir == TRUE & grepl("^[0-9]{4}$", filename)]
    for (i in seq_len(nrow(year_dirs))) {
      yr <- year_dirs$filename[i]
      year_url <- paste0(trim_root, "/", yr, "/")
      year_listing <- ibge_list_directory(year_url, timeout = timeout)
      fetched_ok[[paste0("trimestral_", yr)]] <- !is.null(year_listing)
      if (is.null(year_listing)) next

      zips <- year_listing[
        is_dir == FALSE & grepl("^PNADC_[0-9]{6}", filename, perl = TRUE)
      ]
      if (!nrow(zips)) next

      meta <- lapply(zips$filename, parse_ibge_zip_name)
      keep_idx <- which(!vapply(meta, is.null, logical(1L)))
      if (!length(keep_idx)) next

      enriched <- data.table::data.table(
        filename       = zips$filename[keep_idx],
        last_modified  = zips$last_modified[keep_idx],
        size_bytes     = zips$size_bytes[keep_idx],
        year           = vapply(meta[keep_idx], `[[`, integer(1L), "year"),
        quarter        = vapply(meta[keep_idx], `[[`, integer(1L), "period"),
        upstream_date  = vapply(meta[keep_idx], function(x)
                                  x$upstream_date %||% NA_character_,
                                character(1L))
      )
      out$trimestral$dados[[yr]] <- enriched
    }
  }

  # ---------------- Trimestral deflator ----------------
  trim_doc <- file.path(.ibge_ftp_root,
                        "Trimestral/Microdados/Documentacao/")
  trim_doc_listing <- ibge_list_directory(trim_doc, timeout = timeout)
  fetched_ok$trimestral_deflator <- !is.null(trim_doc_listing)
  if (!is.null(trim_doc_listing)) {
    defl <- trim_doc_listing[filename == "Deflatores.zip"]
    if (nrow(defl)) out$trimestral$deflator <- defl[, .(filename,
                                                          last_modified,
                                                          size_bytes)]
  }

  # ---------------- Anual data, per visit ----------------
  for (v in 1L:5L) {
    visit_url <- file.path(.ibge_ftp_root,
                           sprintf("Anual/Microdados/Visita/Visita_%d/Dados/", v))
    visit_listing <- ibge_list_directory(visit_url, timeout = timeout)
    fetched_ok[[paste0("anual_visita_", v)]] <- !is.null(visit_listing)
    if (is.null(visit_listing)) next

    zips <- visit_listing[
      is_dir == FALSE & grepl("^PNADC_[0-9]{4}_visita", filename, perl = TRUE)
    ]
    if (!nrow(zips)) next

    meta <- lapply(zips$filename, parse_ibge_zip_name)
    keep_idx <- which(!vapply(meta, is.null, logical(1L)))
    if (!length(keep_idx)) next

    enriched <- data.table::data.table(
      filename      = zips$filename[keep_idx],
      last_modified = zips$last_modified[keep_idx],
      size_bytes    = zips$size_bytes[keep_idx],
      year          = vapply(meta[keep_idx], `[[`, integer(1L), "year"),
      visit         = vapply(meta[keep_idx], `[[`, integer(1L), "period"),
      upstream_date = vapply(meta[keep_idx], function(x)
                                x$upstream_date %||% NA_character_,
                              character(1L))
    )
    out$anual$dados[[as.character(v)]] <- enriched
  }

  # ---------------- Anual deflator ----------------
  anual_doc <- file.path(.ibge_ftp_root,
                         "Anual/Microdados/Visita/Documentacao_Geral/")
  anual_doc_listing <- ibge_list_directory(anual_doc, timeout = timeout)
  fetched_ok$anual_deflator <- !is.null(anual_doc_listing)
  if (!is.null(anual_doc_listing)) {
    xlsx <- anual_doc_listing[
      is_dir == FALSE & grepl("^deflator_PNADC_[0-9]{4}\\.xls$",
                              filename, ignore.case = TRUE, perl = TRUE)
    ]
    if (nrow(xlsx)) {
      meta <- lapply(xlsx$filename, parse_ibge_deflator_xls_name)
      keep_idx <- which(!vapply(meta, is.null, logical(1L)))
      if (length(keep_idx)) {
        out$anual$deflator <- data.table::data.table(
          filename      = xlsx$filename[keep_idx],
          last_modified = xlsx$last_modified[keep_idx],
          size_bytes    = xlsx$size_bytes[keep_idx],
          year          = vapply(meta[keep_idx], `[[`, integer(1L), "year")
        )
      }
    }
  }

  out$fetched_ok <- fetched_ok

  # Persist if any section succeeded
  any_ok <- any(unlist(fetched_ok), na.rm = TRUE)
  if (!is.null(state_path) && any_ok) {
    .save_catalog_state(out, state_path)
  }

  # If nothing succeeded AND we have a saved catalog, fall back
  if (!any_ok && !is.null(state_path) && file.exists(state_path)) {
    cached <- tryCatch(.load_catalog_state(state_path), error = function(e) NULL)
    if (!is.null(cached)) {
      message("FTP catalog: all sections failed; using sticky cache from ",
              state_path)
      cached$fetched_ok <- fetched_ok    # record the failed-fetch attempt
      cached$fetched_at_cached <- TRUE
      return(cached)
    }
  }

  out
}

# ------------------------------------------------------------------------------
# Catalog persistence (JSON)
# ------------------------------------------------------------------------------

.save_catalog_state <- function(catalog, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Convert data.tables to plain lists for JSON
  serializable <- .catalog_to_serializable(catalog)
  jsonlite::write_json(serializable, path, auto_unbox = TRUE,
                       pretty = TRUE, na = "string", null = "null",
                       force = TRUE)
  invisible(path)
}

.load_catalog_state <- function(path) {
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)
  .catalog_from_serializable(raw)
}

.catalog_to_serializable <- function(catalog) {
  conv <- function(x) {
    if (data.table::is.data.table(x)) {
      x_copy <- data.table::copy(x)
      if ("last_modified" %in% names(x_copy)) {
        x_copy[, last_modified := format(last_modified,
                                          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
      }
      return(as.list(x_copy))
    }
    x
  }
  list(
    fetched_at = catalog$fetched_at,
    fetched_ok = catalog$fetched_ok,
    trimestral = list(
      dados = lapply(catalog$trimestral$dados, conv),
      deflator = conv(catalog$trimestral$deflator)
    ),
    anual = list(
      dados = lapply(catalog$anual$dados, conv),
      deflator = conv(catalog$anual$deflator)
    )
  )
}

.catalog_from_serializable <- function(raw) {
  rev <- function(x) {
    if (is.null(x) || !length(x)) return(NULL)
    dt <- data.table::as.data.table(x)
    if ("last_modified" %in% names(dt)) {
      dt[, last_modified := as.POSIXct(last_modified,
                                        format = "%Y-%m-%dT%H:%M:%SZ",
                                        tz = "UTC")]
    }
    dt[]
  }
  list(
    fetched_at = raw$fetched_at,
    fetched_ok = raw$fetched_ok,
    trimestral = list(
      dados = lapply(raw$trimestral$dados, rev),
      deflator = rev(raw$trimestral$deflator)
    ),
    anual = list(
      dados = lapply(raw$anual$dados, rev),
      deflator = rev(raw$anual$deflator)
    )
  )
}

# %||% helper (also defined in tar-deploy / tar-network — defensive)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
