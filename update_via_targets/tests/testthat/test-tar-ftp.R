# Unit tests for tar-ftp.R parsers (HTML, ZIP names, deflator XLS).
#
# Uses captured HTML fixtures in tests/testthat/fixtures/ibge_*.html.
# Network calls (ibge_list_directory, fetch_ibge_ftp_catalog) are tested
# separately with mocked httr2 responses.

library(data.table)
source(testthat::test_path("..", "..", "R", "tar-ftp.R"), chdir = FALSE)

fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

# -----------------------------------------------------------------------------
# parse_ibge_directory_listing
# -----------------------------------------------------------------------------

test_that("parse_ibge_directory_listing: trimestral year listing", {
  html <- paste(readLines(fixture_path("ibge_trimestral_2024.html"),
                          warn = FALSE), collapse = "\n")
  dt <- parse_ibge_directory_listing(html)

  expect_s3_class(dt, "data.table")
  expect_named(dt, c("filename", "last_modified", "size_bytes", "is_dir"))
  expect_gt(nrow(dt), 0L)

  # All entries here are ZIP files (no subdirs)
  expect_true(all(!dt$is_dir))

  # We expect 4 quarters of 2024
  expected_quarters <- c("PNADC_012024_20250815.zip",
                         "PNADC_022024_20260324.zip",
                         "PNADC_032024_20250815.zip",
                         "PNADC_042024_20250815.zip")
  expect_true(all(expected_quarters %in% dt$filename))

  # Sizes parsed (M = mega): 204M ≈ 204 * 1024^2
  q1 <- dt[filename == "PNADC_012024_20250815.zip"]
  expect_equal(q1$size_bytes, 204 * 1024^2)
  expect_equal(format(q1$last_modified, "%Y-%m-%d %H:%M"), "2025-08-15 10:23")
})

test_that("parse_ibge_directory_listing: trimestral root has folders", {
  html <- paste(readLines(fixture_path("ibge_trimestral_root.html"),
                          warn = FALSE), collapse = "\n")
  dt <- parse_ibge_directory_listing(html)

  # Folders only (year subdirs)
  year_folders <- dt[is_dir == TRUE & grepl("^[0-9]{4}$", filename)]
  expect_gte(nrow(year_folders), 13L)  # 2012..2025+
  expect_true("2024" %in% year_folders$filename)
  expect_true("2025" %in% year_folders$filename)
  expect_true(all(is.na(year_folders$size_bytes)))

  # Documentacao should also be a dir
  doc <- dt[filename == "Documentacao"]
  expect_equal(nrow(doc), 1L)
  expect_true(doc$is_dir)
})

test_that("parse_ibge_directory_listing: trimestral doc lists Deflatores.zip", {
  html <- paste(readLines(fixture_path("ibge_trimestral_doc.html"),
                          warn = FALSE), collapse = "\n")
  dt <- parse_ibge_directory_listing(html)
  defl <- dt[filename == "Deflatores.zip"]
  expect_equal(nrow(defl), 1L)
  expect_false(defl$is_dir)
  expect_true(!is.na(defl$last_modified))
  expect_gt(defl$size_bytes, 1000)
})

test_that("parse_ibge_directory_listing: anual visita_1 lists ZIPs", {
  html <- paste(readLines(fixture_path("ibge_anual_visita1.html"),
                          warn = FALSE), collapse = "\n")
  dt <- parse_ibge_directory_listing(html)
  zips <- dt[grepl("^PNADC_\\d{4}_visita1", filename)]
  expect_gte(nrow(zips), 5L)
  expect_true(all(grepl("\\.zip$", zips$filename)))
})

test_that("parse_ibge_directory_listing: anual doc lists deflator XLS files", {
  html <- paste(readLines(fixture_path("ibge_anual_doc.html"),
                          warn = FALSE), collapse = "\n")
  dt <- parse_ibge_directory_listing(html)
  xls <- dt[grepl("^deflator_PNADC_\\d{4}\\.xls$", filename)]
  expect_gte(nrow(xls), 5L)  # at least 5 years' deflators
  expect_true("deflator_PNADC_2024.xls" %in% xls$filename)
})

test_that("parse_ibge_directory_listing: empty/malformed input => empty data.table", {
  expect_equal(nrow(parse_ibge_directory_listing("")), 0L)
  expect_equal(nrow(parse_ibge_directory_listing(character(0))), 0L)
  expect_equal(nrow(parse_ibge_directory_listing(NA_character_)), 0L)
  expect_equal(nrow(parse_ibge_directory_listing("<html>no rows</html>")), 0L)
})

test_that("parse_ibge_directory_listing parses K/M/G size suffixes and dash", {
  mk_row <- function(name, size) sprintf(
    paste0('<tr><td valign="top"><img src="/icons/x.gif" alt="[ZIP]"></td>',
           '<td><a href="%s">%s</a></td>',
           '<td align="right">2024-01-01 00:00  </td>',
           '<td align="right">%s</td><td>&nbsp;</td></tr>'),
    name, name, size)
  html <- paste0(mk_row("a.zip", "204M"),
                 mk_row("b.zip", "108K"),
                 mk_row("c.zip", "7.5K"),
                 mk_row("d.zip", "-"))
  res <- parse_ibge_directory_listing(html)
  expect_equal(res$size_bytes,
               c(204 * 1024^2, 108 * 1024, 7.5 * 1024, NA_real_))
})

# -----------------------------------------------------------------------------
# parse_ibge_zip_name
# -----------------------------------------------------------------------------

test_that("parse_ibge_zip_name: quarterly with date suffix", {
  res <- parse_ibge_zip_name("PNADC_012024_20250815.zip")
  expect_equal(res$kind, "quarterly")
  expect_equal(res$year, 2024L)
  expect_equal(res$period, 1L)
  expect_equal(res$upstream_date, "20250815")
})

test_that("parse_ibge_zip_name: quarterly without date suffix", {
  res <- parse_ibge_zip_name("PNADC_032025.zip")
  expect_equal(res$kind, "quarterly")
  expect_equal(res$year, 2025L)
  expect_equal(res$period, 3L)
  expect_true(is.na(res$upstream_date))
})

test_that("parse_ibge_zip_name: annual visit (with and without date)", {
  res1 <- parse_ibge_zip_name("PNADC_2020_visita5_20250822.zip")
  expect_equal(res1$kind, "annual")
  expect_equal(res1$year, 2020L)
  expect_equal(res1$period, 5L)
  expect_equal(res1$upstream_date, "20250822")

  res2 <- parse_ibge_zip_name("PNADC_2024_visita5.zip")
  expect_equal(res2$kind, "annual")
  expect_equal(res2$year, 2024L)
  expect_equal(res2$period, 5L)
  expect_true(is.na(res2$upstream_date))
})

test_that("parse_ibge_zip_name: non-PNADC filename returns NULL", {
  expect_null(parse_ibge_zip_name("LEIA-ME.pdf"))
  expect_null(parse_ibge_zip_name("Deflatores.zip"))
  expect_null(parse_ibge_zip_name("garbage"))
})

# -----------------------------------------------------------------------------
# parse_ibge_deflator_xls_name
# -----------------------------------------------------------------------------

test_that("parse_ibge_deflator_xls_name: extracts year", {
  res <- parse_ibge_deflator_xls_name("deflator_PNADC_2025.xls")
  expect_equal(res$kind, "annual_deflator")
  expect_equal(res$year, 2025L)
})

test_that("parse_ibge_deflator_xls_name: case-insensitive", {
  res <- parse_ibge_deflator_xls_name("deflator_pnadc_2024.xls")
  expect_equal(res$year, 2024L)
})

test_that("parse_ibge_deflator_xls_name: rejects non-deflator", {
  expect_null(parse_ibge_deflator_xls_name("Variaveis_PNADC_Anual_Visita.xls"))
  expect_null(parse_ibge_deflator_xls_name("PNADC_2024_visita1.zip"))
})

# -----------------------------------------------------------------------------
# Catalog round-trip (serialization)
# -----------------------------------------------------------------------------

test_that("catalog serialize -> deserialize round-trips", {
  catalog <- list(
    fetched_at = "2026-04-26T18:00:00Z",
    fetched_ok = list(trimestral_root = TRUE, anual_visita_1 = TRUE),
    trimestral = list(
      dados = list(
        "2024" = data.table(
          filename = "PNADC_012024_20250815.zip",
          last_modified = as.POSIXct("2025-08-15 10:23", tz = "UTC"),
          size_bytes = 204 * 1024^2,
          year = 2024L, quarter = 1L,
          upstream_date = "20250815"
        )
      ),
      deflator = data.table(
        filename = "Deflatores.zip",
        last_modified = as.POSIXct("2026-02-26 09:00", tz = "UTC"),
        size_bytes = 114 * 1024
      )
    ),
    anual = list(
      dados = list(
        "1" = data.table(
          filename = "PNADC_2024_visita1_20251119.zip",
          last_modified = as.POSIXct("2025-11-19 10:34", tz = "UTC"),
          size_bytes = 169 * 1024^2,
          year = 2024L, visit = 1L,
          upstream_date = "20251119"
        )
      ),
      deflator = data.table(
        filename = "deflator_PNADC_2025.xls",
        last_modified = as.POSIXct("2026-04-24 10:00", tz = "UTC"),
        size_bytes = 174 * 1024,
        year = 2025L
      )
    )
  )

  tmp <- tempfile(fileext = ".json")
  .save_catalog_state(catalog, tmp)
  expect_true(file.exists(tmp))

  rt <- .load_catalog_state(tmp)
  expect_equal(rt$fetched_at, catalog$fetched_at)
  expect_equal(rt$trimestral$dados$`2024`$filename,
               catalog$trimestral$dados$`2024`$filename)
  expect_equal(rt$trimestral$dados$`2024`$year, 2024L)
  expect_equal(rt$anual$deflator$year, 2025L)

  unlink(tmp)
})
