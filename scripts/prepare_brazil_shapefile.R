# ==============================================================================
# Prepare Brazil State Shapefile for Dashboard
# ==============================================================================
#
# Downloads state boundaries from geobr and simplifies for web use
# Uses rmapshaper for aggressive simplification (much smaller than st_simplify)
#
# ==============================================================================

if (!requireNamespace("geobr", quietly = TRUE)) {
  install.packages("geobr")
}

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("rmapshaper", quietly = TRUE)) {
  install.packages("rmapshaper")
}

library(geobr)
library(sf)
library(rmapshaper)

# Set app directory
app_dir <- "d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard"

cat("Downloading Brazil state boundaries from IBGE via geobr...\n")

# Download state boundaries (already simplified version from geobr)
states <- geobr::read_state(year = 2020, simplified = TRUE)

cat("Downloaded", nrow(states), "states\n")

# Use rmapshaper for aggressive simplification - keeps 1% of vertices
# This produces much smaller files while preserving topology
states_simple <- rmapshaper::ms_simplify(states, keep = 0.01, keep_shapes = TRUE)

# Transform to WGS84 (required by leaflet)
states_simple <- sf::st_transform(states_simple, 4326)

cat("Simplified and transformed to WGS84\n")

# Keep only essential columns
states_simple <- states_simple[, c("code_state", "abbrev_state",
                                   "name_state", "geom")]

# Rename columns
names(states_simple) <- c("uf_code", "uf_abbrev", "uf_name", "geometry")

# Ensure uf_code is character for matching
states_simple$uf_code <- as.character(as.integer(states_simple$uf_code))

# Set the geometry column name explicitly
sf::st_geometry(states_simple) <- "geometry"

cat("\nState shapefile summary:\n")
print(sf::st_drop_geometry(states_simple))

# Save as RDS for R/leaflet use
rds_path <- file.path(app_dir, "data/brazil_states_sf.rds")
saveRDS(states_simple, rds_path)
cat("\nSaved RDS to:", rds_path, "\n")
cat("RDS file size:", format(file.size(rds_path), big.mark = ","), "bytes\n")

cat("\n=== Shapefile Preparation Complete ===\n")
