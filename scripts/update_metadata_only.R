# ==============================================================================
# Quick Script: Update Only Series Metadata
# ==============================================================================
# This script regenerates just the series_metadata.rds file without refetching
# all SIDRA data. Use this after updating the metadata structure in the package.
# ==============================================================================

library(PNADCperiods)
library(data.table)

cat("=== Updating Series Metadata Only ===\n\n")

# Change to dashboard directory
setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")

# Generate new metadata from package
cat("Generating metadata from PNADCperiods package...\n")
metadata <- get_sidra_series_metadata()

# Show structure
cat("\nMetadata structure:\n")
cat("  - Columns:", paste(names(metadata), collapse = ", "), "\n")
cat("  - Number of series:", nrow(metadata), "\n")
cat("  - Themes:", paste(unique(metadata$theme), collapse = ", "), "\n")

# Save
saveRDS(metadata, "data/series_metadata.rds")
cat("\n  -> Saved to data/series_metadata.rds\n")

# Verify it has the new structure
cat("\nVerification:\n")
cat("  - Has 'theme' column:", "theme" %in% names(metadata), "\n")
cat("  - Has 'theme_category' column:", "theme_category" %in% names(metadata), "\n")
cat("  - Has 'subcategory' column:", "subcategory" %in% names(metadata), "\n")
cat("  - Has 'description_en' column:", "description_en" %in% names(metadata), "\n")

# Show example English descriptions
cat("\nSample English descriptions:\n")
sample_idx <- head(which(!is.na(metadata$description_en)), 5)
for (i in sample_idx) {
  cat("  - ", metadata$series_name[i], ": ", metadata$description_en[i], "\n")
}

cat("\n=== Done ===\n")
