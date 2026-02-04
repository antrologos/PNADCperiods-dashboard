# ==============================================================================
# Generate Sample Geographic Data for Testing
# ==============================================================================
#
# This script generates realistic sample data for testing the Geographic tab
# when SIDRA API is unavailable.
#
# Output: data/geographic_data.rds
#
# ==============================================================================

library(data.table)

cat("Generating sample geographic data for testing...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# UF codes (all 27 Brazilian states + DF)
uf_codes <- c(11L, 12L, 13L, 14L, 15L, 16L, 17L,
              21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
              31L, 32L, 33L, 35L,
              41L, 42L, 43L,
              50L, 51L, 52L, 53L)

# Generate periods (rolling quarters from 2019 to 2024)
# Format: YYYYMM (final month of rolling quarter)
periods <- c()
for (year in 2019:2024) {
  for (month in 1:12) {
    periods <- c(periods, as.integer(paste0(year, sprintf("%02d", month))))
  }
}
# Trim to reasonable range
periods <- periods[periods >= 201903 & periods <= 202412]

# Indicators
indicators <- c("taxadesocup", "taxapartic", "nivelocup")

# Regional base rates (realistic for Brazil)
regional_baselines <- list(
  # North region (11-17)
  `11` = list(taxadesocup = 8.5, taxapartic = 60.0, nivelocup = 55.0),
  `12` = list(taxadesocup = 13.0, taxapartic = 58.0, nivelocup = 50.0),
  `13` = list(taxadesocup = 12.5, taxapartic = 57.0, nivelocup = 50.0),
  `14` = list(taxadesocup = 11.0, taxapartic = 59.0, nivelocup = 52.0),
  `15` = list(taxadesocup = 10.5, taxapartic = 58.0, nivelocup = 52.0),
  `16` = list(taxadesocup = 14.0, taxapartic = 56.0, nivelocup = 48.0),
  `17` = list(taxadesocup = 9.5, taxapartic = 62.0, nivelocup = 56.0),
  # Northeast region (21-29)
  `21` = list(taxadesocup = 14.5, taxapartic = 52.0, nivelocup = 44.0),
  `22` = list(taxadesocup = 12.0, taxapartic = 54.0, nivelocup = 47.0),
  `23` = list(taxadesocup = 11.5, taxapartic = 55.0, nivelocup = 48.0),
  `24` = list(taxadesocup = 13.0, taxapartic = 53.0, nivelocup = 46.0),
  `25` = list(taxadesocup = 12.5, taxapartic = 52.0, nivelocup = 45.0),
  `26` = list(taxadesocup = 15.0, taxapartic = 54.0, nivelocup = 46.0),
  `27` = list(taxadesocup = 14.0, taxapartic = 50.0, nivelocup = 43.0),
  `28` = list(taxadesocup = 15.5, taxapartic = 51.0, nivelocup = 43.0),
  `29` = list(taxadesocup = 16.5, taxapartic = 52.0, nivelocup = 43.0),
  # Southeast region (31-35)
  `31` = list(taxadesocup = 10.0, taxapartic = 62.0, nivelocup = 56.0),
  `32` = list(taxadesocup = 11.0, taxapartic = 61.0, nivelocup = 54.0),
  `33` = list(taxadesocup = 13.5, taxapartic = 60.0, nivelocup = 52.0),
  `35` = list(taxadesocup = 11.5, taxapartic = 64.0, nivelocup = 56.0),
  # South region (41-43)
  `41` = list(taxadesocup = 6.5, taxapartic = 66.0, nivelocup = 62.0),
  `42` = list(taxadesocup = 4.5, taxapartic = 68.0, nivelocup = 65.0),
  `43` = list(taxadesocup = 6.0, taxapartic = 65.0, nivelocup = 61.0),
  # Central-West region (50-53)
  `50` = list(taxadesocup = 7.5, taxapartic = 63.0, nivelocup = 58.0),
  `51` = list(taxadesocup = 8.0, taxapartic = 64.0, nivelocup = 59.0),
  `52` = list(taxadesocup = 9.0, taxapartic = 62.0, nivelocup = 56.0),
  `53` = list(taxadesocup = 10.5, taxapartic = 63.0, nivelocup = 56.0)
)

# Generate data
all_data <- list()

set.seed(42)  # For reproducibility

for (indicator in indicators) {
  for (uf in uf_codes) {
    baseline <- regional_baselines[[as.character(uf)]][[indicator]]

    for (period in periods) {
      # Add time-varying component (COVID shock in 2020, recovery after)
      year <- as.integer(substr(period, 1, 4))
      month <- as.integer(substr(period, 5, 6))

      # COVID effect (peak in Q2-Q3 2020)
      covid_effect <- 0
      if (year == 2020 && month >= 4 && month <= 9) {
        if (indicator == "taxadesocup") {
          covid_effect <- 3.0 + runif(1, -0.5, 0.5)
        } else {
          covid_effect <- -3.0 + runif(1, -0.5, 0.5)
        }
      } else if (year == 2020 && month >= 10) {
        if (indicator == "taxadesocup") {
          covid_effect <- 2.0 + runif(1, -0.5, 0.5)
        } else {
          covid_effect <- -2.0 + runif(1, -0.5, 0.5)
        }
      } else if (year == 2021) {
        if (indicator == "taxadesocup") {
          covid_effect <- 1.5 - (month / 12) * 1.0
        } else {
          covid_effect <- -1.5 + (month / 12) * 1.0
        }
      }

      # Seasonal variation (smaller for rates)
      seasonal <- sin(2 * pi * month / 12) * 0.3

      # Random noise
      noise <- rnorm(1, 0, 0.2)

      # Final value
      value <- baseline + covid_effect + seasonal + noise

      # Ensure reasonable bounds
      if (indicator == "taxadesocup") {
        value <- max(2, min(25, value))
      } else {
        value <- max(40, min(75, value))
      }

      all_data[[length(all_data) + 1]] <- data.table(
        uf_code = uf,
        anomesfinaltrimmovel = period,
        value = round(value, 1),
        indicator = indicator
      )
    }
  }
}

geographic_data <- rbindlist(all_data)

# Sort
setorder(geographic_data, indicator, anomesfinaltrimmovel, uf_code)

# Summary
cat("\n=== Sample Data Summary ===\n")
cat("Total observations:", nrow(geographic_data), "\n")
cat("Indicators:", length(unique(geographic_data$indicator)), "\n")
cat("States (UFs):", length(unique(geographic_data$uf_code)), "\n")
cat("Period range:", min(geographic_data$anomesfinaltrimmovel), "to",
    max(geographic_data$anomesfinaltrimmovel), "\n")

# Show sample
cat("\nSample values for unemployment rate (taxadesocup) in Dec 2024:\n")
sample <- geographic_data[indicator == "taxadesocup" &
                           anomesfinaltrimmovel == 202412][order(-value)]
print(head(sample, 10))

# Save
saveRDS(geographic_data, "data/geographic_data.rds")
cat("\nSaved to: data/geographic_data.rds\n")
cat("File size:", format(file.size("data/geographic_data.rds"),
                         big.mark = ","), "bytes\n")

cat("\n=== Sample Data Generation Complete ===\n")
