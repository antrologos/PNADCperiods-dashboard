# ==============================================================================
# Precompute Microdata Base for Inequality & Poverty Tabs
# ==============================================================================
#
# This script prepares the annual PNADC microdata for inequality and poverty
# analysis. It does the heavy lifting:
#
#   1. Builds mensalization crosswalk from all available quarterly data
#   2. Loads annual PNADC (auto-discovers available years) with proper visit selection
#   3. Applies crosswalk + calibrates monthly weights
#   4. Deflates incomes (IBGE CO2 + INPC to Dec 2025)
#   5. Constructs 8 per-capita income components
#   6. Creates demographic grouping variables
#   7. Saves prepared microdata as .fst cache
#
# Output:
#   ../data/processed/prepared_microdata.fst  (~150-200 MB)
#
# Runtime: ~15-30 minutes (first run); cached afterward.
#
# Author: Rogerio J. Barbosa
# ==============================================================================

rm(list = ls())
gc()
options(scipen = 999)

library(data.table)
library(fst)
library(readxl)
library(deflateBR)

# ==============================================================================
# Paths
# ==============================================================================

project_dir        <- "d:/Dropbox/Artigos/mensalizacao_pnad"
pkg_dir            <- file.path(project_dir, "PNADCperiods")
dashboard_dir      <- file.path(project_dir, "PNADCperiods-dashboard")
pnad_quarterly_dir <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados"
pnad_annual_dir    <- "D:/Dropbox/Bancos_Dados/PNADC/Anual/visitas"
data_output_dir    <- file.path(dashboard_dir, "data")

# Create output directory
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)

# Load PNADCperiods package
devtools::load_all(pkg_dir)

# Source utility functions
source(file.path(dashboard_dir, "R", "utils_inequality.R"))

# Cache file — stored OUTSIDE dashboard dir (not needed at runtime)
cache_dir <- file.path(project_dir, "data", "processed")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_file <- file.path(cache_dir, "prepared_microdata.fst")

# ==============================================================================
# Check cache
# ==============================================================================

if (file.exists(cache_file)) {
  cache_info <- file.info(cache_file)
  cache_age_days <- as.numeric(difftime(Sys.time(), cache_info$mtime, units = "days"))
  message(sprintf("Cache exists (%.1f days old): %s", cache_age_days, cache_file))
  message("To force rebuild, delete the cache file and re-run.")
  message("Skipping rebuild. Loading cached data...")
  d <- read_fst(cache_file, as.data.table = TRUE)
  message(sprintf("Loaded %s rows from cache.", format(nrow(d), big.mark = ",")))
  # Still print summary
  message(sprintf("Columns: %s", paste(names(d), collapse = ", ")))
  message(sprintf("Date range: %d to %d",
                  min(d$ref_month_yyyymm, na.rm = TRUE),
                  max(d$ref_month_yyyymm, na.rm = TRUE)))
  message("Done.")
  quit(save = "no")
}

# ==============================================================================
# STEP 1: Build Mensalization Crosswalk
# ==============================================================================

message("\n=== STEP 1: Building mensalization crosswalk from quarterly data ===\n")

# List ALL available quarterly files (2012-2025+) — full series for best determination rate (~97%)
quarterly_files <- list.files(
  path = pnad_quarterly_dir,
  pattern = "pnadc_20[0-9]{2}-[1-4]q\\.fst$",
  full.names = TRUE
)

message(sprintf("Found %d quarterly files.", length(quarterly_files)))

# Variables needed for mensalization
quarterly_vars <- c(
  "Ano", "Trimestre", "UPA", "V1008", "V1014",
  "V2008", "V20081", "V20082", "V2009",
  "V1028", "UF", "posest", "posest_sxi", "Estrato"
)

quarterly_data <- rbindlist(
  lapply(quarterly_files, function(f) {
    message(sprintf("  Loading: %s", basename(f)))
    tryCatch({
      read_fst(f, as.data.table = TRUE, columns = quarterly_vars)
    }, error = function(e) {
      message(sprintf("    Warning: Could not load %s: %s", basename(f), e$message))
      NULL
    })
  }),
  fill = TRUE
)
gc()

message(sprintf("Quarterly data: %s rows", format(nrow(quarterly_data), big.mark = ",")))

# Build the crosswalk
message("Running pnadc_identify_periods()...")
crosswalk <- pnadc_identify_periods(quarterly_data, verbose = TRUE)

det_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]
message(sprintf("Crosswalk: %s rows, determination rate: %.1f%%",
                format(nrow(crosswalk), big.mark = ","), 100 * det_rate))

rm(quarterly_data)
gc()

# ==============================================================================
# STEP 2: Load Annual PNADC Data (auto-discover available years)
# ==============================================================================

message("\n=== STEP 2: Loading annual PNADC data ===\n")

# Auto-discover available annual files
# Visit selection rule: Visit 5 for 2020-2021 (COVID), Visit 1 for all others
all_annual <- list.files(
  pnad_annual_dir,
  pattern = "pnadc_\\d{4}_visita[15]\\.fst$",
  full.names = TRUE
)
annual_files <- data.table(
  file = all_annual,
  basename = basename(all_annual)
)
annual_files[, ano := as.integer(
  sub("pnadc_(\\d{4})_visita\\d\\.fst", "\\1", basename)
)]
annual_files[, visita := as.integer(
  sub("pnadc_\\d{4}_visita(\\d)\\.fst", "\\1", basename)
)]
# Keep Visit 5 for 2020-2021, Visit 1 for all others
annual_files <- annual_files[
  (ano %in% c(2020, 2021) & visita == 5) |
  (!ano %in% c(2020, 2021) & visita == 1)
]
setorder(annual_files, ano)

message(sprintf("Found %d annual files (years %d-%d).",
                nrow(annual_files),
                min(annual_files$ano),
                max(annual_files$ano)))

if (nrow(annual_files) == 0) {
  stop("No annual PNADC files found. Check pnad_annual_dir path.")
}

# ---- Income variable definitions ----
# Pre-2015 variables (old format)
vars_before_2015q4 <- c("v500111", "v500211", "v500311", "v500411",
                         "v500511", "v500611", "v500711", "v500811",
                         "v500911", "v501011", "v501111", "v501211",
                         "v501311")

# Post-2015q4 variables (new format)
vars_after_2015q4 <- c("v5001a2", "v5002a2", "v5003a2", "v5005a2",
                        "v5008a2", "v5004a2", "v5006a2", "v5007a2")

# Base variables (all years)
annual_vars_base <- c(
  # Join keys
  "ano", "trimestre", "upa", "v1008", "v1014",
  # Demographics
  "v2005", "v2007", "v2009", "v2010", "vd3004", "v1022",
  "uf", "estrato",
  # Weights and calibration
  "v1032", "posest", "posest_sxi",
  # Total per capita income (IBGE pre-calculated)
  "vd5008",
  # Labor income
  "vd4019", "vd4020"
)

# Helper function: Sum with NA handling (like reference script)
# Returns NA only when ALL inputs are NA; otherwise sum(non-NA values)
Sum <- function(...) {
  X <- cbind(...)
  all_na <- rowSums(!is.na(X)) == 0
  result <- rowSums(X, na.rm = TRUE)
  result[all_na] <- NA
  result
}

# Load and harmonize income variables per year
annual_data <- rbindlist(
  lapply(seq_len(nrow(annual_files)), function(i) {
    row <- annual_files[i]
    f <- row$file
    ano_i <- row$ano
    message(sprintf("  Loading: %s", basename(f)))

    tryCatch({
      dt <- read_fst(f, as.data.table = TRUE)
      setnames(dt, tolower(names(dt)))

      # Determine which income vars to load based on year
      if (ano_i < 2015) {
        income_vars <- vars_before_2015q4
      } else if (ano_i == 2015) {
        income_vars <- c(vars_before_2015q4, vars_after_2015q4)
      } else {
        income_vars <- vars_after_2015q4
      }

      all_vars <- c(annual_vars_base, income_vars)
      cols_present <- intersect(all_vars, names(dt))
      dt <- dt[, ..cols_present]

      # ---- Harmonize income variables ----
      # Map pre-2015 variable names to standardized post-2015 names
      if (ano_i <= 2014) {
        # Direct mappings from old to new
        dt[, v5001a2 := fifelse(is.na(v500911), NA_real_, as.numeric(v500911))]  # BPC
        dt[, v5002a2 := fifelse(is.na(v501011), NA_real_, as.numeric(v501011))]  # Bolsa Familia
        dt[, v5003a2 := fifelse(is.na(v501111), NA_real_, as.numeric(v501111))]  # Other programs
        dt[, v5005a2 := fifelse(is.na(v500811), NA_real_, as.numeric(v500811))]  # Unemployment ins.
        dt[, v5008a2 := Sum(v500311, v500411, v501211, v501311)]                 # Grants
        dt[, v5004a2 := Sum(v500111, v500211)]                                   # Pensions
        dt[, v5006a2 := Sum(v500711, v500511)]                                   # Donations
        dt[, v5007a2 := fifelse(is.na(v500611), NA_real_, as.numeric(v500611))]  # Rental

        # Remove old variables
        old_vars_present <- intersect(vars_before_2015q4, names(dt))
        if (length(old_vars_present) > 0) dt[, (old_vars_present) := NULL]
      }

      if (ano_i == 2015) {
        # 2015 has BOTH old and new formats — combine (Sum handles NAs)
        dt[, v5001a2 := Sum(v5001a2, v500911)]   # BPC
        dt[, v5002a2 := Sum(v5002a2, v501011)]   # Bolsa Familia
        dt[, v5003a2 := Sum(v5003a2, v501111)]   # Other programs
        dt[, v5005a2 := Sum(v5005a2, v500811)]   # Unemployment ins.
        dt[, v5008a2 := Sum(v5008a2, v500311, v500411, v501211, v501311)]  # Grants
        dt[, v5004a2 := Sum(v5004a2, v500111, v500211)]                    # Pensions
        dt[, v5006a2 := Sum(v5006a2, v500711, v500511)]                    # Donations
        dt[, v5007a2 := Sum(v5007a2, v500611)]   # Rental

        # Remove old variables
        old_vars_present <- intersect(vars_before_2015q4, names(dt))
        if (length(old_vars_present) > 0) dt[, (old_vars_present) := NULL]
      }

      dt
    }, error = function(e) {
      message(sprintf("    Error: %s", e$message))
      NULL
    })
  }),
  fill = TRUE
)
gc()

message(sprintf("Annual data: %s rows", format(nrow(annual_data), big.mark = ",")))

# ==============================================================================
# STEP 2b: Standardize Column Names
# ==============================================================================

message("\n=== STEP 2b: Standardizing column names ===\n")

# pnadc_apply_periods() expects uppercase join keys
key_mappings <- c(
  "ano" = "Ano", "trimestre" = "Trimestre",
  "upa" = "UPA", "v1008" = "V1008", "v1014" = "V1014",
  "v1032" = "V1032", "uf" = "UF", "v2009" = "V2009"
)

for (old_name in names(key_mappings)) {
  if (old_name %in% names(annual_data)) {
    setnames(annual_data, old_name, key_mappings[[old_name]])
  }
}

# ==============================================================================
# STEP 3: Apply Crosswalk and Calibrate Weights
# ==============================================================================

message("\n=== STEP 3: Applying crosswalk + calibrating monthly weights ===\n")

d <- pnadc_apply_periods(
  annual_data,
  crosswalk,
  weight_var = "V1032",
  anchor = "year",
  calibrate = TRUE,
  calibration_unit = "month",
  smooth = TRUE,
  verbose = TRUE
)

match_rate <- mean(!is.na(d$ref_month_in_quarter))
message(sprintf("Merged data: %s rows, match rate: %.1f%%",
                format(nrow(d), big.mark = ","), 100 * match_rate))

rm(annual_data, crosswalk)
gc()

# ==============================================================================
# STEP 4: Filter Household Members
# ==============================================================================

message("\n=== STEP 4: Filtering household members ===\n")

n_before <- nrow(d)
d <- d[v2005 <= 14 | v2005 == 16]
message(sprintf("Filtered: %s -> %s rows (removed %s non-household members)",
                format(n_before, big.mark = ","),
                format(nrow(d), big.mark = ","),
                format(n_before - nrow(d), big.mark = ",")))

# ==============================================================================
# STEP 5: Apply Deflation (IBGE CO2 + INPC to Dec 2025)
# ==============================================================================

message("\n=== STEP 5: Deflating incomes to Dec 2025 BRL ===\n")

# Load IBGE deflator file
deflator_file <- file.path(pnad_annual_dir, "documentacao/deflator_pnadc_2024.xls")

if (!file.exists(deflator_file)) {
  stop("Deflator file not found: ", deflator_file)
}

deflator <- readxl::read_excel(deflator_file)
setDT(deflator)
deflator <- deflator[, .(Ano = ano, Trimestre = trim, UF = uf, CO2, CO2e, CO3)]

# Ensure matching types
deflator[, `:=`(Ano = as.numeric(Ano),
                Trimestre = as.numeric(Trimestre),
                UF = as.numeric(UF))]
d[, UF := as.numeric(UF)]

# Merge deflators
setkeyv(deflator, c("Ano", "Trimestre", "UF"))
setkeyv(d, c("Ano", "Trimestre", "UF"))
d <- deflator[d]

# INPC adjustment factor: CO2 reference → December 2025
# CO2 deflator brings values to a quarterly reference point;
# we need a final INPC step to align to Dec 2025
inpc_factor <- deflateBR::inpc(
  1,
  nominal_dates = as.Date("2024-07-01"),
  real_date = "12/2025"
)

message(sprintf("INPC adjustment factor (mid-2024 -> Dec 2025): %.4f", inpc_factor))

# Deflate total per capita income
d[, hhinc_pc_nominal := fifelse(is.na(vd5008), 0, as.numeric(vd5008))]
d[, hhinc_pc := hhinc_pc_nominal * CO2 * inpc_factor]

# Deflate labor income
d[, vd4019_num := fifelse(is.na(vd4019), 0, as.numeric(vd4019))]
d[, vd4020_num := fifelse(is.na(vd4020), 0, as.numeric(vd4020))]

# Deflate transfer income components
income_transfer_vars <- c("v5001a2", "v5002a2", "v5003a2", "v5004a2",
                           "v5005a2", "v5006a2", "v5007a2", "v5008a2")
for (var in income_transfer_vars) {
  if (var %in% names(d)) {
    new_var <- paste0(var, "_num")
    d[, (new_var) := fifelse(is.na(get(var)), 0, as.numeric(get(var)))]
  }
}

message("Income variables deflated.")

# ==============================================================================
# STEP 6: Construct Per-Capita Income Components
# ==============================================================================

message("\n=== STEP 6: Constructing per-capita income components ===\n")

# Household identifier
d[, id_dom := paste(UPA, V1008, V1014, sep = "_")]

# Count household members (for per-capita computation of components)
d[, n_members := .N, by = .(Ano, Trimestre, id_dom)]

# Individual-level income sources (deflated)
# These are individual amounts, need to be aggregated to household then divided per capita

# Labor income (individual level, already per person)
d[, renda_trab_ha := vd4019_num * CO2 * inpc_factor]
d[, renda_trab_ef := vd4020_num * CO2 * inpc_factor]

# Transfer/other income sources (individual level)
# BPC-LOAS
if ("v5001a2_num" %in% names(d)) {
  d[, renda_bpc := v5001a2_num * CO2e * inpc_factor]
} else {
  d[, renda_bpc := 0]
}

# Bolsa Familia / Auxilio Brasil
if ("v5002a2_num" %in% names(d)) {
  d[, renda_bf := v5002a2_num * CO2e * inpc_factor]
} else {
  d[, renda_bf := 0]
}

# Other social programs (includes Auxilio Emergencial 2020-2021)
if ("v5003a2_num" %in% names(d)) {
  d[, renda_outprogs := v5003a2_num * CO2e * inpc_factor]
} else {
  d[, renda_outprogs := 0]
}

# Retirement / pensions
if ("v5004a2_num" %in% names(d)) {
  d[, renda_previd := v5004a2_num * CO2e * inpc_factor]
} else {
  d[, renda_previd := 0]
}

# Unemployment insurance
if ("v5005a2_num" %in% names(d)) {
  d[, renda_segdesemp := v5005a2_num * CO2e * inpc_factor]
} else {
  d[, renda_segdesemp := 0]
}

# Donations
if ("v5006a2_num" %in% names(d)) {
  d[, renda_doacoes := v5006a2_num * CO2e * inpc_factor]
} else {
  d[, renda_doacoes := 0]
}

# Rental income
if ("v5007a2_num" %in% names(d)) {
  d[, renda_alugueis := v5007a2_num * CO2e * inpc_factor]
} else {
  d[, renda_alugueis := 0]
}

# Grants / scholarships
if ("v5008a2_num" %in% names(d)) {
  d[, renda_bolsas := v5008a2_num * CO2e * inpc_factor]
} else {
  d[, renda_bolsas := 0]
}

# Aggregate to household level and compute per capita
# Step 1: Sum individual incomes within household
hh_income <- d[, .(
  hh_trab_ha    = sum(renda_trab_ha, na.rm = TRUE),
  hh_trab_ef    = sum(renda_trab_ef, na.rm = TRUE),
  hh_previd     = sum(renda_previd, na.rm = TRUE),
  hh_bpc        = sum(renda_bpc, na.rm = TRUE),
  hh_bf         = sum(renda_bf, na.rm = TRUE),
  hh_outprogs   = sum(renda_outprogs, na.rm = TRUE),
  hh_segdesemp  = sum(renda_segdesemp, na.rm = TRUE),
  hh_alugueis   = sum(renda_alugueis, na.rm = TRUE),
  hh_outros     = sum(renda_doacoes + renda_bolsas, na.rm = TRUE),
  n_members     = .N
), by = .(Ano, Trimestre, id_dom)]

# Step 2: Compute per capita
hh_income[, `:=`(
  rendaTrab_ha_pc    = hh_trab_ha / n_members,
  rendaTrab_ef_pc    = hh_trab_ef / n_members,
  rendaPrevid_pc     = hh_previd / n_members,
  rendaBPC_pc        = hh_bpc / n_members,
  rendaBolsaFam_pc   = hh_bf / n_members,
  rendaOutProgs_pc   = hh_outprogs / n_members,
  rendaSegDesemp_pc  = hh_segdesemp / n_members,
  rendaAlugueis_pc   = hh_alugueis / n_members,
  rendaOutros_pc     = hh_outros / n_members
)]

# Step 3: Merge back to individual data
pc_cols <- c("rendaTrab_ha_pc", "rendaTrab_ef_pc", "rendaPrevid_pc",
             "rendaBPC_pc", "rendaBolsaFam_pc", "rendaOutProgs_pc",
             "rendaSegDesemp_pc", "rendaAlugueis_pc", "rendaOutros_pc")

d <- merge(
  d,
  hh_income[, c("Ano", "Trimestre", "id_dom", pc_cols), with = FALSE],
  by = c("Ano", "Trimestre", "id_dom"),
  all.x = TRUE
)

# Total per capita income from components (for decomposition validation)
d[, hhinc_pc_components := rendaTrab_ha_pc + rendaPrevid_pc + rendaBPC_pc +
    rendaBolsaFam_pc + rendaOutProgs_pc + rendaSegDesemp_pc +
    rendaAlugueis_pc + rendaOutros_pc]

message("Per-capita income components constructed.")

# Clean up intermediate variables
rm(hh_income)
gc()

# ==============================================================================
# STEP 7: Create Demographic Grouping Variables
# ==============================================================================

message("\n=== STEP 7: Creating demographic grouping variables ===\n")

# Sex
d[, sexo := sex_label(v2007)]

# Race/color
d[, raca := race_label(v2010)]

# Education level
if ("vd3004" %in% names(d)) {
  d[, faixa_educ := education_group(vd3004)]
} else {
  d[, faixa_educ := NA_character_]
  message("  Warning: vd3004 not found in data. Education breakdown unavailable.")
}

# Region
d[, regiao := uf_to_region(UF)]

# State abbreviation
d[, uf_abbrev := uf_to_abbrev(UF)]

# Urban/rural
if ("v1022" %in% names(d)) {
  d[, urbano := urban_rural_label(v1022)]
} else {
  d[, urbano := NA_character_]
  message("  Warning: v1022 not found in data. Urban/rural breakdown unavailable.")
}

# Age group
d[, faixa_idade := age_group(V2009)]

message("Demographic grouping variables created.")

# ==============================================================================
# STEP 8: Select Final Columns and Save
# ==============================================================================

message("\n=== STEP 8: Saving prepared microdata ===\n")

# Select only the columns needed for analysis
keep_cols <- c(
  # Period identifiers
  "Ano", "Trimestre", "UF", "ref_month_yyyymm", "ref_month_in_quarter",
  "ref_month_in_year", "weight_monthly",
  # Income (deflated to Dec 2025 BRL)
  "hhinc_pc",                        # Total per capita (from VD5008)
  "rendaTrab_ha_pc",                 # Labor (habitual)
  "rendaPrevid_pc",                  # Pensions
  "rendaBPC_pc",                     # BPC-LOAS
  "rendaBolsaFam_pc",               # Bolsa Familia
  "rendaOutProgs_pc",               # Other programs
  "rendaSegDesemp_pc",              # Unemployment insurance
  "rendaAlugueis_pc",               # Rental income
  "rendaOutros_pc",                 # Other income
  "hhinc_pc_components",            # Sum of components
  # Demographics
  "sexo", "raca", "faixa_educ", "regiao", "uf_abbrev",
  "urbano", "faixa_idade", "V2009"
)

# Keep only existing columns
keep_cols <- intersect(keep_cols, names(d))
d_final <- d[, ..keep_cols]

# Filter to determined observations only
n_total <- nrow(d_final)
d_final <- d_final[!is.na(ref_month_yyyymm)]
n_determined <- nrow(d_final)

message(sprintf("Determined observations: %s of %s (%.1f%%)",
                format(n_determined, big.mark = ","),
                format(n_total, big.mark = ","),
                100 * n_determined / n_total))

# Save
write_fst(d_final, cache_file)
message(sprintf("Saved: %s (%s rows, %.1f MB)",
                cache_file,
                format(nrow(d_final), big.mark = ","),
                file.size(cache_file) / 1e6))

# Summary
message("\n=== Summary ===")
message(sprintf("Date range: %d to %d",
                min(d_final$ref_month_yyyymm),
                max(d_final$ref_month_yyyymm)))
message(sprintf("Unique months: %d", uniqueN(d_final$ref_month_yyyymm)))
message(sprintf("Income columns: hhinc_pc + 8 components"))
message(sprintf("Demographic columns: sexo, raca, faixa_educ, regiao, uf_abbrev, urbano, faixa_idade"))
message("\nDone. Run precompute_inequality.R and precompute_poverty.R next.")
