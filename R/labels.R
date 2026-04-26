# ==============================================================================
# Demographic Grouping & Labelling Helpers
# ==============================================================================
#
# Pure label functions on PNADC categorical codes. Used by builders that
# attach human-readable demographic groupings to microdata before
# aggregation. Cheap, no external dependencies beyond data.table::fcase.
#
# Split from utils_inequality.R (PR2) so that downstream targets that ONLY
# depend on labels (build_demographic_groupings → build_prepared_microdata)
# are not invalidated when inequality or poverty measures change.
# ==============================================================================

#' UF code to region mapping
#'
#' @param uf Numeric UF code
#' @return Character region name
uf_to_region <- function(uf) {
  uf <- as.numeric(uf)
  data.table::fcase(
    uf %in% c(11, 12, 13, 14, 15, 16, 17), "Norte",
    uf %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29), "Nordeste",
    uf %in% c(31, 32, 33, 35), "Sudeste",
    uf %in% c(41, 42, 43), "Sul",
    uf %in% c(50, 51, 52, 53), "Centro-Oeste",
    default = NA_character_
  )
}

#' UF code to region name (bilingual)
#'
#' @param uf Numeric UF code
#' @param lang Language code
#' @return Character region name
uf_to_region_i18n <- function(uf, lang = "pt") {
  region_pt <- uf_to_region(uf)
  if (lang == "en") {
    data.table::fcase(
      region_pt == "Norte", "North",
      region_pt == "Nordeste", "Northeast",
      region_pt == "Sudeste", "Southeast",
      region_pt == "Sul", "South",
      region_pt == "Centro-Oeste", "Center-West",
      default = NA_character_
    )
  } else {
    region_pt
  }
}


#' UF code to state abbreviation mapping
uf_to_abbrev <- function(uf) {
  uf <- as.numeric(uf)
  uf_map <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )
  uf_map[as.character(uf)]
}


#' Create age group variable
#'
#' @param age Numeric age vector
#' @return Character age group
age_group <- function(age) {
  data.table::fcase(
    age <= 17, "0-17",
    age <= 29, "18-29",
    age <= 59, "30-59",
    age >= 60, "60+",
    default = NA_character_
  )
}

#' Create education group variable
#'
#' @param vd3004 PNADC education level variable
#' @return Character education group
education_group <- function(vd3004) {
  vd3004 <- as.numeric(vd3004)
  data.table::fcase(
    vd3004 == 1, "Sem instrucao",
    vd3004 == 2, "Fundamental",
    vd3004 %in% c(3, 4), "Medio",
    vd3004 %in% c(5, 6, 7), "Superior",
    default = NA_character_
  )
}

#' Create sex label
#'
#' @param v2007 PNADC sex variable (1=male, 2=female)
#' @return Character sex label
sex_label <- function(v2007) {
  v2007 <- as.numeric(v2007)
  data.table::fcase(
    v2007 == 1, "Homens",
    v2007 == 2, "Mulheres",
    default = NA_character_
  )
}

#' Create race/color label
#'
#' @param v2010 PNADC race/color variable
#' @return Character race label
race_label <- function(v2010) {
  v2010 <- as.numeric(v2010)
  data.table::fcase(
    v2010 == 1, "Branca",
    v2010 == 2, "Preta",
    v2010 == 3, "Amarela",
    v2010 == 4, "Parda",
    v2010 == 5, "Indigena",
    default = NA_character_
  )
}

#' Create urban/rural label
#'
#' @param v1022 PNADC urban/rural variable (1=urban, 2=rural)
#' @return Character label
urban_rural_label <- function(v1022) {
  v1022 <- as.numeric(v1022)
  data.table::fcase(
    v1022 == 1, "Urbano",
    v1022 == 2, "Rural",
    default = NA_character_
  )
}


#' Get default group selection for breakdown filter
#'
#' Returns a sensible subset of groups to display by default when a
#' demographic breakdown is selected. For low-cardinality breakdowns
#' (<=6 groups), all groups are selected. For UF (27 states), the top 5
#' by population are selected. For race, the 3 main categories.
#'
#' @param breakdown Character breakdown type
#' @param available_groups Character vector of available group values
#' @return Character vector of default selected groups
get_default_groups <- function(breakdown, available_groups) {
  # Race: always default to 3 main groups (Amarela/Indigena have tiny samples)
  if (breakdown == "race") {
    main3 <- c("Branca", "Preta", "Parda")
    matched <- intersect(main3, available_groups)
    if (length(matched) >= 2) return(matched)
  }

  # UF: always default to top 5 states by population
  if (breakdown == "uf") {
    top5 <- c("SP", "RJ", "MG", "BA", "RS")
    matched <- intersect(top5, available_groups)
    if (length(matched) >= 3) return(matched)
  }

  # Low-cardinality breakdowns: select all
  if (length(available_groups) <= 6) return(available_groups)

  # Fallback for high-cardinality: first 5
  head(available_groups, 5)
}
