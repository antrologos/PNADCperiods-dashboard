# PNADCperiods Dashboard

Interactive Shiny dashboard for exploring mensalized Brazilian labor market series from the PNADC survey.

## Features

- **Series Explorer**: Interactive visualization of 86 mensalized SIDRA series
- **Geographic Analysis**: Labor market indicators by state (UF) with interactive bar chart
- **Inequality Analysis**: Gini, Lorenz curves, Growth Incidence Curves (coming soon)
- **Poverty Analysis**: FGT indices with World Bank poverty lines (coming soon)

## Quick Start

### 1. Install dependencies

```r
install.packages(c("shiny", "bslib", "data.table", "ggplot2", "plotly", "DT", "bsicons", "shinyjs"))

# Install PNADCperiods from GitHub
remotes::install_github("antrologos/PNADCperiods")
```

### 2. Precompute data

```r
setwd("path/to/PNADCperiods-dashboard")

# Series Explorer data (national mensalized series)
source("scripts/precompute_sidra.R")

# Geographic data (state-level indicators)
source("scripts/precompute_geographic.R")

# Or for testing without SIDRA API:
source("scripts/generate_sample_geographic_data.R")
```

### 3. Run the app

```r
shiny::runApp()
```

## Data Sources

- **SIDRA API**: Official IBGE labor market statistics
- **PNADC Microdata**: Quarterly and annual survey data (for geographic/inequality/poverty tabs)

## Authors

- **Rogerio Barbosa** (R package development)
- **Marcos Hecksher** (Mensalization methodology)

## License

MIT

## Links

- [PNADCperiods Package](https://github.com/antrologos/PNADCperiods)
- [Documentation](https://antrologos.github.io/PNADCperiods/)
- [IBGE SIDRA](https://sidra.ibge.gov.br/)
