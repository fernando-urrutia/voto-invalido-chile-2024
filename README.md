# voto-invalido-chile-2024
Analysis of invalid vote determinants in Chile's 2024 elections

# Determinants of Invalid Voting in Chile's 2024 Municipal and Regional Elections

## Overview

This project analyzes the factors that influence invalid voting (null and blank votes) across Chile's 344 municipalities in the 2024 elections — the first municipal and regional elections held under mandatory voting.

Four simultaneous elections are compared: Mayors, City Councilors, Regional Councilors (CORE), and Regional Governors.

## Research Question

How does the complexity of the electoral supply — number of candidates per seat and proportion of independent candidates outside coalitions — affect invalid voting at the municipal level, controlling for socioeconomic variables?

## Data Sources

- **Electoral data**: Servicio Electoral de Chile (SERVEL) — votes, candidates, and participation for 2024 and 2021 elections
- **Socioeconomic data**: Sistema Nacional de Información Municipal (SINIM) — poverty index, rural population
- **Education data**: Instituto Nacional de Estadísticas (INE) — average years of schooling per municipality
- **Population data**: INE Census 2024

## Methodology

- **Unit of analysis**: 344 municipalities (comunas)
- **Models**: OLS multiple regression with robust standard errors
  - HC3 robust standard errors for Mayors and City Councilors
  - CR2 cluster-robust standard errors by province for Regional Councilors (CORE)
  - Wild cluster bootstrap (Rademacher, 9,999 replications) for Regional Governors (16 clusters)
- **Diagnostics**: VIF for multicollinearity, Breusch-Pagan for heteroskedasticity, Shapiro-Wilk for residual normality, Moran's I for spatial autocorrelation
- **Robustness check**: Spatial Error Model (SEM) with queen contiguity weights

## Key Findings

- More candidates per seat significantly increases invalid voting, especially in City Councilor and CORE elections (R² adj. = 0.73 and 0.15 respectively)
- Independent candidates outside coalitions reduce invalid voting in mayoral elections (p < 0.01), suggesting they channel voter discontent into valid votes
- Higher electoral participation in 2021 (under voluntary voting) predicts lower invalid voting in 2024, indicating that mandatory voting converted abstention into invalid votes

## Tools

- **R** (tidyverse, sandwich, clubSandwich, lmtest, spdep, spatialreg, modelsummary, flextable)

## File Structure

```
├── tesus_2.R              # Full analysis script (data processing, models, diagnostics, tables)
└── README.md
```

## Author

Fernando Urrutia — Political Science, Universidad de Chile

## License

MIT License

