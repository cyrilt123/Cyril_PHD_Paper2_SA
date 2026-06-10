# One Country, Different Clusters? Multimorbidity Patterns Across Rural South Africa (2012–2022)

This repository contains the R code used to clean, harmonise, analyse, and visualise verbal autopsy (VA) data from three rural South African Health and Demographic Surveillance System (HDSS) sites: **Agincourt**, **AHRI**, and **DIMAMO**.

The study examines whether **pooled multimorbidity clusters** are consistent with **site-specific clusters** across the three rural South African sites, using descriptive epidemiology, exact condition-combination analyses, and latent class analysis (LCA).

---

## Project overview

The main script:

- loads and harmonises VA datasets from multiple source files
- standardises variable names and classes across sites
- derives chronic condition indicators and demographic variables
- filters the analytic sample to **Agincourt, AHRI, and DIMAMO**
- produces descriptive tables and plots
- compares multimorbidity burden across sites
- examines exact multimorbidity combinations
- supports pooled and site-specific multimorbidity analyses

---

## Study objective

The primary aim of this analysis was to assess the extent to which **pooled latent multimorbidity class profiles** differed from **site-specific latent class profiles** among decedents in Agincourt, AHRI, and DIMAMO, rural South Africa, between **2012 and 2022**.

Secondary objectives included:

- comparing the prevalence of individual chronic conditions across sites
- comparing multimorbidity burden across sites
- examining exact condition-combination patterns
- describing pooled and site-specific latent classes by age group and sex

---

## Repository contents

- `SA_sites.R` — main R script for data cleaning, harmonisation, descriptive analysis, and plotting
- `README.md` — project description and usage instructions
  
---

## Data sources

This project uses harmonised **verbal autopsy (VA)** data from rural South African HDSS sites.

### Sites included in the final analytic sample
- Agincourt
- AHRI
- DIMAMO

### Time period
- 2012 to 2022

### Important note
The raw data are **not included in this repository** because they are surveillance data and may be subject to ethical, institutional, and data-access restrictions.

---

## Data requirements

The current script expects:

1. a folder containing the input `.csv` and `.dta` files
2. a `code_book.csv` file for variable selection
3. a `variable_names.csv` file for variable harmonisation


