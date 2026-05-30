# Rearing Conditions and Behavioral Health Outcomes in Captive Rhesus Macaques

## Overview

This repository contains the data pipeline for a retrospective longitudinal study examining how early rearing experiences shape adult behavioral health outcomes in captive rhesus macaques (*Macaca mulatta*) at the Oregon National Primate Research Center (ONPRC). This study uses multi-year records from the ONPRC's institutional Electronic Health Record (EHR) system (LabKey/PRIMe), spanning a cohort of approximately 5,200 rhesus macaques born between 2014 and 2024. Statistical analyses are currently underway using generalized linear mixed-effects models (GLMERs); manuscript in preparation.

---

## Research Background

Early rearing experience is a well-established determinant of behavioral and psychological health in both humans and nonhuman primates. In captive settings, the gold standard for early rearing is group housing — ideally, animals spend at least the first three years of life in outdoor group enclosures before potentially transitioning to indoor caged housing for research purposes. Group-housed animals have access to complex social environments with peers, juveniles, and adults of both sexes, which supports normal social development and behavioral health.

However, some animals end up in indoor caged housing during early life due to circumstantial factors such as health needs, research protocol requirements, or colony management. We know that dam-rearing in caged housing is associated with worse behavioral health outcomes than dam-rearing in group enclosures. What is less well understood is whether the social complexity of the cage environment — specifically, having additional cagemates beyond the dam, such as peer infants, juveniles, or non-dam adults — can mitigate some of the negative effects of early cage-rearing.

This study tests the hypothesis that greater social contact during the first year of life, even within a caged setting, reduces the likelihood of developing self-injurious behavior and improves social housing outcomes in adulthood. It also compares outcomes across housing conditions (caged vs. group-housed) to contextualize findings within the broader literature.

Key outcome variables:
- **Self-injurious behavior (SIB)**: presence, age of onset, and severity score
- **Pairing incompatibility rate**: proportion of caged social housing attempts ending in incompatibility

Key predictor variables:
- Days co-housed with dam, peer infants, juveniles, and adults during the first and second six months of life (for animals reared in caging)
- Housing type (indoor caged vs. group housing) by age window
- Number of sedation events

---

## Repository Structure

```
rearing-conditions-study/
├── R/
│   ├── 01_parentage.R                   # Pull and derive rearing dam ID
│   ├── 02_cagemates.R                   # Pull and summarize cagemate history by type and age window
│   ├── 03_housing.R                     # Pull and summarize housing type by age window
│   ├── 04_pairing.R                     # Pull and summarize pairing history and incompatibility rates
│   ├── 05_SIB.R                         # Pull self-injurious behavior observations and SOAP notes
│   └── 06_data_aggregation_and_plots.R  # Join all datasets and produce exploratory plots
├── data/                                # Data directory (not included — see below)
└── README.md
```

Scripts are numbered in the order they should be run. Each script reads from and writes to the `data/` directory.

---

## Data

**Raw data is not included in this repository.** All data originates from ONPRC's proprietary institutional EHR system (LabKey/PRIMe) and contains protected animal research records that cannot be shared publicly.

To run these scripts, you would need:
- Access to an institutional LabKey installation with equivalent schema and query structure
- The following input files in a `data/` subdirectory:
  - `rearing_conditions_subjects.csv` — subject list with demographics
  - `room_categories.xlsx` — room-to-housing-type lookup table
  - `parentage.xlsx` — output of Script 01, after manual review of edge cases

All scripts use `data/` as the working data directory. Hardcoded institutional server URLs have been replaced with `[INSTITUTIONAL_EHR_URL]` placeholders.

---

## Pipeline Summary

| Script | Input | Output | Description |
|--------|-------|--------|-------------|
| 01_parentage | Subject list, EHR | data/parentage.csv | Determines rearing dam for each subject |
| 02_cagemates | parentage.xlsx, EHR | data/cagemate_hx.csv | Days with each cagemate type in first year |
| 03_housing | Subject list, EHR, room_categories.xlsx | data/housing_first_3_years.csv | Days in each housing type by age window |
| 04_pairing | Subject list, EHR | data/pairing_summary.csv | Total pairs and incompatibility rate |
| 05_SIB | Subject list, EHR | data/SIB_obs.csv, data/SOAPs.csv | SIB observations and clinical notes |
| 06_aggregation | All above outputs | data/full_data_wide.csv | Joined dataset + exploratory plots |

---

## Methods Notes

### Cagemate type classification
Cagemates are classified into five types based on their age relative to the subject and their relationship (dam vs. non-dam):
- **dam**: the rearing dam (biological, surrogate, or foster)
- **infant**: cagemate born within 365 days of the subject
- **juvie**: cagemate born 366–730 days before or after the subject
- **adult_female** / **adult_male**: cagemate aged >3 years (non-dam)

### Overlapping interval handling
The `merge_intervals()` function in Script 02 merges overlapping co-housing records before calculating total duration, preventing double-counting in cases where records were entered redundantly.

### Age windows
All time-based variables are calculated relative to each subject's birth date, using four standardized age windows:
- **year1a**: days 0–182 (first six months)
- **year1b**: days 183–365 (second six months)
- **year2**: days 366–730
- **year3**: days 731–1095

---

## Tools and Packages

- **R** (≥ 4.2.0)
- **tidyverse** — data manipulation and visualization
- **lubridate** — date arithmetic
- **Rlabkey** — LabKey EHR API queries
- **readxl / openxlsx** — Excel file I/O
- **janitor** — column name standardization

---

## Author

Isabel Bernstein  
Research Associate, Behavioral Services Unit  
Oregon National Primate Research Center, OHSU  
[github.com/irbernstein](https://github.com/irbernstein)
