# Rearing Conditions and Behavioral Health Outcomes in Captive Rhesus Macaques

## Overview

This repository contains the data pipeline and statistical analyses for
a retrospective longitudinal study examining how early rearing
experiences shape behavioral health outcomes in captive rhesus macaques
(*Macaca mulatta*) at the Oregon National Primate Research Center
(ONPRC). The study uses multi-year records from the ONPRC's
institutional Electronic Health Record (EHR) system (LabKey/PRIMe),
spanning a cohort of approximately 5,200 rhesus macaques born between
2014 and 2022. Analyses use generalized linear mixed-effects models
(GLMERs); manuscript in preparation.

------------------------------------------------------------------------

## Research Background

Early rearing experience is a well-established determinant of behavioral
and psychological health in both humans and nonhuman primates. In
captive settings, the gold standard for early rearing is group housing:
ideally, animals spend at least the first three years of life in outdoor
group enclosures before potentially transitioning to indoor caged
housing for research purposes. Group-housed animals have access to
complex social environments with peers, juveniles, and adults of both
sexes, which supports normal social development and behavioral health.

However, some animals end up in indoor caged housing during early life
due to circumstantial factors such as health needs, research protocol
requirements, or colony management. Dam-rearing in caged housing during
infancy is associated with worse behavioral health outcomes than
dam-rearing in group enclosures. What is less well understood is whether
the social complexity of the cage environment - specifically, having
additional cagemates beyond the dam, such as peer infants, juveniles, or
non-dam adults - can mitigate some of the negative effects of early
cage-rearing. Additionally, less is known about the impact of time spent
in caged versus group housing during an animal's second and third year
of life.

This study tests three hypotheses. First, that greater social contact
during the first year of life, even within a caged setting, reduces the
likelihood of developing self-injurious behavior. Second, it examines
the impact of time spent in caged housing during the first three years
of life, respectively, on SIB development later in life. Finally, it
investigates whether the number of sedations experienced during infancy
contributes to SIB.

**Key outcome variable:** Development of self-injurious behavior (SIB),
specifically self-biting (binomial, yes/no)

**Key predictor variables:** Days in indoor caged housing by age window
(years 1–3), Days co-housed with various cagemate combinations (i.e. of
dam, peer infants, juveniles, and non-dam adults) during the first year
of life, Number of sedation events during year 1.

------------------------------------------------------------------------

## Repository Structure

```         
macaque-rearing-behavioral-health/
├── R/
│   ├── 01_parentage.R                   # Pull and derive rearing dam ID
│   ├── 02_cagemates_year1.R             # Pull and summarize cagemate history by type — year 1
│   ├── 03_housing.R                     # Pull and summarize housing type by age window
│   ├── 04_data_aggregation_and_plots.R  # Join all datasets; exploratory plots
│   └── 05_SIB_analysis.R               # Mixed-effects logistic regression models
├── data/                                # Data directory (not included — see below)
└── README.md
```

Scripts are numbered in the order they should be run. Each script reads
from and writes to the `data/` directory.

------------------------------------------------------------------------

## Data

**Raw data is not included in this repository.** All data originates
from ONPRC's proprietary institutional EHR system (LabKey/PRIMe) and
contains protected animal research records that cannot be shared
publicly.

To run these scripts, you would need: - Access to an institutional
LabKey installation with equivalent schema and query structure - The
following input files in a `data/` subdirectory: -
`rearing_conditions_subjects.csv` — subject list with demographics -
`room_categories.xlsx` — room-to-housing-type lookup table -
`parentage.xlsx` — output of Script 01, after manual review of edge
cases - `SIB_obs.xlsx` — self-injurious behavior observations -
`sedation_summary.csv` — sedation history summary

All scripts use `data/` as the working data directory. Institutional
server URLs have been replaced with `[ONPRC PRIMe LabKey URL]` and
`[ONPRC EHR folder path]` placeholders.

------------------------------------------------------------------------

## Pipeline Summary

| Script | Input | Output | Description |
|-----------------|-----------------|-----------------|---------------------|
| 01_parentage | Subject list, EHR | data/parentage.xlsx | Determines rearing dam for each subject |
| 02_cagemates_year1 | parentage.xlsx, EHR | data/days_by_combo_wide.csv | Days spent in each concurrent cagemate combination during year 1 |
| 03_housing | Subject list, EHR, room_categories.xlsx | data/housing_first_3_years.xlsx | Days in each housing type by age window |
| 04_data_aggregation_and_plots | All above outputs + SIB_obs.xlsx, sedation_summary.csv | data/full_data_wide.csv | Joined wide-format analytic dataset; exploratory plots |
| 05_SIB_analysis | data/full_data_wide.csv | — | GLMM models predicting SIB outcome |

------------------------------------------------------------------------

## Statistical Analysis

Script 05 (`05_SIB_analysis.R`) fits a series of generalized linear
mixed-effects models (GLMERs) using the `lme4` package with a binomial
family. The outcome variable is presence/absence of self-injurious
behavior (SIB). Three research questions are addressed:

**Q1: Effect of time in indoor caging on SIB** Models examine caging
time during years 1, 2, and 3 as predictors of SIB. Year 2 models
exclude animals with SIB onset in year 1; year 3 models additionally
exclude animals with SIB onset in year 2. Fixed effects include sex and
caging time (scaled per 10 days). Random effects include birth year and
maximum age at observation.

Key findings: Time in indoor caging during years 1, 2, and 3 each
independently predict increased SIB risk. Best year 1 model (sb6):
marginal R² = 0.22, conditional R² = 0.28 - Best year 3 model (sby3.5):
marginal R² = 0.21, conditional R² = 0.30.

**Q2: Effect of cagemate type on SIB (indoor-reared subjects only)**
Analysis restricted to subjects spending \> 180 days in indoor caging
during year 1 (n ≈ 849). Models test whether specific cagemate
configurations during year 1 predict SIB.

Key finding: Time co-housed with dam + adult female + infant
(dam_adult_female_infant) was the strongest protective cagemate
configuration (significant negative predictor, AIC 452).

**Q3: Effect of sedations during year 1 on SIB** Among indoor-reared
subjects, number of sedation events during year 1 is a significant
positive predictor of SIB (AIC 451, marginal R² = 0.10).

**Combined model:**

------------------------------------------------------------------------

## Methods Notes

### Cagemate type classification

Each cagemate is first classified into one of five types based on their
age relative to the subject and their relationship to the subject (dam
vs. non-dam):

-   **dam**: the rearing dam (biological, surrogate, or foster)
-   **infant**: cagemate born within 365 days of the subject
-   **juvie**: cagemate born 366–1095 days before or after the subject
-   **adult_female** / **adult_male**: cagemate aged \>3 years (non-dam)

### Concurrent cagemate combination approach

Rather than tracking individual cagemate types independently, Script 02
identifies the specific combination of cagemate types simultaneously
present during each segment of the subject's timeline. For example, a
subject co-housed with both their dam and a peer infant simultaneously
would accumulate days in the `dam_infant` combination, not separately in
`dam` and `infant`. The `classify_segment()` function assigns a combo
label to each timeline segment based on the full set of concurrently
active cagemate types. Total days in each combination are then summed
per subject and output in wide format (`days_by_combo_wide.csv`).

A diagnostic version (`inspect_segments()`) returns segment-level detail
for QC purposes and is exported to `data/segment_detail.csv`.

Time in "catch caging" (short-term individual housing in catch areas
without standard cage assignment) is attributed to dam-housing for
infants in the year 1 period.

### Caging variable scaling

Caging time variables are divided by 10 before model fitting (e.g.,
caging_year1.10) to improve numerical scaling and aid model convergence.
Coefficients represent the change in log-odds of SIB per 10 additional
days in caging.

------------------------------------------------------------------------

## Tools and Packages

-   **R** (≥ 4.2.0)
-   **tidyverse** — data manipulation and visualization
-   **lubridate** — date arithmetic
-   **lme4** — generalized linear mixed-effects models
-   **car** — variance inflation factor (VIF) diagnostics
-   **performance** — R-squared for mixed models
-   **Rlabkey** — LabKey EHR API queries
-   **readxl / openxlsx** — Excel file I/O
-   **janitor** — column name standardization

------------------------------------------------------------------------

## Author

Isabel Bernstein\
Research Associate, Behavioral Services Unit\
Oregon National Primate Research Center, OHSU\
[github.com/irbernstein](https://github.com/irbernstein)
