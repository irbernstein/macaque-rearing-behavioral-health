# =============================================================================
# Rearing Conditions Study — Script 6: Data Aggregation and Exploratory Plots
# =============================================================================
# Purpose:  Joins all derived datasets (SIB outcomes, cagemate history, housing
#           history, pairing history, sedation history) into a single wide
#           analysis-ready dataset, then converts to long format for
#           exploratory visualization.
#
#           This script assumes all upstream scripts (01–05) have been run and
#           their outputs saved to the data/ directory.
#
# Inputs:   - data/SIB_obs.xlsx          (from Script 5, manually reviewed)
#           - data/rearing_conditions_subjects.csv
#           - data/cagemate_hx.csv       (from Script 2)
#           - data/housing_first_3_years.xlsx (from Script 3)
#           - data/sedation_summary.csv  (derived separately)
#           - data/pairing_summary.csv   (from Script 4)
#
# Outputs:  - data/full_data_wide.csv    (wide format, one row per subject)
#           - Exploratory ggplot2 visualizations (not saved by default)
# =============================================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(janitor)


# -----------------------------------------------------------------------------
# 1. Load all derived datasets
# -----------------------------------------------------------------------------

# SIB outcome data (manually reviewed from Script 5 output)
SIB <- read_excel("data/SIB_obs.xlsx") |>
  mutate(across(where(is.character), as.factor), AnimalID = as.character(AnimalID)) |>
  clean_names(case = "snake") |>
  rename(days_first_sib = age_first_sb_days)

# Subject demographic data
rrs_subjects <- read.csv("data/rearing_conditions_subjects.csv") |>
  clean_names() |>
  mutate(
    animal_id = as.character(animal_id),
    sex       = as.factor(gender),
    birth     = as.Date(birth, format = "%m/%d/%Y"),
    .before   = birth_year
  ) |>
  select(animal_id, sex, birth_year, birth, maximum_age)

# Cagemate history summary (from Script 2)
cagemates <- read.csv("data/cagemate_hx.csv", header = TRUE) |>
  mutate(across(where(is.character), as.numeric), animal_id = as.character(animal_id))

# Housing history summary (from Script 3)
housing <- read_excel("data/housing_first_3_years.xlsx") |>
  clean_names() |>
  mutate(animal_id = as.character(animal_id))

# Sedation history summary (derived separately — counts sedation events by age window)
sedations <- read.csv("data/sedation_summary.csv", header = TRUE) |>
  select(-X) |>
  mutate(animal_id = as.character(animal_id))

# Pairing history summary (from Script 4)
pairing <- read.csv("data/pairing_summary.csv", header = TRUE) |>
  mutate(animal_id = as.character(AnimalID), .before = 1) |>
  select(-AnimalID)


# -----------------------------------------------------------------------------
# 2. Join all datasets into wide format
# -----------------------------------------------------------------------------
# Uses subjects as the base table; all joins are left joins so all subjects
# are retained even if data is missing for some variables.

full_data_wide <- rrs_subjects |>
  left_join(SIB,       join_by(animal_id)) |>
  left_join(cagemates, join_by(animal_id)) |>
  left_join(housing,   join_by(animal_id)) |>
  left_join(sedations, join_by(animal_id)) |>
  left_join(pairing,   join_by(animal_id)) |>
  # Remove duplicate birth columns introduced by joins
  select(-matches("^birth\\.[xy]$"), -matches("^x1$"), -matches("^yob$"))

write.csv(full_data_wide, "data/full_data_wide.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 3. Convert to long format for visualization
# -----------------------------------------------------------------------------
# Pivots the cagemate/housing columns (which are suffixed by age window) into
# long format, with one row per subject per age window per cagemate type.

full_data_long <- full_data_wide |>
  pivot_longer(
    cols         = matches("year1a|year1b|year2|year3"),
    names_to     = c(".value", "year"),
    names_pattern = "^(.+)_(year\\d[ab]?)$"
  ) |>
  pivot_longer(
    cols      = c(dam, infant, juvie, adult_female, adult_male, none),
    names_to  = "cagemate_type",
    values_to = "cagemate_days"
  )


# -----------------------------------------------------------------------------
# 4. Exploratory plots
# -----------------------------------------------------------------------------
# These plots are for exploratory analysis only and are not publication-ready.
# Adjust filter thresholds (e.g., minimum days in caging) as needed.

# SIB type by days co-housed with each cagemate type — first six months
full_data_long |>
  filter(
    year   == "year1a",
    caging >= 182   # subjects must have been in caged housing for the full window
  ) |>
  ggplot(aes(sib_type, cagemate_days, fill = cagemate_type)) +
  geom_boxplot() +
  stat_summary(
    fun      = mean,
    geom     = "point",
    shape    = 18,
    size     = 4,
    color    = "gold",
    position = position_dodge(.75)
  ) +
  labs(
    title    = "Days co-housed with each cagemate type by SIB outcome (first 6 months)",
    x        = "SIB type",
    y        = "Days co-housed",
    fill     = "Cagemate type"
  ) +
  theme_bw()


# Time in caged housing by pairing incompatibility rate — by age window
full_data_long |>
  ggplot(aes(caging, y = (incompatibility_pairs / total_unique_pairs))) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~year) +
  labs(
    title = "Time in caged housing vs. pairing incompatibility rate",
    x     = "Days in caged housing",
    y     = "Proportion of pairs ending in incompatibility"
  ) +
  theme_bw()
