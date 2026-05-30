# =============================================================================
# Rearing Conditions Study — Script 3: Housing History
# =============================================================================
# Purpose:  Pulls housing location records from the institutional EHR and
#           calculates the total days each subject spent in each housing type
#           during the first three years of life, broken into four age windows:
#             year1a: days 0–182 (first six months)
#             year1b: days 183–365 (second six months)
#             year2:  days 366–730 (second year)
#             year3:  days 731–1095 (third year)
#
#           Housing types are assigned from a manually maintained lookup table
#           (room_categories.xlsx) that categorizes each room code into a
#           housing type (e.g., caging, group).
#
# Inputs:   - data/rearing_conditions_subjects.csv
#           - data/room_categories.xlsx (room-to-housing-type lookup)
#           - LabKey EHR query: housing
#
# Outputs:  - data/housing_first_3_years.csv
# =============================================================================

library(Rlabkey)
library(tidyverse)
library(lubridate)
library(readxl)


# -----------------------------------------------------------------------------
# 1. Load subject list
# -----------------------------------------------------------------------------

rrs_subjects <- read.csv("data/rearing_conditions_subjects.csv", header = TRUE) |>
  mutate(
    animal_id = as.character(AnimalID),
    sex       = as.factor(Gender),
    Birth     = as.Date(Birth, format = "%m/%d/%Y"),
    .before   = Species
  ) |>
  select(-Gender)


# -----------------------------------------------------------------------------
# 2. Pull housing history from EHR
# -----------------------------------------------------------------------------

housing_data_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "housing",
  colSelect    = c("Id", "date", "enddate", "room", "cage"),
  colFilter    = makeFilter(
    c("Id", "IN", paste(rrs_subjects$AnimalID, collapse = ";"))
  ),
  colNameOpt = "rname"
)

housing_data <- housing_data_raw |>
  rename(animal_id = id)


# -----------------------------------------------------------------------------
# 3. Load room category lookup table
# -----------------------------------------------------------------------------
# This table maps each room code to a housing type category (e.g., "indoor",
# "outdoor", "nursery"). Maintained manually and updated as rooms change.

room_categories <- read_excel("data/room_categories.xlsx") |>
  mutate(across(where(is.character), as.factor))


# -----------------------------------------------------------------------------
# 4. Calculate time in each housing type per age window
# -----------------------------------------------------------------------------
# This function processes all subjects at once. For each housing record, it
# calculates the overlap with each of the four age windows, then summarizes
# total days by housing type and age window.

process_all_housing <- function(subject_list, birth_data, housing_data) {

  birth_dates <- data.frame(subject_id = subject_list, birth = birth_data)

  housing_data <- housing_data |>
    left_join(birth_dates, by = c("animal_id" = "subject_id")) |>
    mutate(
      in_date  = as.Date(date),
      out_date = if_else(is.na(enddate), Sys.Date(), as.Date(enddate)),
      birth    = as.Date(birth),

      # Define age window boundaries
      year1a_start = birth,
      year1a_end   = birth + 182,
      year1b_start = birth + 182,
      year1b_end   = birth + 365,
      year2_start  = birth + 365,
      year2_end    = birth + 730,
      year3_start  = birth + 730,
      year3_end    = birth + 1095,

      # Calculate overlap with each age window
      days_year1a = if_else(
        pmax(in_date, year1a_start) <= pmin(out_date, year1a_end),
        as.numeric(pmin(out_date, year1a_end) - pmax(in_date, year1a_start)), 0),
      days_year1b = if_else(
        pmax(in_date, year1b_start) <= pmin(out_date, year1b_end),
        as.numeric(pmin(out_date, year1b_end) - pmax(in_date, year1b_start)), 0),
      days_year2 = if_else(
        pmax(in_date, year2_start) <= pmin(out_date, year2_end),
        as.numeric(pmin(out_date, year2_end) - pmax(in_date, year2_start)), 0),
      days_year3 = if_else(
        pmax(in_date, year3_start) <= pmin(out_date, year3_end),
        as.numeric(pmin(out_date, year3_end) - pmax(in_date, year3_start)), 0)
    ) |>
    filter(days_year1a > 0 | days_year1b > 0 | days_year2 > 0 | days_year3 > 0) |>
    left_join(room_categories, by = "room")

  # Summarize by subject, housing type, and age window
  housing_data |>
    tidyr::pivot_longer(
      cols         = c(days_year1a, days_year1b, days_year2, days_year3),
      names_to     = "year",
      values_to    = "days",
      names_prefix = "days_"
    ) |>
    filter(days > 0) |>
    group_by(animal_id, housing_type, year) |>
    summarize(total_days = sum(days), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from  = c(housing_type, year),
      values_from = total_days,
      values_fill = 0,
      names_sep   = "_"
    )
}

results <- process_all_housing(
  subject_list = rrs_subjects$animal_id,
  birth_data   = rrs_subjects$Birth,
  housing_data = housing_data
)


# -----------------------------------------------------------------------------
# 5. Export
# -----------------------------------------------------------------------------

write.csv(results, "data/housing_first_3_years.csv", row.names = FALSE)
