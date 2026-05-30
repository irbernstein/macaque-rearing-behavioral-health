# =============================================================================
# Rearing Conditions Study — Script 2: Cagemate History
# =============================================================================
# Purpose:  Pulls cagemate housing records from the institutional EHR and
#           calculates, for each subject, the total days spent co-housed with
#           each cagemate type during the first and second six months of life.
#
#           Cagemate types:
#             dam          — the rearing dam (from parentage data)
#             infant       — cagemate born within 365 days of subject
#             juvie        — cagemate born 366–730 days before/after subject
#             adult_female — female cagemate aged >3 years (non-dam)
#             adult_male   — male cagemate aged >3 years
#             none         — no cagemate present
#
#           Also calculates time spent in "catch" housing areas (transitional 
#           caged housing used during yearly physical exams for group-housed animals), 
#           which is added to dam time as a proxy for dam-reared time in early infancy.
#
# Inputs:   - data/parentage.xlsx (from Script 1, manually reviewed)
#           - LabKey EHR queries: housingRoommatesDivider, housing
#
# Outputs:  - data/cagemate_hx.csv
# =============================================================================

library(Rlabkey)
library(tidyverse)
library(lubridate)
library(readxl)
conflicted::conflicts_prefer(dplyr::filter)


# -----------------------------------------------------------------------------
# 1. Load subject list with parentage
# -----------------------------------------------------------------------------

rrs_subjects_ptg <- read_excel("data/parentage.xlsx") |>
  mutate(animal_id = as.character(animal_id)) |>
  select(-starts_with("..."))  # remove any index columns from Excel export

subject_list <- unique(rrs_subjects_ptg$animal_id)


# -----------------------------------------------------------------------------
# 2. Pull cagemate history from EHR
# -----------------------------------------------------------------------------
# Retrieves all co-housing records for subjects, excluding records without
# a cage assignment (i.e., group enclosure records).

cagemate_hx_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "housingRoommatesDivider",
  viewName     = "Caged Housing Only",
  colSelect    = "Id,RoommateId,room,cage,roommateStart,roommateEnd,DaysCoHoused,
                  startDate,removalDate,duration,Id/age/birth,
                  RoommateId/Demographics/birth,RoommateId/Demographics/gender",
  colSort      = "-Id,-RoommateStart",
  colFilter    = makeFilter(
    c("cage", "NOT_MISSING", ""),
    c("Id", "IN", paste(subject_list, collapse = ";"))
  ),
  colNameOpt = "rname"
)


# -----------------------------------------------------------------------------
# 3. Clean and enrich cagemate records
# -----------------------------------------------------------------------------

cagemate_hx <- cagemate_hx_raw |>
  select(-startdate, -removaldate) |>
  rename(
    animal_id       = id,
    cagemate_id     = roommateid,
    cagemate_start  = roommatestart,
    cagemate_end    = roommateend,
    days_cohoused   = dayscohoused,
    id_birth        = id_age_birth,
    cagemate_birth  = roommateid_demographics_birth,
    cagemate_sex    = roommateid_demographics_gender
  ) |>
  # Parse dates (remove timestamps)
  mutate(across(
    c(cagemate_start, cagemate_end, id_birth, cagemate_birth),
    ~ as_date(parse_date_time(., orders = c("ymd_HMS", "ymd")))
  )) |>
  # Calculate subject and cagemate ages at start/end of each co-housing record
  mutate(
    id_age_start       = time_length(interval(id_birth, cagemate_start), unit = "days"),
    id_age_end         = time_length(interval(id_birth, cagemate_end),   unit = "days"),
    cagemate_age_start = time_length(interval(cagemate_birth, cagemate_start), unit = "days"),
    cagemate_age_end   = time_length(interval(cagemate_birth, cagemate_end),   unit = "days")
  ) |>
  # Join rearing dam info from parentage data
  left_join(
    rrs_subjects_ptg |> select(animal_id, reardam, reardam2),
    by = "animal_id"
  ) |>
  mutate(reardam2 = as.character(reardam2))


# -----------------------------------------------------------------------------
# 4. Subset by age window and classify cagemate types
# -----------------------------------------------------------------------------
# Cagemate type is assigned based on the age difference between subject and
# cagemate at the start of the co-housing record, and whether the cagemate
# is the rearing dam.

classify_cagemate_type <- function(df) {
  df |>
    mutate(
      cagemate_type = case_when(
        cagemate_id %in% c(reardam, reardam2) ~ "dam",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 366 &
          abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) <= 730 ~ "juvie",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 0 &
          abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) <= 365 ~ "infant",
        cagemate_age_start > 3 & cagemate_sex == "f" &
          !cagemate_id %in% c(reardam, reardam2) ~ "adult_female",
        cagemate_age_start > 3 & cagemate_sex == "m" ~ "adult_male",
        TRUE ~ "none"
      )
    )
}

# First six months of life (days 0–182)
cagemate_hx_year1a <- cagemate_hx |>
  filter(id_age_start <= 182) |>
  mutate(
    adjusted_end_date = if_else(id_age_end > 182, id_birth + days(182), cagemate_end),
    adjusted_duration = time_length(interval(cagemate_start, adjusted_end_date), unit = "days"),
    end_date_match    = adjusted_end_date == cagemate_end
  ) |>
  classify_cagemate_type()

# Second six months of life (days 183–365)
cagemate_hx_year1b <- cagemate_hx |>
  filter(id_age_start <= 365 & id_age_end >= 183) |>
  mutate(
    adjusted_start_date = if_else(id_age_start < 183, id_birth + days(183), cagemate_start),
    adjusted_end_date   = if_else(id_age_end > 365,   id_birth + days(365), cagemate_end),
    adjusted_duration   = time_length(interval(adjusted_start_date, adjusted_end_date), unit = "days"),
    start_date_match    = adjusted_start_date == cagemate_start,
    end_date_match      = adjusted_end_date == cagemate_end
  ) |>
  classify_cagemate_type()


# -----------------------------------------------------------------------------
# 5. Summarize co-housing duration by cagemate type
# -----------------------------------------------------------------------------
# Custom function to merge overlapping date intervals before summing duration.
# This prevents double-counting when a subject had overlapping co-housing records
# (e.g., during a period when records were entered redundantly).

merge_intervals <- function(starts, ends) {
  df <- data.frame(start = starts, end = ends) |>
    filter(!is.na(start) & !is.na(end)) |>
    arrange(start)

  if (nrow(df) == 0) return(0)

  merged_start <- df$start[1]
  merged_end   <- df$end[1]
  total_days   <- 0

  for (i in seq_len(nrow(df))[-1]) {
    if (df$start[i] <= merged_end) {
      # Overlapping interval — extend the current window
      merged_end <- max(merged_end, df$end[i])
    } else {
      # Gap found — close current window and start a new one
      total_days   <- total_days + as.numeric(difftime(merged_end, merged_start, units = "days"))
      merged_start <- df$start[i]
      merged_end   <- df$end[i]
    }
  }
  total_days + as.numeric(difftime(merged_end, merged_start, units = "days"))
}

cagemate_types <- c("dam", "infant", "juvie", "adult_female", "adult_male", "none")

# Summary for first six months
cagemate_hx_year1a_summary <- cagemate_hx_year1a |>
  group_by(animal_id, cagemate_type) |>
  summarize(
    total_duration = merge_intervals(cagemate_start, adjusted_end_date),
    .groups = "drop"
  ) |>
  tidyr::complete(animal_id, cagemate_type = cagemate_types, fill = list(total_duration = 0)) |>
  pivot_wider(names_from = cagemate_type, values_from = total_duration) |>
  rename_with(~ paste0(., "_year1a"), .cols = -animal_id) |>
  select(animal_id, dam_year1a, infant_year1a, juvie_year1a, adult_female_year1a, adult_male_year1a, none_year1a)

# Summary for second six months
cagemate_hx_year1b_summary <- cagemate_hx_year1b |>
  group_by(animal_id, cagemate_type) |>
  summarize(
    total_duration = merge_intervals(adjusted_start_date, adjusted_end_date),
    .groups = "drop"
  ) |>
  tidyr::complete(animal_id, cagemate_type = cagemate_types, fill = list(total_duration = 0)) |>
  pivot_wider(names_from = cagemate_type, values_from = total_duration) |>
  rename_with(~ paste0(., "_year1b"), .cols = -animal_id) |>
  select(animal_id, dam_year1b, infant_year1b, juvie_year1b, adult_female_year1b, adult_male_year1b, none_year1b)

# Combine both age windows
cagemate_hx_summary <- cagemate_hx_year1a_summary |>
  full_join(cagemate_hx_year1b_summary, by = "animal_id")


# -----------------------------------------------------------------------------
# 6. Pull and incorporate time in transitional "catch" housing
# -----------------------------------------------------------------------------
# Catch housing areas are transitional spaces used during pair separations and
# moves. Time in catch housing without a cagemate is treated as a proxy for
# dam-reared time in early infancy (when infants are typically in catch areas
# with their dams during colony moves).

catch_time_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "housing",
  viewName     = "",
  colSelect    = "Id,Id/birth/date,date,enddate,room/housingType,room,cage",
  colSort      = "-date",
  colFilter    = makeFilter(
    c("room/room", "CONTAINS_ONE_OF", "catch area 2;catch area 5;catch area 8;bos rm 122;bos rm 123"),
    c("Id", "IN", paste(subject_list, collapse = ";")),
    c("cage", "MISSING", "")
  ),
  colNameOpt = "rname"
)

catch_time <- catch_time_raw |>
  select(-room_housingtype) |>
  rename(animal_id = id, date_in = date, date_out = enddate, id_birth = id_birth_date) |>
  mutate(across(
    c(date_in, date_out, id_birth),
    ~ as_date(parse_date_time(., orders = c("ymd_HMS", "ymd")))
  )) |>
  mutate(
    id_age_start = time_length(interval(id_birth, date_in),  unit = "days"),
    id_age_end   = time_length(interval(id_birth, date_out), unit = "days"),
    # Define age window boundaries
    year1a_start = id_birth,
    year1a_end   = id_birth + 182,
    year1b_start = id_birth + 182,
    year1b_end   = id_birth + 365,
    # Calculate overlap with each age window
    overlap_start_y1a = pmax(date_in, year1a_start, na.rm = TRUE),
    overlap_end_y1a   = pmin(date_out, year1a_end,  na.rm = TRUE),
    days_year1a = if_else(overlap_start_y1a <= overlap_end_y1a,
                          as.numeric(overlap_end_y1a - overlap_start_y1a), 0),
    overlap_start_y1b = pmax(date_in, year1b_start, na.rm = TRUE),
    overlap_end_y1b   = pmin(date_out, year1b_end,  na.rm = TRUE),
    days_year1b = if_else(overlap_start_y1b <= overlap_end_y1b,
                          as.numeric(overlap_end_y1b - overlap_start_y1b), 0)
  )

catch_summary <- catch_time |>
  tidyr::pivot_longer(
    cols        = c(days_year1a, days_year1b),
    names_to    = "year",
    values_to   = "days",
    names_prefix = "days_"
  ) |>
  filter(days > 0) |>
  group_by(animal_id, year) |>
  summarize(total_days = sum(days), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from   = year,
    values_from  = total_days,
    values_fill  = 0
  ) |>
  rename(catch_year1a = year1a, catch_year1b = year1b)

# Join catch time to cagemate summary
cagemate_hx_summary <- cagemate_hx_summary |>
  full_join(catch_summary, by = "animal_id") |>
  # Add catch time to dam time in each age window
  mutate(
    dam_year1a = case_when(
      is.na(dam_year1a)  & catch_year1a > 0    ~ catch_year1a,
      !is.na(dam_year1a) & is.na(catch_year1a) ~ dam_year1a,
      !is.na(dam_year1a) & catch_year1a > 0    ~ dam_year1a + catch_year1a,
      TRUE ~ dam_year1a
    ),
    dam_year1b = case_when(
      is.na(dam_year1b)  & catch_year1b > 0    ~ catch_year1b,
      !is.na(dam_year1b) & is.na(catch_year1b) ~ dam_year1b,
      !is.na(dam_year1b) & catch_year1b > 0    ~ dam_year1b + catch_year1b,
      TRUE ~ dam_year1b
    )
  )


# -----------------------------------------------------------------------------
# 7. Export
# -----------------------------------------------------------------------------

write_csv(cagemate_hx_summary, "data/cagemate_hx.csv")
