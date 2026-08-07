## Rearing conditions study — cagemate data
## Script 02: Cagemate history — year 1 (days 0–365)
##
## NOTE: Data queries use labkey.selectRows() to pull from ONPRC's PRIMe EHR
## (LabKey platform). These queries require institutional access and cannot be
## run outside of ONPRC. Query structure is shown for methodological transparency.
## Users at other institutions would need to adapt queries to their own EHR system
## or load data from a local export.
##
## Output: data/days_by_combo_wide.csv
##   One row per subject. Columns represent total days spent in each
##   concurrent cagemate combination during the first year of life.

# Libraries ----
library(Rlabkey)
library(tidyverse)
library(lubridate)
library(readxl)
library(purrr)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)


# Bring in subjects with correct parentage ----

  # Import subjects with parentage assignments (output of 01_parentage.R)
  rrs_subjects_ptg <- read_excel("data/parentage.xlsx") |>
    mutate(animal_id = as.character(animal_id)) |>
    select(-`...1`)

  # Create subject ID list for LabKey imports: n = 5196
  subject_list <- unique(rrs_subjects_ptg$animal_id)


# Get and parse cagemate data ----

  ## Pull cagemate data for subject list from PRIMe ----
  # Path in PRIMe: "Colony Management" > "Cagemate History" > "RRS-cagemates"
  # Excludes records where cage location is blank (i.e., non-cage housing)

  cagemate_hx_raw <- labkey.selectRows(
    baseUrl      = "[ONPRC PRIMe LabKey URL]",
    folderPath   = "[ONPRC EHR folder path]",
    schemaName   = "study",
    queryName    = "housingRoommatesDivider",
    viewName     = "Caged Housing Only",
    colSelect    = "Id,RoommateId,room,cage,roommateStart,roommateEnd,DaysCoHoused,startDate,removalDate,duration,Id/age/birth,RoommateId/Demographics/birth,RoommateId/Demographics/gender",
    colSort      = "-Id,-RoommateStart",
    colFilter    = makeFilter(
      c("cage", "NOT_MISSING", ""),
      c("Id", "IN", paste(subject_list, collapse = ";"))
    ),
    containerFilter = NULL,
    colNameOpt   = "rname"
  )


  ## Clean up cagemate history ----

  cagemate_hx <- cagemate_hx_raw |>
    select(-startdate, -removaldate) |>

    # Rename columns
    rename(
      animal_id      = id,
      cagemate_id    = roommateid,
      cagemate_start = roommatestart,
      cagemate_end   = roommateend,
      days_cohoused  = dayscohoused,
      id_birth       = id_age_birth,
      cagemate_birth = roommateid_demographics_birth,
      cagemate_sex   = roommateid_demographics_gender,
    ) |>

    # Parse dates and remove timestamps
    mutate(across(c(cagemate_start, cagemate_end, id_birth, cagemate_birth),
                  ~ as_date(parse_date_time(., orders = c("ymd_HMS", "ymd"))))) |>

    # Add ages (in days) at start and end of each cagemate record
    mutate(
      id_age_start       = time_length(interval(id_birth, cagemate_start), unit = "days"),
      id_age_end         = time_length(interval(id_birth, cagemate_end),   unit = "days"),
      cagemate_age_start = time_length(interval(cagemate_birth, cagemate_start), unit = "days"),
      cagemate_age_end   = time_length(interval(cagemate_birth, cagemate_end),   unit = "days")
    ) |>

    # Join with subject list to bring in rearing dam IDs
    left_join(rrs_subjects_ptg |> select(animal_id, reardam, reardam2), by = "animal_id") |>
    mutate(reardam2 = as.character(reardam2))


  ## Identify subjects without cagemate history ----

    # Subjects with cagemate history: n = 5031
    subjects_cagemate_hx <- unique(cagemate_hx$animal_id)

    # Subjects in list but NOT in pulled data (no caging history): n = 165
    subjects_no_cagemate_hx <- setdiff(subject_list, subjects_cagemate_hx)

    # Subjects in pulled data but NOT in subject list: n = 0 (expected)
    extra_in_pull <- setdiff(subjects_cagemate_hx, subject_list)


# Classify cagemate types ----
  # Categories:
  #   dam           = rearing dam (from parentage data)
  #   infant        = cagemate DOB within 365 days of subject DOB
  #   juvie         = cagemate DOB 366-1095 days from subject DOB
  #   adult_female  = female cagemate > 3 years old at record start, not dam
  #   adult_male    = male cagemate > 3 years old at record start

  cagemate_hx <- cagemate_hx |>
    mutate(
      cagemate_type = case_when(
        is.na(cagemate_id)                                                                      ~ "none",
        cagemate_id == reardam | cagemate_id == reardam2                                        ~ "dam",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 0 &
          abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) <= 365            ~ "infant",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 366 &
          abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) <= 1095           ~ "juvie",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 1096 &
          cagemate_sex == "f"                                                                   ~ "adult_female",
        abs(time_length(interval(id_birth, cagemate_birth), unit = "days")) >= 1096 &
          cagemate_sex == "m"                                                                   ~ "adult_male",
        TRUE                                                                                    ~ "other"
      )
    )


# Subset to year 1 (birth through day 365) ----

  cagemate_hx_year1 <- cagemate_hx |>
    filter(id_age_start <= 365) |>
    mutate(
      adjusted_end_date = if_else(
        cagemate_end > id_birth + 365 | is.na(cagemate_end),
        id_birth + 365,
        cagemate_end
      ),
      adjusted_duration = time_length(interval(cagemate_start, adjusted_end_date), unit = "days")
    )


# Classify concurrent cagemate combinations ----

  # For each timeline segment, identify the set of cagemate types
  # simultaneously present and assign a combo label
  classify_segment <- function(active_types) {
    case_when(
      setequal(active_types, c("none"))                               ~ "no_cagemate",
      setequal(active_types, c("dam"))                                ~ "dam",
      setequal(active_types, c("dam", "adult_female"))                ~ "dam_adult_female",
      setequal(active_types, c("dam", "adult_female", "infant"))      ~ "dam_adult_female_infant",
      setequal(active_types, c("dam", "adult_female", "juvie"))       ~ "dam_adult_female_juvie",
      setequal(active_types, c("dam", "juvie"))                       ~ "dam_juvie",
      setequal(active_types, c("dam", "adult_male"))                  ~ "dam_adult_male",
      setequal(active_types, c("infant"))                             ~ "infant",
      setequal(active_types, c("juvie"))                              ~ "juvie",
      setequal(active_types, c("adult_female"))                       ~ "adult_female",
      setequal(active_types, c("adult_female", "adult_male", "dam"))  ~ "adult_female_adult_male_dam",
      setequal(active_types, c("adult_female", "dam", "infant", "juvie")) ~ "dam_adult_female_juvie_infant",
      setequal(active_types, c("adult_female", "infant"))             ~ "adult_female_infant",
      setequal(active_types, c("adult_female", "infant", "juvie"))    ~ "adult_female_juvie_inf",
      setequal(active_types, c("adult_female", "juvie"))              ~ "adult_female_juvie",
      setequal(active_types, c("adult_male"))                         ~ "adult_male",
      setequal(active_types, c("dam", "infant"))                      ~ "dam_infant",
      setequal(active_types, c("dam", "infant", "juvie"))             ~ "dam_juvie_infant",
      setequal(active_types, c("infant", "juvie"))                    ~ "juvie_infant",
      TRUE                                                            ~ "not_caged"
    )
  }

  # For one subject's rows, segment the timeline and sum days per combo
  compute_days_by_combo <- function(df) {
    breakpoints <- sort(unique(c(df$start, df$end)))
    if (length(breakpoints) < 2) return(tibble())

    segments <- tibble(
      seg_start = breakpoints[-length(breakpoints)],
      seg_end   = breakpoints[-1]
    ) |>
      mutate(
        active_types   = map2(seg_start, seg_end, function(s, e) {
          df |> filter(start <= s, end >= e) |> pull(cagemate_type) |> unique()
        }),
        cagemate_combo = map_chr(active_types, classify_segment),
        days           = as.numeric(seg_end - seg_start)
      )

    segments |>
      group_by(cagemate_combo) |>
      summarize(days = sum(days), .groups = "drop")
  }

  # Diagnostic version: returns segment-level detail for QC
  inspect_segments <- function(df) {
    breakpoints <- sort(unique(c(df$start, df$end)))
    if (length(breakpoints) < 2) return(tibble())

    tibble(
      seg_start = breakpoints[-length(breakpoints)],
      seg_end   = breakpoints[-1]
    ) |>
      mutate(
        active_types = map(seg_start, function(s) {
          e <- seg_end[which(seg_start == s)]
          df |> filter(start <= s, end >= e) |> pull(cagemate_type) |> unique()
        }),
        active_types_label = map_chr(active_types, ~ paste(sort(.x), collapse = " + ")),
        category           = map_chr(active_types, classify_segment),
        days               = as.numeric(seg_end - seg_start)
      ) |>
      select(seg_start, seg_end, days, active_types_label, category)
  }

  # Run diagnostic inspection across all subjects
  segment_detail <- cagemate_hx_year1 |>
    transmute(animal_id, cagemate_type, start = cagemate_start, end = adjusted_end_date) |>
    group_by(animal_id) |>
    group_split() |>
    map_dfr(~ inspect_segments(.x) |> mutate(animal_id = unique(.x$animal_id))) |>
    relocate(animal_id)

  write.csv(segment_detail, "data/segment_detail.csv")

  # Run combo computation across all subjects
  days_by_combo <- cagemate_hx_year1 |>
    transmute(animal_id, cagemate_type, start = cagemate_start, end = adjusted_end_date) |>
    group_by(animal_id) |>
    group_split() |>
    map_dfr(~ compute_days_by_combo(.x) |> mutate(animal_id = unique(.x$animal_id))) |>
    relocate(animal_id) |>
    filter(cagemate_combo != "not_caged")

  # Verify: no subject should appear more than once per combo
  days_by_combo |>
    count(animal_id, cagemate_combo) |>
    filter(n > 1)


# Pull time in catch caging ----
  # "Catch caging" = short-term individual housing in catch areas or bosky rooms
  # without standard cage assignment. Time here is attributed to dam housing
  # for infants in the year 1 period.

  catch_time_raw <- labkey.selectRows(
    baseUrl      = "[ONPRC PRIMe LabKey URL]",
    folderPath   = "[ONPRC EHR folder path]",
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
    containerFilter = NULL,
    colNameOpt   = "rname"
  )

  catch_time <- catch_time_raw |>
    select(-room_housingtype) |>
    rename(animal_id = id, date_in = date, date_out = enddate, id_birth = id_birth_date) |>
    mutate(across(c(date_in, date_out, id_birth),
                  ~ as_date(parse_date_time(., orders = c("ymd_HMS", "ymd"))))) |>
    mutate(
      id_age_start = time_length(interval(id_birth, date_in),  unit = "days"),
      id_age_end   = time_length(interval(id_birth, date_out), unit = "days")
    )

  # Summarize catch time within year 1 per subject
  catch_summary <- catch_time |>
    filter(id_age_start <= 365) |>
    mutate(
      adjusted_date_out = if_else(
        date_out > id_birth + 365 | is.na(date_out),
        id_birth + 365,
        date_out
      ),
      duration = time_length(interval(date_in, adjusted_date_out), unit = "days")
    ) |>
    group_by(animal_id) |>
    summarize(catch_duration = sum(duration, na.rm = TRUE), .groups = "drop")


# Add catch time to dam days and convert to wide format ----

  days_by_combo <- days_by_combo |>
    left_join(catch_summary, by = "animal_id") |>
    mutate(
      catch_duration = replace_na(catch_duration, 0),
      days = if_else(cagemate_combo == "dam", days + catch_duration, days)
    ) |>
    select(-catch_duration)

  # Convert to wide format
  days_by_combo_wide <- days_by_combo |>
    pivot_wider(
      id_cols     = animal_id,
      names_from  = cagemate_combo,
      values_from = days,
      values_fill = 0
    )

  # Add variable for any time with dam (all dam-containing combos)
  days_by_combo_wide <- days_by_combo_wide |>
    mutate(dam_any = rowSums(pick(contains("dam")), na.rm = TRUE))


# Export ----
  write.csv(days_by_combo_wide, "data/days_by_combo_wide.csv")
