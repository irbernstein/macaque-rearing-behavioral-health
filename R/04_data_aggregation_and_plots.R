## Rearing conditions study — data aggregation and exploratory plots
## Script 04: Merge data sources into analysis dataset and generate exploratory plots
##
## Inputs (outputs of prior scripts):
##   - SIB observations (data/SIB_obs.xlsx)
##   - Subject list with demographics (data/rearing_conditions_subjects.csv)
##   - Cagemate summary (data/cagemate_hx.csv) — output of 02_cagemates_year1.R
##   - Housing history (data/housing_first_3_years.xlsx) — output of 03_housing.R
##   - Sedation summary (data/sedation_summary.csv)
##
## Output:
##   - full_data_wide.csv — wide-format analytic dataset
##   - data_long — long-format dataset for plots (created in session)

# Libraries ----
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(janitor)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)


# 1. Load data files ----

  # 1a. SIB observations
  SIB <- read_excel("data/SIB_obs.xlsx") |>
    mutate(across(where(is.character), as.factor), AnimalID = as.character(AnimalID)) |>
    clean_names(case = "snake") |>
    rename(days_first_sb = age_first_sb_days)

  # 1b. Subject list with demographics
  rrs_subjects <- read.csv("data/rearing_conditions_subjects.csv") |>
    clean_names() |>
    mutate(animal_id = as.character(animal_id)) |>
    select(animal_id, gender, birth_year, birth, maximum_age) |>
    mutate(sex = as.factor(gender), .before = birth_year) |>
    select(-gender) |>
    mutate(birth = as.Date(birth, format = "%m/%d/%Y"))

  # 1c. Cagemate summary (output of 02_cagemates_year1.R)
  cagemates <- read.csv("data/cagemate_hx.csv", header = TRUE) |>
    mutate(across(where(is.character), as.numeric), animal_id = as.character(animal_id))

  # 1d. Housing history (output of 03_housing.R)
  housing <- read_excel("data/housing_first_3_years.xlsx") |>
    clean_names() |>
    mutate(animal_id = as.character(animal_id))

  # 1e. Sedation history
  sedations <- read.csv("data/sedation_summary.csv", header = TRUE) |>
    select(-X) |>
    mutate(animal_id = as.character(animal_id))


# 2. Join tables into wide-format analytic dataset ----

  full_data_wide1 <- rrs_subjects |>
    left_join(SIB,       join_by(animal_id)) |>
    left_join(cagemates, join_by(animal_id)) |>
    left_join(housing,   join_by(animal_id)) |>
    left_join(sedations, join_by(animal_id))

  # Clean up and derive additional variables
  full_data_wide <- full_data_wide1 |>
    select(-birth.x, -birth.y, -x1, -yob) |>
    mutate(
      sib            = ifelse(is.na(sib_type), 0, 1),
      caging_year2.10 = caging_year2 / 10,
      caging_year3.10 = caging_year3 / 10,
      caging_year1    = caging_year1a + caging_year1b,
      caging_year1.10 = caging_year1 / 10
    ) |>
    mutate(
      dam.10                    = dam / 10,
      infant.10                 = infant / 10,
      dam_adult_female.10       = dam_adult_female / 10,
      dam_adult_female_infant.10 = dam_adult_female_infant / 10,
      dam_any.10                = dam_any / 10,
      no_cagemate.10            = no_cagemate / 10
    ) |>
    mutate(sedations_year1 = sedations_year1a + sedations_year1b) |>
    select(-caging_year1a, -caging_year1b, -group_year1b, -group_year1a,
           -sedations_year1a, -sedations_year1b) |>
    mutate(
      companion1    = dam + infant + juvie + adult_female + adult_male,
      companion2    = dam_adult_female + adult_female_infant + dam_infant + dam_juvie +
                      dam_adult_male + juvie_infant + adult_female_juvie,
      companion3    = dam_adult_female_infant + dam_juvie_infant + dam_adult_female_juvie +
                      adult_female_juvie_inf,
      companion1.10 = companion1 / 10,
      companion2.10 = companion2 / 10,
      companion3.10 = companion3 / 10
    )

  # Export wide-format analytic dataset
  write.csv(full_data_wide, "data/full_data_wide.csv")


# 3. Convert to long format for plots ----

  combo_cols <- c("dam", "infant", "no_cagemate", "adult_female", "dam_adult_male",
                  "dam_adult_female", "adult_male", "dam_adult_female_infant", "dam_juvie",
                  "adult_female_infant", "adult_female_juvie", "juvie", "juvie_infant",
                  "dam_adult_female_juvie", "dam_adult_female_juvie_infant", "adult_female_juvie_inf",
                  "dam_juvie_infant", "adult_female_adult_male_dam", "dam_any")

  static_cols <- c("sex", "birth_year", "maximum_age", "sib_type", "first_date_sb",
                   "days_first_sb", "sb_emerged", "sib", "sedations_year1", "total_days")

  # Cagemate combo variables — long, all treated as year 1
  combo_long <- full_data_wide |>
    select(animal_id, all_of(combo_cols)) |>
    pivot_longer(
      cols      = all_of(combo_cols),
      names_to  = "cagemate_combo",
      values_to = "days_with_combo"
    ) |>
    mutate(year = 1L)

  # Caging days per year — long, year extracted from column name
  caging_long <- full_data_wide |>
    select(animal_id, matches("^caging_year\\d$")) |>
    pivot_longer(
      cols         = -animal_id,
      names_to     = "year",
      names_pattern = "caging_year(\\d)",
      values_to    = "caging_days"
    ) |>
    mutate(year = as.integer(year))

  # Combine into long-format dataset
  data_long <- caging_long |>
    full_join(combo_long, by = c("animal_id", "year")) |>
    left_join(full_data_wide |> select(animal_id, all_of(static_cols)), by = "animal_id") |>
    arrange(animal_id, year)


# 4. Exploratory plots ----

  ## Number of animals in each cagemate condition (at least 1 day, year 1) ----
  data_long |>
    filter(days_with_combo > 0, year == 1) |>
    group_by(cagemate_combo) |>
    summarise(n_animals = n_distinct(animal_id)) |>
    arrange(-n_animals)

  ## Animals per cagemate condition — caged subjects only ----
  data_long |>
    filter(year == 1, days_with_combo > 10, caging_days > 180) |>
    group_by(cagemate_combo, sib_type) |>
    summarise(n_animals = n_distinct(animal_id)) |>
    ggplot(aes(fct_reorder(cagemate_combo, n_animals), n_animals, fill = sib_type)) +
    geom_col(position = position_dodge(preserve = "single")) +
    geom_text(aes(label = n_animals),
              position = position_dodge(width = 0.9),
              hjust = -0.2, size = 3) +
    coord_flip() +
    theme_classic()

  ## Days in cagemate combo by SIB type (caged subjects, year 1) ----
  data_long |>
    filter(caging_days > 180, year == 1,
           cagemate_combo %in% c("dam", "dam_adult_female", "dam_adult_female_infant", "infant")) |>
    ggplot(aes(cagemate_combo, days_with_combo, fill = sib_type)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                 color = "darkolivegreen3", position = position_dodge(.75)) +
    facet_wrap(~sex) +
    theme_classic()

  ## Time in caging by SIB type, sex, and year ----
  data_long |>
    ggplot(aes(sex, caging_days, fill = sib_type)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                 color = "darkolivegreen3", position = position_dodge(.75)) +
    facet_wrap(~year) +
    theme_classic()

  ## SIB type by sedations in first year ----
  data_long |>
    ggplot(aes(sex, sedations_year1, fill = sib_type)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                 color = "darkolivegreen3", position = position_dodge(.75)) +
    theme_classic()

  ## Sedations and time in caging in year 1 ----
  data_long |>
    filter(year == 1) |>
    ggplot(aes(caging_days, sedations_year1, color = sib_type)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~sex) +
    theme_classic()

  ## Correlation between days in caging across years ----
  full_data_wide |>
    ggplot(aes(caging_year1, caging_year2)) +
    geom_point(alpha = .3, shape = 21, fill = "slateblue", color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic()

  full_data_wide |>
    ggplot(aes(caging_year2, caging_year3)) +
    geom_point(alpha = .3, shape = 21, fill = "tomato", color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic()

  full_data_wide |>
    ggplot(aes(caging_year1, caging_year3)) +
    geom_point(alpha = .3, shape = 21, fill = "violet", color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic()

  ## Animals per birth year spending > 180 days in caging (year 1) ----
  data_long |>
    filter(year == 1, caging_days > 180) |>
    group_by(birth_year) |>
    summarize(n_animal = n_distinct(animal_id)) |>
    ggplot(aes(birth_year, n_animal)) +
    geom_col() +
    scale_x_continuous(breaks = seq(min(data_long$birth_year), max(data_long$birth_year), by = 1)) +
    theme_classic()

  ## SIB cases per birth year ----
  data_long |>
    group_by(birth_year, sib_type) |>
    summarize(n_animal = n_distinct(animal_id)) |>
    ggplot(aes(birth_year, n_animal, fill = sib_type)) +
    geom_col() +
    scale_x_continuous(breaks = seq(min(data_long$birth_year), max(data_long$birth_year), by = 1)) +
    theme_classic()
