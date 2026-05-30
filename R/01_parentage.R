# =============================================================================
# Rearing Conditions Study — Script 1: Parentage Data
# =============================================================================
# Purpose:  Pulls dam and parentage records from the institutional EHR system
#           (LabKey/PRIMe) and determines the "rearing dam" for each subject —
#           i.e., the dam that actually raised the infant, accounting for
#           foster placements and surrogate dams.
#
# Inputs:   - Subject list CSV (data/rearing_conditions_subjects.csv)
#           - LabKey EHR queries: Demographics (Dams view), parentage table
#
# Outputs:  - data/parentage.xlsx  (manually reviewed for edge cases)
#           - parentage object in R environment for use in downstream scripts
#
# Notes:    Some records with reardam = "unclear" were manually reviewed and
#           edited in the output Excel file. If re-running this script, those
#           manual edits will need to be reapplied.
#           LabKey connection requires institutional network access.
# =============================================================================

library(Rlabkey)
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(janitor)


# -----------------------------------------------------------------------------
# 1. Load subject list
# -----------------------------------------------------------------------------

rrs_subjects <- read.csv("data/rearing_conditions_subjects.csv", header = TRUE) |>
  clean_names(case = "snake") |>
  mutate(
    animal_id = as.character(animal_id),
    across(c(species, geographic_origin, birth_year, birth_type), as.factor),
    sex = as.factor(gender),
    .before = species
  ) |>
  select(-gender)


# -----------------------------------------------------------------------------
# 2. Pull parentage records from institutional EHR (LabKey/PRIMe)
# -----------------------------------------------------------------------------
# Note: baseUrl points to an institutional server; not accessible externally.
# Column selection includes observed dam, genetic dam, foster dam, and sire.

parentage_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "Demographics",
  viewName     = "Dams",
  colSelect    = "Id,species,gender,Id/age/birth,Id/birth/dam,Id/parents/dam,
                  Id/parents/damType,Id/Parents/fostermom,Id/Parents/fosterType,
                  Id/Parents/sire",
  colSort      = "Id",
  colFilter    = makeFilter(
    c("species", "EQUAL", "RHESUS MACAQUE"),
    c("Id", "IN", paste(rrs_subjects$animal_id, collapse = ";"))
  ),
  colNameOpt = "rname"
)

parentage <- parentage_raw |>
  rename(
    animal_id    = id,
    sex          = gender,
    birth        = id_age_birth,
    obsdam       = id_birth_dam,      # observed (social) dam
    gendam       = id_parents_dam,    # genetic dam
    dam_type     = id_parents_damtype,
    fosdam       = id_parents_fostermom,
    foster_type  = id_parents_fostertype,
    sire         = id_parents_sire
  )


# -----------------------------------------------------------------------------
# 3. Determine rearing dam
# -----------------------------------------------------------------------------
# Priority: foster dam > observed dam > genetic dam
# Animals with conflicting dam records are flagged as "unclear" for manual review.

parentage <- parentage |>
  mutate(
    reardam = ifelse(
      !is.na(fosdam), fosdam,
      ifelse(
        is.na(obsdam), gendam,
        ifelse(obsdam == gendam, obsdam, "unclear")
      )
    )
  )


# -----------------------------------------------------------------------------
# 4. Identify animals with multiple rearing dams (foster date differs from birth)
# -----------------------------------------------------------------------------
# Pulls foster relationship records and flags animals fostered >20 days post-birth,
# which may indicate a second rearing dam was involved.

foster_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "parentage",
  viewName     = "",
  colSelect    = "Id,date,enddate,parent,relationship,method,remark",
  colSort      = "-date",
  colFilter    = makeFilter(c("relationship/value", "EQUAL", "Foster Dam")),
  colNameOpt   = "rname"
) |>
  mutate(animal_id = id) |>
  select(-id)

# Flag animals fostered more than 20 days after birth
foster_birth <- parentage |>
  left_join(foster_raw, join_by("animal_id")) |>
  filter(!is.na(foster_type)) |>
  mutate(days_birth_to_foster = as.numeric(difftime(birth, date, units = "days"))) |>
  filter(abs(days_birth_to_foster) > 20)


# -----------------------------------------------------------------------------
# 5. Export
# -----------------------------------------------------------------------------
# Note: Output is saved as CSV for initial export, then manually reviewed
# in Excel for "unclear" cases before being saved as parentage.xlsx.

write.csv(parentage, "data/parentage.csv", row.names = FALSE)
