# =============================================================================
# Rearing Conditions Study — Script 4: Pairing History
# =============================================================================
# Purpose:  Pulls social pairing records from the institutional EHR and
#           calculates, for each subject, the total number of unique pairing
#           attempts and the number that ended in incompatibility.
#
#           Pairing incompatibility rate is used as an outcome variable
#           reflecting adult social housing success, a key behavioral health
#           indicator in captive rhesus macaques.
#
#           Certain pairing event types are excluded from analysis (e.g.,
#           temporary moves, bio dam reunites, foster placements, monitoring
#           events) to focus on substantive social introduction attempts.
#
# Inputs:   - data/rearing_conditions_subjects.csv
#           - LabKey EHR query: pairingSummary (Pairing Events view)
#
# Outputs:  - data/pairing_summary.csv
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
    AnimalID = as.character(AnimalID),
    across(c(Species, Geographic.Origin, Status, Birth.Condition, Birth.Type), as.factor),
    Sex = as.factor(Gender),
    .before = Species
  ) |>
  select(-Gender)


# -----------------------------------------------------------------------------
# 2. Pull pairing records from EHR
# -----------------------------------------------------------------------------
# Excludes event types that do not represent substantive social introduction
# attempts: temporary moves (TMB), short-term foster/surrogate placements (STF,
# STE), bio dam reunites, foster placements, and monitoring/comment events.

pairing_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "pairingSummary",
  viewName     = "Pairing Events",
  colSelect    = "Id,infant_id,lowestCage,otherIds,Id/demographics/gender,
                  other_infant,date,category,eventType,remark,goal,outcome,
                  enddate,endeventType,separationreason,priorgrouphousing,
                  observation,duration,remark2,performedby,taskid",
  colSort      = "-date",
  colFilter    = makeFilter(
    c("eventType", "NOT_IN",
      "TMB pair;TMB;STF;STF.;STE;STE.;STF DNPC;STF DNPC.;STF/E;STF/E DNPC;
       STF/E DNPC.;Bio dam reunite;C-sx reunite;Nonlactating foster;
       Lactating foster;General comment;Pair Monitor;remote monitoring"),
    c("Id", "IN", paste(rrs_subjects$AnimalID, collapse = ";"))
  ),
  colNameOpt = "rname"
)

pairing_hx <- pairing_raw |>
  rename(AnimalID = id, PairID = lowestcage, sex = id_demographics_gender, startdate = date)


# -----------------------------------------------------------------------------
# 3. Summarize pairing outcomes per subject
# -----------------------------------------------------------------------------
# For each subject, calculates:
#   total_unique_pairs:      number of distinct pairing attempts (by cage ID)
#   incompatibility_pairs:   number of pairs ending in incompatibility

pairing_summary <- pairing_hx |>
  group_by(AnimalID) |>
  summarise(
    total_unique_pairs    = n_distinct(PairID),
    incompatibility_pairs = n_distinct(
      PairID[grepl("incompatibility|not compatible", separationreason, ignore.case = TRUE)]
    )
  )


# -----------------------------------------------------------------------------
# 4. Export
# -----------------------------------------------------------------------------

write.csv(pairing_summary, "data/pairing_summary.csv", row.names = FALSE)
