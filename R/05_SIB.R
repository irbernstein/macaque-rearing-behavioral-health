# =============================================================================
# Rearing Conditions Study — Script 5: Self-Injurious Behavior (SIB) Data
# =============================================================================
# Purpose:  Pulls self-injurious behavior (SIB) observations and associated
#           clinical notes (SOAPs) from the institutional EHR for all study
#           subjects.
#
#           SIB is the primary outcome variable in this study. It is recorded
#           in the EHR as a behavioral observation under category "SIB Score",
#           coded using a standardized scoring system. Clinical SOAP notes
#           are also pulled for subjects with SIB records to aid in
#           characterizing SIB type and severity.
#
# Inputs:   - data/rearing_conditions_subjects.csv
#           - LabKey EHR queries: clinical_observations (BSU Observations view),
#             clinremarks (Behavior Remarks view)
#
# Outputs:  - data/SIB_obs.csv
#           - data/SOAPs.csv
# =============================================================================

library(Rlabkey)
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)


# -----------------------------------------------------------------------------
# 1. Load subject list
# -----------------------------------------------------------------------------

rrs_subjects <- read.csv("data/rearing_conditions_subjects.csv", header = TRUE) |>
  mutate(
    AnimalID = as.character(AnimalID),
    across(c(Species, Geographic.Origin, Status, Birth.Condition, Birth.Type,
             Foster.Type, Viral.Status.in.2024), as.factor),
    Sex  = as.factor(Gender),
    birth = as.POSIXct(paste(as.Date(Birth), "00:00:00"), tz = "America/Los_Angeles"),
    .before = Species
  ) |>
  select(-Gender)


# -----------------------------------------------------------------------------
# 2. Pull SIB behavioral observations from EHR
# -----------------------------------------------------------------------------
# Filters to observations categorized as "SIB Score" within the Behavior
# category, for all study subjects.

SIB_obs_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "clinical_observations",
  viewName     = "BSU Observations",
  colSelect    = "Id,date,category,area,observation,remark,taskid,performedby,
                  requestid,Container,history,QCState,isAssignedAtTime,
                  isAssignedToProtocolAtTime,enteredSinceVetReview",
  colSort      = "-date,Id",
  colFilter    = makeFilter(
    c("Id",                "IN",    paste(rrs_subjects$AnimalID, collapse = ";")),
    c("category/category", "EQUAL", "Behavior"),
    c("category",          "EQUAL", "SIB Score")
  ),
  colNameOpt = "rname"
)

SIB_obs <- SIB_obs_raw |>
  rename(AnimalID = id) |>
  select(-requestid, -area, -taskid, -container, -history, -qcstate,
         -isassignedattime, -isassignedtoprotocolattime, -enteredsincevetreview)

SIB_animals <- unique(SIB_obs$AnimalID)


# -----------------------------------------------------------------------------
# 3. Pull SOAP clinical notes for SIB-positive subjects
# -----------------------------------------------------------------------------
# Behavioral SOAP notes provide additional context for characterizing SIB
# type (e.g., self-biting vs. skin-picking) and treatment history.

SOAPs_raw <- labkey.selectRows(
  baseUrl      = "https://[INSTITUTIONAL_EHR_URL]",
  folderPath   = "/ONPRC/EHR",
  schemaName   = "study",
  queryName    = "clinremarks",
  viewName     = "Behavior Remarks",
  colSelect    = "QCState,Id,date,project,remark,description,CEG_Plan,
                  performedby,category,taskid",
  colSort      = "-date",
  colFilter    = makeFilter(
    c("Id",       "IN",    paste(SIB_animals, collapse = ";")),
    c("category", "EQUAL", "Behavior")
  ),
  colNameOpt = "rname"
)

SOAPs <- SOAPs_raw |>
  select(-qcstate, -project, -ceg_plan, -taskid) |>
  rename(AnimalID = id)


# -----------------------------------------------------------------------------
# 4. Export
# -----------------------------------------------------------------------------

write.csv(SIB_obs, "data/SIB_obs.csv",  row.names = FALSE)
write.csv(SOAPs,   "data/SOAPs.csv",    row.names = FALSE)
