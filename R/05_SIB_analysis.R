## Rearing conditions study — SIB (self-injurious behavior) analysis
## Script 05: Mixed-effects logistic regression models predicting SIB outcome
##
## Input: full_data_wide.csv — output of 05_data_aggregation_and_plots.R
##
## Models use glmer() (lme4) with binomial family.
## Random effects: birth year, maximum age at observation
## Fixed effects: sex, time in indoor caging (per 10 days), cagemate type,
##                sedation count
##
## Three analysis questions:
##   Q1: Effect of time in caging during year 1 on SIB
##   Q2: Effect of cagemate type on SIB (indoor-reared subjects only)
##   Q3: Effect of sedations during year 1 on SIB

# Libraries ----
library(lme4)
library(tidyverse)
library(car)         # VIF
library(performance) # R-squared
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)


# Load data ----
full_data_wide <- read.csv("data/full_data_wide.csv")


# Q1: Effect of time in caging — year 1 ----
# Analysis uses full subject list (n = ~5,000)
# Outcome: sib (binary 0/1)

  # Null model — random effects only
  sb1 <- glmer(sib ~ (1|birth_year) + (1|maximum_age),
               family = "binomial", data = full_data_wide)
  summary(sb1) # AIC 1090

  # Add sex
  sb2 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex,
               family = "binomial", data = full_data_wide)
  summary(sb2) # AIC 1075 — males SIB more

  # Add caging year 1a only
  sb3 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1a.10,
               family = "binomial", data = full_data_wide)
  summary(sb3) # AIC 964, caging year 1a significant

  # Add caging year 1b only
  sb4 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1b.10,
               family = "binomial", data = full_data_wide)
  summary(sb4) # AIC 934, caging year 1b significant

  # Add both year 1a and 1b
  sb5 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1a.10 + caging_year1b.10,
               family = "binomial", data = full_data_wide)
  summary(sb5) # AIC 928, both significant
  vif(sb5)

  # Combined year 1 (1a + 1b)
  sb6 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10,
               family = "binomial", data = full_data_wide)
  summary(sb6) # AIC 929 — best model for year 1
  vif(sb6)     # All VIF below 2
  r2(sb6)
  # Conditional R2: 0.276 (full model)
  # Marginal R2:    0.224 (fixed effects only)

  # AIC comparison
  rbind(AIC(sb1), AIC(sb2), AIC(sb3), AIC(sb4), AIC(sb5), AIC(sb6))
  # sb6 is the best single-year model


# Q1 continued: Effect of time in caging — year 2 ----
# Filter out subjects with SIB onset in year 1b (n = 7 removed)

  fdw_2nd <- full_data_wide |>
    mutate(sb_emerged = as.factor(sb_emerged)) |>
    filter(sb_emerged != "year1b" | is.na(sb_emerged))

  # Null model — year 2 data
  sby2.1 <- glmer(sib ~ (1|birth_year) + (1|maximum_age),
                  family = "binomial", data = fdw_2nd)
  summary(sby2.1) # AIC 1037

  # Add sex
  sby2.2 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex,
                  family = "binomial", data = fdw_2nd)
  summary(sby2.2) # AIC 1023

  # Add caging year 2
  sby2.3 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year2.10,
                  family = "binomial", data = fdw_2nd)
  summary(sby2.3) # AIC 900, significant

  # Add year 1 additively
  sby2.4 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 + caging_year2.10,
                  family = "binomial", data = fdw_2nd)
  summary(sby2.4) # AIC 855, both significant

  # Year 1 x Year 2 interaction
  sby2.5 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 * caging_year2.10,
                  family = "binomial", data = fdw_2nd)
  summary(sby2.5) # AIC 852, negative interaction — best model for year 2

  # Check for false convergence alarm
  relgrad <- with(sby2.5@optinfo$derivs, solve(Hessian, gradient))
  max(abs(relgrad)) # 0.0016 — borderline; verify with allFit()

  all_fits <- allFit(sby2.5)
  ss <- summary(all_fits)
  ss$fixef    # Fixed effects consistent across optimizers
  ss$llik     # Log-likelihoods consistent
  ss$which.OK # All 5 optimizers converged

  vif(sby2.5)
  r2(sby2.5)
  # Conditional R2: 0.323
  # Marginal R2:    0.255


# Q1 continued: Effect of time in caging — year 3 ----
# Filter out subjects with SIB onset in year 1b or year 2 (n = 24 removed)

  fdw_3rd <- full_data_wide |>
    mutate(sb_emerged = as.factor(sb_emerged)) |>
    filter(!sb_emerged %in% c("year1b", "year2") | is.na(sb_emerged))

  # Null model — year 3 data
  sby3.1 <- glmer(sib ~ (1|birth_year) + (1|maximum_age),
                  family = "binomial", data = fdw_3rd)
  summary(sby3.1) # AIC 901.8

  # Add sex
  sby3.2 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex,
                  family = "binomial", data = fdw_3rd)
  summary(sby3.2) # AIC 895

  # Add year 1 and 2
  sby3.3 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 + caging_year2.10,
                  family = "binomial", data = fdw_3rd)
  summary(sby3.3) # AIC 774

  # Add year 3
  sby3.5 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex +
                    caging_year1.10 + caging_year2.10 + caging_year3.10,
                  family = "binomial", data = fdw_3rd)
  summary(sby3.5) # AIC 745 — best model for year 3
  vif(sby3.5)
  r2(sby3.5)
  # Conditional R2: 0.303
  # Marginal R2:    0.211

  # AIC comparison across year 3 models
  rbind(
    AIC(sby3.3),   # year 1 + 2:       774.7
    AIC(sby3.5),   # year 1 + 2 + 3:   745.7 BEST MODEL
    AIC(sby3.5a <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10, family = "binomial", data = fdw_3rd)),  # year 1: 805.8
    AIC(sby3.5b <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year2.10, family = "binomial", data = fdw_3rd)),  # year 2: 804.0
    AIC(sby3.5c <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year3.10, family = "binomial", data = fdw_3rd)),  # year 3: 832.3
    AIC(sby3.5d <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 + caging_year3.10, family = "binomial", data = fdw_3rd)), # year 1 + 3: 748.6
    AIC(sby3.5e <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year2.10 + caging_year3.10, family = "binomial", data = fdw_3rd))  # year 2 + 3: 786.0
  )


# Q2: Effect of cagemate type — indoor-reared subjects only ----
# Filter: subjects spending > 180 days in indoor caging during year 1

  fdw_all_caging <- full_data_wide |>
    filter(caging_year1 > 180)
  # n = 849 subjects, 70 with SIB

  # Null model
  cm1 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1),
               family = "binomial", data = fdw_all_caging)
  summary(cm1) # AIC 478

  # Add sex
  cm2 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex,
               family = "binomial", data = fdw_all_caging)
  summary(cm2) # AIC 461

  # Dam only
  cm3 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + dam.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm3) # AIC 461, not significant

  # Dam + adult female
  cm4 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + dam_adult_female.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm4) # AIC 459, trending positive

  # Dam + adult female + infant — best cagemate model
  cm5 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + dam_adult_female_infant.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm5) # AIC 452, significant negative effect

  # Infant only
  cm6 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + infant.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm6) # AIC 462, not significant

  # No cagemate
  cm7 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + no_cagemate.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm7) # AIC 462, not significant

  # Dam (any configuration)
  cm8 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + dam_any.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm8) # AIC 461, not significant (possible ceiling effect)

  # Interaction: sex x dam_adult_female_infant
  cm9 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex * dam_adult_female_infant.10,
               family = "binomial", data = fdw_all_caging)
  summary(cm9) # AIC 453, interaction not significant


# Q3: Effect of sedations during year 1 on SIB ----
# Indoor-reared subjects only (fdw_all_caging, n = 849)

  # Null model
  s1 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1.10),
              family = "binomial", data = fdw_all_caging)
  summary(s1) # AIC 478.8

  # Add sex
  s2 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1.10) + sex,
              family = "binomial", data = fdw_all_caging)
  summary(s2) # AIC 461

  # Add sedations — best model
  s3 <- glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1.10) + sex + sedations_year1,
              family = "binomial", data = fdw_all_caging)
  summary(s3) # AIC 451, sedations significant and positive
  vif(s3)
  r2(s3)
  # Conditional R2: 0.316
  # Marginal R2:    0.101

