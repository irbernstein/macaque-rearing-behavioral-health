# Publication ready plots and tables ####


# libraries -----
library(tidyverse)
library(broom.mixed)
library(lme4)
library(patchwork)
library(ggeffects)
library(viridis)


# List of plots ----
# 1. Descriptive, bar plot - number of SB cases per subjects per birth year
# 2. Descriptive, box/jitter plot - days in caging per sex per year
# 3. Descriptive, bar plot - count of subjects who spent time in each cagemate combo
# 4. Model, predicted values - caging
#   a. cage time model years 1, 2 and 3
#   b. cage time different trajectories - no-interaction model
#   c. cage time different trajectories - interaction model
# 5. Model, predicted values - cagemate combo time
# 6. Model, predicted values - sedations
# 7. Model, predicted values - comprehensive model

# List of tables ----
# 1. Number of males and females in each data subset
# 2. Model results - caging
# 3. Model results - cagemates
# 4. Model results - sedations
# 5. Model results - comprehensive


# Bring in datasets ----
# full dataset wide
full_data_wide <- read.csv("data/full_data_wide.csv")

# full dataset long
data_long <- read.csv("data/full_data_long.csv")

## Subset wide data ---- 
# sb emerged after year 1
fdw_2nd <- full_data_wide |> 
  mutate(sb_emerged = as.factor(sb_emerged)) |> 
  filter(sb_emerged != "year1b" | is.na(sb_emerged)) |> 
  filter(maximum_age >= 3)

# sb emerged after year 2
fdw_3rd <- full_data_wide |> 
  mutate(sb_emerged = as.factor(sb_emerged), sex=as.factor(sex)) |> 
  filter(!sb_emerged %in% c("year1b", "year2") | is.na(sb_emerged)) |> 
  filter(maximum_age >= 4)

# sb emerged after year 3
fdw_all_caging <-  full_data_wide |> 
  filter(caging_year1 > 180) |> 
  mutate(sex = as.factor(sex))


# Bring in models ----

## Caging models ----
# All years, no interactions - sby3.5
sby3.5 = glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 + caging_year2.10
               + caging_year3.10,  family="binomial", data=fdw_3rd) 
summary(sby3.5)

# All years with interactions - sby3.5i
sby3.5i = glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex * caging_year1.10 + sex * caging_year2.10
                + sex * caging_year3.10,  family="binomial", data=fdw_3rd)
summary(sby3.5i)


## Cagemate model ----
# Dam, adult female, infant - cm5
cm5 = glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1) + sex + dam_adultfemale_infant.10, family="binomial", data=fdw_all_caging)
summary(cm5)

## Sedations model ----
# Sedations - s3
s3 = glmer(sib ~ (1|birth_year) + (1|maximum_age) + (1|caging_year1.10) + sex + sedations_year1, family="binomial", data=fdw_all_caging)
summary(s3)

## Comprehensive model ----
# Caging, cagemates, sedations - all2
all2 = glmer(sib ~ (1|birth_year) + (1|maximum_age) + sex + caging_year1.10 + dam_adultfemale_infant.10 + sedations_year1, family="binomial", data=fdw_all_caging)
summary(all2)

# Plot themes ----

okabe_ito <- c(
  orange      = "#E69F00",
  sky_blue    = "#56B4E9",
  bluish_green= "#009E73",
  yellow      = "#F0E442",
  blue        = "#0072B2",
  vermillion  = "#D55E00",
  reddish_purple = "#CC79A7",
  black       = "#000000"
)

scale_color_manual(values = okabe_ito)
scale_fill_manual(values = okabe_ito)


# Descriptive plots ----
## SB cases per birth year ----
# ADD POSITION DODGE
sb.by.yob.barplot <- data_long %>% 
  mutate(sib_type = case_when(
    sib_type == "self biting" ~ "Self biters",
    is.na(sib_type)           ~ "Non self-biters",
    TRUE                      ~ as.character(sib_type)
  )) %>% 
  group_by(birth_year, sib_type) %>% 
  summarize(n_animal = n_distinct(animal_id)) %>% 
  ggplot(aes(birth_year, n_animal, fill = sib_type)) +
  geom_col(position = position_dodge()) +
  scale_x_continuous(breaks = seq(min(data_long$birth_year), max(data_long$birth_year), by = 1)) +
  scale_fill_manual(values = c("Self biters" = "#E69F00", "Non self-biters" = "#009E73")) +
  labs(
    y = "Count of subjects",
    x = "Birth Year",
    fill = "SB Status"
  ) +
  theme_classic()

sb.by.yob.barplot

## Days in caging per sex per year ----

days.caged.boxplot <- data_long %>% 
  mutate(year = paste("Year", year)) %>% 
  ggplot(aes(sex, caging_days)) +
  geom_jitter(alpha = 0.09, size = .8, width = .38, shape = 16) +
  geom_boxplot(color = "#0072B2", fill = "#0072B2", alpha = .4) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#D55E00", position = position_dodge(.75)) +
  facet_wrap(~year) +
  labs(
    y = "Days in Caging",
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_blank()
  )

days.caged.boxplot

## Animals that spent at least one day with each cagemate combo ----

cagemate.combo.barplot <- data_long %>%
  filter(year == 1,
         days_with_combo > 1,
         caging_days > 180) %>% 
  group_by(cagemate_combo) %>%
  summarise(n_animals = n_distinct(animal_id)) %>% 
  mutate(cagemate_combo = case_when(
    cagemate_combo == "no_cagemate" ~ "no cagemate",
    TRUE ~ str_replace_all(cagemate_combo, "_", ", "))) %>% 
  ggplot(aes(fct_reorder(cagemate_combo, n_animals), n_animals)) +
  geom_col() +
  geom_text(aes(label = n_animals),
            position = position_dodge(width = 0.9),
            hjust = -0.2,
            size = 3) +
  coord_flip() +
  labs(
    y = "Count of Subjects",
    x = "Cagemate Combination",
  ) +
  theme_classic()

cagemate.combo.barplot


# Predictive values plots ----

## Cage time plots ----

### sby3.5 - no interaction with sex, held at mean ----
# predict across each caging-year variable separately,
# holding the other two at their mean
pred_y1 <- ggpredict(sby3.5, terms = c("caging_year1.10 [all]", "sex"))
pred_y2 <- ggpredict(sby3.5, terms = c("caging_year2.10 [all]", "sex"))
pred_y3 <- ggpredict(sby3.5, terms = c("caging_year3.10 [all]", "sex"))

pred_y1 <- as.data.frame(pred_y1)
pred_y2 <- as.data.frame(pred_y2)
pred_y3 <- as.data.frame(pred_y3)

# label each with which year it represents, and back-transform x to original scale
pred_y1$year <- "Year 1"
pred_y2$year <- "Year 2"
pred_y3$year <- "Year 3"

pred_all <- bind_rows(pred_y1, pred_y2, pred_y3) %>%
  mutate(x_original = x * 10)

# plot all three curves faceted by year

sby3.5.pred.plot <- ggplot(pred_all, aes(x = x_original, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~ year) +
  labs(x = "Days in caging", 
       y = "Predicted probability of self biting", 
       color = "Sex",
       fill = "Sex") +
  theme_classic()

sby3.5.pred.plot

### sby3.5i - with interaction with sex, held at mean ----
# predict across each caging-year variable separately,
# holding the other two at their mean
pred_y1i <- ggpredict(sby3.5i, terms = c("caging_year1.10 [all]", "sex"))
pred_y2i <- ggpredict(sby3.5i, terms = c("caging_year2.10 [all]", "sex"))
pred_y3i <- ggpredict(sby3.5i, terms = c("caging_year3.10 [all]", "sex"))

pred_y1i <- as.data.frame(pred_y1i)
pred_y2i <- as.data.frame(pred_y2i)
pred_y3i <- as.data.frame(pred_y3i)

# label each with which year it represents, and back-transform x to original scale
pred_y1i$year <- "Year 1"
pred_y2i$year <- "Year 2"
pred_y3i$year <- "Year 3"

pred_alli <- bind_rows(pred_y1i, pred_y2i, pred_y3i) %>%
  mutate(x_original = x * 10)

# plot all three curves faceted by year
ggplot(pred_alli, aes(x = x_original, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  facet_wrap(~ year) +
  labs(x = "Days in caging", 
       y = "Predicted probability of self biting") +
  theme_classic()


### Scenario curves: no interaction model sby3.5 ----
#### year1 cage, year2 group ----

pred_y3_cage_group <- ggpredict(sby3.5, 
                                terms = c("caging_year3.10 [all]"),
                                condition = c(caging_year1.10 = 36.5, 
                                              caging_year2.10 = 0))

pred_y3_cage_group <- as.data.frame(pred_y3_cage_group)
pred_y3_cage_group$x_original <- pred_y3_cage_group$x * 10

ggplot(pred_y3_cage_group, aes(x = x_original, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
  ) +
  theme_classic()


#### year1 cage, year2 cage ----

pred_y3_cage_cage <- ggpredict(sby3.5, 
                               terms = c("caging_year3.10 [all]"),
                               condition = c(caging_year1.10 = 36.5, 
                                             caging_year2.10 = 36.5))

pred_y3_cage_cage <- as.data.frame(pred_y3_cage_cage)
pred_y3_cage_cage$x_original <- pred_y3_cage_cage$x * 10

ggplot(pred_y3_cage_cage, aes(x = x_original, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
  ) +
  theme_classic()

#### year1 group, year2 group ----
pred_y3_group_group <- ggpredict(sby3.5, 
                                 terms = c("caging_year3.10 [all]"),
                                 condition = c(caging_year1.10 = 0, 
                                               caging_year2.10 = 0))

pred_y3_group_group <- as.data.frame(pred_y3_group_group)
pred_y3_group_group$x_original <- pred_y3_group_group$x * 10

ggplot(pred_y3_group_group, aes(x = x_original, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
  ) +
  theme_classic()

#### year1 group, year2 cage ----
pred_y3_group_cage <- ggpredict(sby3.5, 
                                terms = c("caging_year3.10 [all]"),
                                condition = c(caging_year1.10 = 0, 
                                              caging_year2.10 = 36.5))

pred_y3_group_cage <- as.data.frame(pred_y3_group_cage)
pred_y3_group_cage$x_original <- pred_y3_group_cage$x * 10

ggplot(pred_y3_group_cage, aes(x = x_original, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
  ) +
  theme_classic()

#### Scenario curves combined: no-interaction model ----

pred_y3_cage_group$scenario  <- "Year1 caged, Year2 group"
pred_y3_cage_cage$scenario   <- "Year1 caged, Year2 caged"
pred_y3_group_group$scenario <- "Year1 group, Year2 group"
pred_y3_group_cage$scenario  <- "Year1 group, Year2 caged"

pred_all_scenarios <- bind_rows(pred_y3_cage_group, pred_y3_cage_cage,
                                pred_y3_group_group, pred_y3_group_cage)

okabe_ito <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73")

scenario.pred.plot <- ggplot(pred_all_scenarios, aes(x = x_original, y = predicted, 
                               color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  ylim(0, .7) +
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Scenario", fill = "Scenario"
  ) +
  theme_classic(base_size = 12)

scenario.pred.plot

### Scenario curves: interaction model sby3.5i ----
#### year1 cage, year2 group ----

pred_y3i_cage_group <- ggpredict(sby3.5i, 
                                 terms = c("caging_year3.10 [all]", "sex"),
                                 condition = c(caging_year1.10 = 36.5, 
                                               caging_year2.10 = 0))

pred_y3i_cage_group <- as.data.frame(pred_y3i_cage_group)
pred_y3i_cage_group$x_original <- pred_y3i_cage_group$x * 10

ggplot(pred_y3i_cage_group, aes(x = x_original, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Sex", fill = "Sex"
  ) +
  theme_classic()


#### year1 cage, year2 cage ----

pred_y3i_cage_cage <- ggpredict(sby3.5i, 
                                terms = c("caging_year3.10 [all]", "sex"),
                                condition = c(caging_year1.10 = 36.5, 
                                              caging_year2.10 = 36.5))

pred_y3i_cage_cage <- as.data.frame(pred_y3i_cage_cage)
pred_y3i_cage_cage$x_original <- pred_y3i_cage_cage$x * 10

ggplot(pred_y3_cage_cage, aes(x = x_original, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Sex", fill = "Sex"
  ) +
  theme_classic()

#### year1 group, year2 group ----
pred_y3i_group_group <- ggpredict(sby3.5i, 
                                  terms = c("caging_year3.10 [all]", "sex"),
                                  condition = c(caging_year1.10 = 0, 
                                                caging_year2.10 = 0))

pred_y3i_group_group <- as.data.frame(pred_y3i_group_group)
pred_y3i_group_group$x_original <- pred_y3i_group_group$x * 10

ggplot(pred_y3i_group_group, aes(x = x_original, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Sex", fill = "Sex"
  ) +
  theme_classic()

#### year1 group, year2 cage ----
pred_y3i_group_cage <- ggpredict(sby3.5i, 
                                 terms = c("caging_year3.10 [all]", "sex"),
                                 condition = c(caging_year1.10 = 0, 
                                               caging_year2.10 = 36.5))

pred_y3i_group_cage <- as.data.frame(pred_y3i_group_cage)
pred_y3i_group_cage$x_original <- pred_y3i_group_cage$x * 10

ggplot(pred_y3i_group_cage, aes(x = x_original, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  ylim(0, .7) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Sex", fill = "Sex"
  ) +
  theme_classic()

#### Scenario curves combined: interaction model ----

pred_y3i_cage_group$scenario  <- "Year1 caged, Year2 group"
pred_y3i_cage_cage$scenario   <- "Year1 caged, Year2 caged"
pred_y3i_group_group$scenario <- "Year1 group, Year2 group"
pred_y3i_group_cage$scenario  <- "Year1 group, Year2 caged"

pred_alli_scenarios <- bind_rows(pred_y3i_cage_group, pred_y3i_cage_cage,
                                 pred_y3i_group_group, pred_y3i_group_cage)

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00")

ggplot(pred_alli_scenarios, aes(x = x_original, y = predicted, 
                                color = scenario, fill = scenario)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  facet_wrap(~ group) +
  ylim(0, .7) +
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  labs(
    x = "Days in caging, Year 3",
    y = "Predicted probability of self biting",
    color = "Scenario", fill = "Scenario"
  ) +
  theme_classic(base_size = 12)





## Cagemate plot ----
pred.cagemate <- ggpredict(cm5, terms = c("dam_adultfemale_infant.10 [all]", "sex"))

# back-transform x to original (untransformed) scale
pred.cagemate$x_original <- pred.cagemate$x * 10

cagemate.pred.plot <- ggplot(pred.cagemate, aes(x = x_original, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(x = "Days with cagemate combo: dam + adult female + infant", 
       y = "Predicted probability of self biting",
       color = "Sex",
       fill = "Sex") +
  theme_classic()

cagemate.pred.plot

## Sedations plot ----
pred.sedations <- ggpredict(s3, terms = c("sedations_year1[all]", "sex"))

sedations.pred.plot <- ggplot(pred.sedations, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(x = "Number of sedations in year 1",
       y = "Predicted probability of self biting",
       color = "Sex",
       fill = "Sex") +
  theme_classic()

sedations.pred.plot

## Combined year1 prediction plots ----

single_color <- "#CC79A7"

# Plot 1: Year 1 only, from sby3.5
pred_y1.combo <- ggpredict(sby3.5, terms = "caging_year1.10 [all]")
pred_y1.combo <- as.data.frame(pred_y1.combo)
pred_y1.combo$x_original <- pred_y1.combo$x * 10

plot1 <- ggplot(pred_y1.combo, aes(x = x_original, y = predicted)) +
  geom_line(color = single_color) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = single_color) +
  labs(x = "Days in caging, Year 1",
       y = "Predicted probability of self biting") +
  theme_classic()

# Plot 2: cagemate combo, from cm5
pred.cagemate.combo <- ggpredict(cm5, terms = "dam_adultfemale_infant.10 [all]")
pred.cagemate.combo <- as.data.frame(pred.cagemate.combo)
pred.cagemate.combo$x_original <- pred.cagemate.combo$x * 10

plot2 <- ggplot(pred.cagemate.combo, aes(x = x_original, y = predicted)) +
  geom_line(color = single_color) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = single_color) +
  labs(x = "Days with cagemate combo: dam + adult female + infant",
       y = "Predicted probability of self biting") +
  theme_classic()

# Plot 3: sedations, from s3
pred.sedations.combo <- ggpredict(s3, terms = "sedations_year1[all]")
pred.sedations.combo <- as.data.frame(pred.sedations.combo)

plot3 <- ggplot(pred.sedations.combo, aes(x = x, y = predicted)) +
  geom_line(color = single_color) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = single_color) +
  labs(x = "Number of sedations in Year 1",
       y = "Predicted probability of self biting") +
  theme_classic()

# combine, side by side (no legend needed now, since there's no group variable)
combined.model.plot <- plot1 + 
  (plot2 + theme(axis.title.y = element_blank())) + 
  (plot3 + theme(axis.title.y = element_blank())) +
  plot_layout(ncol = 3) &
  ylim(0, 0.16)

combined.model.plot

# Export plots ----

# output folder
output_dir <- "figures/"

## collect plot objects - not including combined plot due to width requirements
plot_list <- list(
  sb.by.yob.barplot        = sb.by.yob.barplot,
  days.caged.boxplot    = days.caged.boxplot,
  cagemate.combo.barplot = cagemate.combo.barplot,
  sby3.5.pred.plot    = sby3.5.pred.plot,
  scenario.pred.plot     = scenario.pred.plot,
  cagemate.pred.plot     = cagemate.pred.plot,
  sedations.pred.plot    = sedations.pred.plot
)

## loop and save each with its list name as filename
for (plot_name in names(plot_list)) {
  ggsave(
    filename = paste0(output_dir, plot_name, ".tiff"),
    plot = plot_list[[plot_name]],
    width = 8, height = 5, units = "in",
    dpi = 300
  )
}

## Export combined panel plot
ggsave(
  filename = file.path(output_dir, "combined.model.plot.tiff"),
  plot = combined.model.plot,
  width = 14, height = 5, units = "in",
  dpi = 300
)
