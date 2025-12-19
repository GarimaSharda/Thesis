############################################################
# Survival analysis of footballer career length
# - Data: premier_all4.csv (Transfermarkt + FBref merge)
# - Steps: load, engineer variables, EDA, KM curves, Cox models
############################################################

#-----------------------------------------------------------
# 0. Packages and reproducibility
#-----------------------------------------------------------

# Load required packages
library(dplyr)
library(ggplot2)
library(readr)
library(survival)
library(survminer)

# Set a seed for any procedures that might use randomness
set.seed(123)

#-----------------------------------------------------------
# 1. Load data
#-----------------------------------------------------------

# Adjust path if needed; here assumed in working directory
df <- read_csv("C:/Users/garim/Downloads/premier_all4.csv",
               show_col_types = FALSE)

# Inspect available columns once (optional)
names(df)
# Check it's a data frame / tibble
str(df)

#-----------------------------------------------------------
# 2. Select and engineer main variables
#   - Keep variables relevant for survival, exposure, and covariates
#   - Create numeric debut age and categorical bands
#   - Create injury bands, national‑team flag, and height bands
#-----------------------------------------------------------

eda <- df %>%
  select(
    player_name.x,
    career_length,
    retired,
    retirement_age,
    retirement_date_clean,
    age, age_current,
    first_tier_debut_age, `first_tier_debut_age.1`,
    debut_age_transfermarket, debut_age, debut_year, debut_season,
    injury_count_total, days_missed_total, games_missed_total, injury_seasons,
    total_matches_club, total_matches_total,
    position_simple, position_clean,
    nation,              # nationality
    height.y,            # height (cm)
    nantional_team       # ever played for national team (Yes/No)
  ) %>%
  mutate(
    # Numeric debut age from text field (e.g. "17 years")
    debut_age_num = as.numeric(gsub("[^0-9.]", "", debut_age)),
    
    # Debut‑age bands for KM and Cox models
    debut_band = cut(
      debut_age_num,
      breaks = c(15, 18, 21, 24, 30),
      include.lowest = TRUE,
      right = FALSE
    ),
    
    # Injury bands based on total injury counts (tertiles)
    inj_band = cut(
      injury_count_total,
      breaks = quantile(
        injury_count_total,
        probs = c(0, 0.33, 0.66, 1),
        na.rm = TRUE
      ),
      include.lowest = TRUE
    ),
    
    # National‑team participation flag
    nat_team_flag = ifelse(nantional_team == "Yes", "Yes", "No"),
    
    # Height bands (tertiles)
    height_group = cut(
      height.y,
      breaks = quantile(
        height.y,
        probs = c(0, 0.33, 0.66, 1),
        na.rm = TRUE
      ),
      include.lowest = TRUE
    )
  )

#-----------------------------------------------------------
# 3. Structure and missingness
#   - Document data structure and missing values
#   - Use these outputs in the Dataset/EDA chapter
#-----------------------------------------------------------

str(eda)
summary(eda)
sapply(eda, function(x) sum(is.na(x)))

#-----------------------------------------------------------
# 4. Consistency checks for time variables
#   - Check that retirement_age ≈ debut_age_num + career_length
#   - Check for non‑positive durations
#-----------------------------------------------------------

# Retirement age ≈ debut_age_num + career_length (sanity check)
eda %>%
  filter(
    !is.na(retirement_age),
    !is.na(debut_age_num),
    !is.na(career_length)
  ) %>%
  mutate(diff_age = retirement_age - (debut_age_num + career_length)) %>%
  summarise(
    n        = n(),
    mean_diff = mean(diff_age),
    sd_diff   = sd(diff_age),
    min_diff  = min(diff_age),
    max_diff  = max(diff_age)
  )

# Non‑positive career length, by retired status
eda %>%
  filter(!is.na(career_length)) %>%
  group_by(retired) %>%
  summarise(
    n        = n(),
    n_nonpos = sum(career_length <= 0),
    min_len  = min(career_length, na.rm = TRUE),
    max_len  = max(career_length, na.rm = TRUE),
    .groups  = "drop"
  )

# Detailed career‑length summary by position
eda %>%
  filter(!is.na(career_length)) %>%
  group_by(position_simple) %>%
  summarise(
    n        = n(),
    mean_len = mean(career_length, na.rm = TRUE),
    sd_len   = sd(career_length, na.rm = TRUE),
    q1       = quantile(career_length, 0.25, na.rm = TRUE),
    median   = median(career_length, na.rm = TRUE),
    q3       = quantile(career_length, 0.75, na.rm = TRUE),
    .groups  = "drop"
  )

# Cross‑tab of position by retired status
table(eda$position_simple, eda$retired)

#-----------------------------------------------------------
# 5. Univariate distributions
#   - Histograms for key numeric variables
#   - Bar plots for categorical covariates
#-----------------------------------------------------------

num_vars <- c(
  "career_length", "retirement_age", "debut_age_num",
  "injury_count_total", "days_missed_total",
  "games_missed_total", "total_matches_club",
  "total_matches_total", "height.y"
)

# Histograms
for (v in num_vars) {
  if (v %in% names(eda)) {
    p <- ggplot(eda, aes_string(x = v)) +
      geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
      theme_minimal() +
      labs(title = v)
    print(p)
  }
}

# Position counts
ggplot(eda, aes(x = position_simple)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Count by position_simple")

# National‑team status counts
ggplot(eda, aes(x = nat_team_flag)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Count by national‑team status")

#-----------------------------------------------------------
# 6. Bivariate relationships: career length vs covariates
#-----------------------------------------------------------

# Career length by position
ggplot(eda, aes(x = position_simple, y = career_length)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Career length by position_simple")

# Career length vs debut age (numeric)
ggplot(eda, aes(x = debut_age_num, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career length vs debut_age", x = "debut_age_num")

# Career length vs injury count
ggplot(eda, aes(x = injury_count_total, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career length vs injury_count_total")

# Career length vs club matches (career exposure)
ggplot(eda, aes(x = total_matches_club, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career length vs total_matches_club")

# Career length vs height
ggplot(eda, aes(x = height.y, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career length vs height (cm)")

# Career length by debut band and position (table summary)
eda %>%
  filter(!is.na(career_length)) %>%
  group_by(debut_band, position_simple) %>%
  summarise(
    n        = n(),
    mean_len = mean(career_length, na.rm = TRUE),
    .groups  = "drop"
  )

#-----------------------------------------------------------
# 7. Event vs censoring structure
#   - Summaries by retired status
#   - Density of career_length for retired vs active
#-----------------------------------------------------------

eda %>%
  filter(!is.na(career_length)) %>%
  group_by(retired) %>%
  summarise(
    n          = n(),
    mean_len   = mean(career_length),
    sd_len     = sd(career_length),
    median_len = median(career_length),
    q1         = quantile(career_length, 0.25),
    q3         = quantile(career_length, 0.75),
    .groups    = "drop"
  )

ggplot(eda, aes(x = career_length,
                colour = factor(retired),
                fill   = factor(retired))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(colour = "retired", fill = "retired",
       title = "Career length by retired status")

#-----------------------------------------------------------
# 8. Correlation matrix for numeric fields
#   - Used in EDA to discuss collinearity
#-----------------------------------------------------------

num_df <- eda %>%
  select(all_of(num_vars)) %>%
  mutate(across(everything(), as.numeric))

cor_mat <- cor(num_df, use = "pairwise.complete.obs")
print(cor_mat)

#-----------------------------------------------------------
# 9. Prepare clean survival dataset
#   - Keep complete cases for key survival variables
#   - Create event indicator
#-----------------------------------------------------------

surv_df <- eda %>%
  filter(
    !is.na(career_length),
    !is.na(retired),
    !is.na(debut_age_num),
    !is.na(position_simple)
  ) %>%
  mutate(
    event = ifelse(retired == "Yes", 1L, 0L)
  )

summary(surv_df$career_length)
table(surv_df$event)
table(surv_df$position_simple, surv_df$event)

#-----------------------------------------------------------
# 10. Kaplan–Meier analyses (unadjusted survival)
#   - Overall, by position, debut age band, injury band,
#     national‑team status, and height group
#-----------------------------------------------------------

surv_obj <- Surv(time = surv_df$career_length,
                 event = surv_df$event)

## 10.1 Overall KM
km_overall <- survfit(surv_obj ~ 1)
ggsurvplot(
  km_overall,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years since top-tier debut)",
  ylab = "Survival probability (still active)",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)

## 10.2 KM by position
km_pos <- survfit(surv_obj ~ position_simple, data = surv_df)
ggsurvplot(
  km_pos,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years)",
  ylab = "Survival probability",
  legend.title = "Position",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)
survdiff(surv_obj ~ position_simple, data = surv_df)

## 10.3 KM by debut age band
km_debut <- survfit(surv_obj ~ debut_band, data = surv_df)
ggsurvplot(
  km_debut,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years)",
  ylab = "Survival probability",
  legend.title = "Debut age band",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)
survdiff(surv_obj ~ debut_band, data = surv_df)

## 10.4 KM by injury band
km_inj <- survfit(surv_obj ~ inj_band, data = surv_df)
ggsurvplot(
  km_inj,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years)",
  ylab = "Survival probability",
  legend.title = "Injury band",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)
survdiff(surv_obj ~ inj_band, data = surv_df)

## 10.5 KM by national‑team status
km_nat <- survfit(Surv(career_length, event) ~ nat_team_flag,
                  data = surv_df)
ggsurvplot(
  km_nat,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years)",
  ylab = "Survival probability",
  legend.title = "National team",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)
# Note: log‑rank may fail if only one group is present after filtering
survdiff(Surv(career_length, event) ~ nat_team_flag, data = surv_df)

## 10.6 KM by height group
km_height <- survfit(surv_obj ~ height_group, data = surv_df)
ggsurvplot(
  km_height,
  data = surv_df,
  conf.int = TRUE,
  xlab = "Career length (years)",
  ylab = "Survival probability",
  legend.title = "Height group",
  ggtheme = theme_minimal(),
  risk.table = TRUE
)
survdiff(surv_obj ~ height_group, data = surv_df)

#-----------------------------------------------------------
# 11. Cox proportional hazards models
#   - 11.1 Baseline Cox model
#   - 11.2 PH diagnostics
#   - 11.3 Stratified Cox model (by position)
#   - Time‑interaction model kept for experimentation only
#-----------------------------------------------------------

# Recreate surv_df with complete cases for Cox covariates
surv_df <- eda %>%
  filter(
    !is.na(career_length),
    !is.na(retired),
    !is.na(debut_age_num),
    !is.na(position_simple),
    !is.na(injury_count_total),
    !is.na(total_matches_club),
    !is.na(height_group)
  ) %>%
  mutate(
    event           = ifelse(retired == "Yes", 1L, 0L),
    position_simple = factor(position_simple),
    debut_band      = factor(debut_band),
    inj_band        = factor(inj_band),
    height_group    = factor(height_group)
  )

## 11.1 Baseline Cox model (no stratification)
cox1 <- coxph(
  Surv(career_length, event) ~
    position_simple + debut_band + inj_band +
    total_matches_club + height_group,
  data = surv_df
)
summary(cox1)

## 11.2 PH diagnostics for baseline Cox
ph_test <- cox.zph(cox1)
print(ph_test)
plot(ph_test)  # Use in thesis appendix to illustrate non‑PH

## 11.3 Stratified Cox model (strata = position)
cox_strat <- coxph(
  Surv(career_length, event) ~
    debut_band + inj_band + total_matches_club + height_group +
    strata(position_simple),
  data = surv_df
)
summary(cox_strat)
cox.zph(cox_strat)

# (Optional) 11.4 Time‑interaction model (exploratory only; not used as main model)
cox_timeint <- coxph(
  Surv(career_length, event) ~
    position_simple + debut_band + inj_band +
    total_matches_club + height_group +
    total_matches_club:log(career_length),
  data = surv_df
)
cox.zph(cox_timeint)

