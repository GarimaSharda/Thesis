############################################################
# Survival analysis of footballer career length (Full EDA)
# - Data: premier_simple.csv (One row per player)
############################################################

#-----------------------------------------------------------
# 0. Packages and Setup
#-----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(survival)
library(survminer)
library(gridExtra) # Useful for arranging plots if installed

set.seed(123)

#-----------------------------------------------------------
# 1. Load Data
#-----------------------------------------------------------
# Ensure correct path or use file.choose()
df <- read_csv("C:/Users/garim/Downloads/premier_simple.csv", show_col_types = FALSE)

#-----------------------------------------------------------
# 2. Data Engineering
#-----------------------------------------------------------
eda <- df %>%
  select(
    player_name = player_name.x,
    career_length,
    retired,
    retirement_age,
    age_current,
    debut_age,
    debut_year,
    injury_count_total,
    days_missed_total,
    games_missed_total,
    total_matches_club,
    total_matches_total,
    position_simple,
    height.y
  ) %>%
  mutate(
    # Clean numeric variables
    debut_age_num = as.numeric(gsub("[^0-9.]", "", debut_age)),
    height_clean = as.numeric(height.y),
    
    # Create Factors / Bands
    debut_band = cut(debut_age_num, breaks = c(15, 18, 21, 24, 30), include.lowest = TRUE, right = FALSE),
    inj_band = cut(injury_count_total, breaks = quantile(injury_count_total, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), include.lowest = TRUE),
    height_group = cut(height_clean, breaks = quantile(height_clean, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), include.lowest = TRUE),
    
    # Clean Retired Status
    retired = ifelse(retired %in% c("Yes", "No"), retired, NA)
  )

#-----------------------------------------------------------
# 3. Univariate Distributions (Restored from Original)
#-----------------------------------------------------------

# 3.1 Numeric Histograms
num_vars <- c("career_length", "retirement_age", "debut_age_num", 
              "injury_count_total", "total_matches_club", "height_clean")

for (v in num_vars) {
  if (v %in% names(eda)) {
    p <- ggplot(eda, aes_string(x = v)) +
      geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Distribution of", v))
    print(p)
  }
}

# 3.2 Categorical Bar Charts
ggplot(eda, aes(x = position_simple)) +
  geom_bar(fill = "firebrick", alpha=0.8) +
  theme_minimal() +
  labs(title = "Count by Position")

#-----------------------------------------------------------
# 4. Bivariate Relationships (Restored from Original)
#-----------------------------------------------------------

# 4.1 Career Length vs Position
ggplot(eda, aes(x = position_simple, y = career_length)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Career Length by Position")

# 4.2 Career Length vs Debut Age (Scatter + Smooth)
ggplot(eda, aes(x = debut_age_num, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career Length vs Debut Age")

# 4.3 Career Length vs Injuries
ggplot(eda, aes(x = injury_count_total, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "darkgreen") +
  theme_minimal() +
  labs(title = "Career Length vs Total Injuries")

# 4.4 Career Length vs Height
ggplot(eda, aes(x = height_clean, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "purple") +
  theme_minimal() +
  labs(title = "Career Length vs Height (cm)")

#-----------------------------------------------------------
# 5. Density Plots (Active vs Retired)
#-----------------------------------------------------------
ggplot(eda, aes(x = career_length, colour = retired, fill = retired)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Career Length Density: Retired vs Active")

#-----------------------------------------------------------
# 6. Kaplan-Meier Curves (Full Suite)
#-----------------------------------------------------------

# Prepare Survival Data
surv_df <- eda %>%
  filter(!is.na(career_length), !is.na(retired), !is.na(debut_age_num), 
         !is.na(position_simple), !is.na(height_group)) %>%
  mutate(event = ifelse(retired == "Yes", 1L, 0L))

surv_obj <- Surv(time = surv_df$career_length, event = surv_df$event)

# 6.1 Overall
km_overall <- survfit(surv_obj ~ 1)
ggsurvplot(km_overall, data = surv_df, risk.table = TRUE, title = "Overall Survival")

# 6.2 By Position
km_pos <- survfit(surv_obj ~ position_simple, data = surv_df)
ggsurvplot(km_pos, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Position", legend.title = "Pos")

# 6.3 By Debut Age Band
km_debut <- survfit(surv_obj ~ debut_band, data = surv_df)
ggsurvplot(km_debut, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Debut Age", legend.title = "Age Band")

# 6.4 By Injury Band
km_inj <- survfit(surv_obj ~ inj_band, data = surv_df)
ggsurvplot(km_inj, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Injury Tier", legend.title = "Injuries")

# 6.5 By Height Group
km_height <- survfit(surv_obj ~ height_group, data = surv_df)
ggsurvplot(km_height, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Height Group", legend.title = "Height")

#-----------------------------------------------------------
# 7. Cox Models (Restored)
#-----------------------------------------------------------

# Baseline
cox1 <- coxph(Surv(career_length, event) ~ position_simple + debut_band + inj_band + 
                total_matches_club + height_group, data = surv_df)
summary(cox1)

# Stratified
cox_strat <- coxph(Surv(career_length, event) ~ debut_band + inj_band + 
                     total_matches_club + height_group + strata(position_simple), 
                   data = surv_df)
summary(cox_strat)

# Check Assumptions
cox.zph(cox_strat)
