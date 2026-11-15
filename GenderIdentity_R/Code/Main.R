#############################################################################################
# Main.R
#
# (i) construct individual-level variables
# (ii) construct ShareMarried, i.e., dependent variable
# (iii) estimate potential wages
# (iv) construct market-level variables (controls + regressors)
# (v) merge (ii) + (iv)
# (vi) run regressions
#
# Input: valuesRaw.csv
#
# Output: regression_table.tex
#
# Last Updated: Nov 15. 11:00 by Woonseo Paek
##############################################################################################

### housekeeping

library(dplyr)
library(tidyr)
library(fixest)
library(vtable)

### load data
dir <- "/Users/paek-woonseo/Desktop/genderidentity2026/USEK_ReplicationFiles"
in_dir<- file.path(dir, "Data", "derived")
out_dir <- file.path(dir, "Output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

raw<- read.csv(file.path(in_dir, "valuesRaw.csv"))


cpi_data <- data.frame(
  year = 2008:2024,
  cpi = c(81.656, 83.906, 86.373, 89.850, 91.815, 93.010, 94.196, 94.861, 95.783, 97.645,
          99.086, 99.466, 100.000, 102.500, 107.720, 111.590, 114.18)
)

raw <- raw %>%
  left_join(cpi_data, by = "year") %>%
  mutate(
    wage_monthly = (as.numeric(wage_monthly) / cpi) * 100
  )

### construct individual-level variables

df_base <- raw %>% 
  mutate(
    wage_monthly = as.numeric(wage_monthly),
    wage_monthly = ifelse(is.na(wage_monthly), 0, wage_monthly), #impute NA of wage_monthly as 0, following Bertrand et al. (2015)
    age = as.numeric(age),
    edu_level = as.character(edu_level),
    age_group = case_when(
      (sex == "여자" & age >= 22 & age <= 31) | (sex == "남자" & age >= 24 & age <= 33) ~ "Age Group 1 (W:22~31, M:24~33)",
      (sex == "여자" & age >= 32 & age <= 41) | (sex == "남자" & age >= 34 & age <= 43) ~ "Age Group 2 (W:32~41, M:34~43)",
      (sex == "여자" & age >= 42 & age <= 51) | (sex == "남자" & age >= 44 & age <= 53) ~ "Age Group 3 (W:42~51, M:44~53)",
      TRUE ~ NA_character_
    ),
    edu_group = case_when(
      grepl("대학원|대학교|4년제|전문대|초급대|2ㆍ3년제|2년·3년제", edu_level) ~ "2_College+",
      TRUE ~ "1_HighSchool_or_Less" 
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  mutate(
    years_of_schooling = case_when(
      grepl("무학", edu_level) ~ 0, grepl("초등학교", edu_level) ~ 6,
      grepl("중학교", edu_level) ~ 9, grepl("고등학교", edu_level) ~ 12,
      grepl("전문대|2ㆍ3년제", edu_level) ~ 14, grepl("대학교|4년제", edu_level) ~ 16,
      grepl("석사", edu_level) ~ 18, grepl("박사", edu_level) ~ 21,
      TRUE ~ 12
    ),
    experience = age - years_of_schooling - 6
  )

### construct ShareMarried_mt (dependent variable)

male_marriage_rates <- df_base %>%
  filter(sex == "남자") %>% #proportion of married *males*, following Bertrand et al. (2015)
  group_by(year, region_do, edu_group, age_group) %>%
  summarise(
    ShareMarried_mt = mean(marry == "유배우", na.rm = TRUE),
    .groups = "drop"
  )


### estimate potential wages (Mincer equation)

df_mincer <- df_base %>% filter(experience >= 0)

training_data  <- df_mincer %>% filter(!is.na(occ_code) & wage_monthly > 0)
predicted_data <- df_mincer 

mincer <- feols(
  log(wage_monthly) ~ experience + I(experience^2) | 
    year + region_do^occ_code + edu_group^occ_code + age_group^occ_code,
  data = training_data,
  weights = ~weight,
  cluster = ~region_do
)
summary(mincer)
coeftable(mincer)

predicted_data$log_pot_wage <- predict(mincer, newdata = predicted_data)

# individual-level final data with potential wages
df_potential_final <- predicted_data %>%
  mutate(
    potential_wage = exp(log_pot_wage)
  ) %>%
  filter(potential_wage > 0 & wage_monthly > 0)

sumtable(df_potential_final, vars = c("potential_wage", "wage_monthly"), digits = 6)

### construct market-level variables

# potential wage distributions by sex within each market

base_df_potential <- df_potential_final %>%
  group_by(year, region_do, edu_group, age_group, sex) %>%
  summarise(pot_wage_list = list(potential_wage), .groups = "drop") %>%
  pivot_wider(
    names_from = sex,
    values_from = pot_wage_list,
    names_prefix = "pot_list_"
  ) %>%
  rename(
    w_pot_wage_list = pot_list_여자,
    m_pot_wage_list = pot_list_남자
  )

# control variables 

controls <- df_potential_final %>%
  mutate(
    years_of_schooling = case_when(
      grepl("무학|안받았음(무학)", as.character(edu_level)) ~ 0,
      grepl("초등학교", as.character(edu_level)) ~ 6,
      grepl("중학교", as.character(edu_level)) ~ 9,
      grepl("고등학교", as.character(edu_level)) ~ 12,
      grepl("전문대(초급대,2ㆍ3년제 대학포함)|전문대|전문대(초급대, 2년·3년제 대학 포함)", as.character(edu_level)) ~ 14,
      grepl("대학교(4년제 대학포함)|대학교(4년제 이상 대학포함)|대학교(4년제 이상 대학 포함)|대학교", as.character(edu_level)) ~ 16,
      grepl("대학원(석사)", as.character(edu_level)) ~ 18,
      grepl("대학원(박사)", as.character(edu_level)) ~ 21,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(age_group), !is.na(years_of_schooling)) %>%
  group_by(year, region_do, edu_group, age_group) %>%
  summarise(
    num_women = sum(sex == "여자", na.rm = TRUE),
    num_men = sum(sex == "남자", na.rm = TRUE),
    
    avg_w_wage = mean(wage_monthly[sex == "여자"], na.rm = TRUE),
    avg_m_wage = mean(wage_monthly[sex == "남자"], na.rm = TRUE),
    
    w_p10 = quantile(wage_monthly[sex == "여자"], 0.1, na.rm = TRUE),
    w_p20 = quantile(wage_monthly[sex == "여자"], 0.2, na.rm = TRUE),
    w_p30 = quantile(wage_monthly[sex == "여자"], 0.3, na.rm = TRUE),
    w_p40 = quantile(wage_monthly[sex == "여자"], 0.4, na.rm = TRUE),
    w_p50 = quantile(wage_monthly[sex == "여자"], 0.5, na.rm = TRUE),
    w_p60 = quantile(wage_monthly[sex == "여자"], 0.6, na.rm = TRUE),
    w_p70 = quantile(wage_monthly[sex == "여자"], 0.7, na.rm = TRUE),
    w_p80 = quantile(wage_monthly[sex == "여자"], 0.8, na.rm = TRUE),
    w_p90 = quantile(wage_monthly[sex == "여자"], 0.9, na.rm = TRUE),
    
    m_p10 = quantile(wage_monthly[sex == "남자"], 0.1, na.rm = TRUE),
    m_p20 = quantile(wage_monthly[sex == "남자"], 0.2, na.rm = TRUE),
    m_p30 = quantile(wage_monthly[sex == "남자"], 0.3, na.rm = TRUE),
    m_p40 = quantile(wage_monthly[sex == "남자"], 0.4, na.rm = TRUE),
    m_p50 = quantile(wage_monthly[sex == "남자"], 0.5, na.rm = TRUE),
    m_p60 = quantile(wage_monthly[sex == "남자"], 0.6, na.rm = TRUE),
    m_p70 = quantile(wage_monthly[sex == "남자"], 0.7, na.rm = TRUE),
    m_p80 = quantile(wage_monthly[sex == "남자"], 0.8, na.rm = TRUE),
    m_p90 = quantile(wage_monthly[sex == "남자"], 0.9, na.rm = TRUE),
    
    sex_ratio = ifelse(num_women == 0, 0, num_men / num_women),
    
    avg_w_schooling = mean(years_of_schooling[sex == "여자"], na.rm = TRUE),
    avg_m_schooling = mean(years_of_schooling[sex == "남자"], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    share_w_income = avg_w_wage / (avg_w_wage + avg_m_wage)
  ) %>%
  #log transformation
  mutate(across(c(avg_w_wage:m_p90), ~log(.), .names = "log_{.col}"))

# simulate PrWomanEarnsMore and GapLower/Higher within each market

set.seed(2026)
n_sim <- 50000
total_markets <- nrow(base_df_potential)

sim_results_pot <- lapply(1:total_markets, function(i) {
  w_vec <- unlist(base_df_potential$w_pot_wage_list[[i]])
  m_vec <- unlist(base_df_potential$m_pot_wage_list[[i]])
  
  if (length(w_vec) == 0 || length(m_vec) == 0) {
    return(c(pr_w_earn_more = NA, gap_if_w_higher = NA, gap_if_w_lower = NA))
  }
  
  w_draw <- sample(w_vec, n_sim, replace = TRUE)
  m_draw <- sample(m_vec, n_sim, replace = TRUE)
  
  diffs <- w_draw - m_draw
  
  pr_val <- mean(diffs > 0, na.rm = TRUE)
  
  pos_diffs <- diffs[diffs > 0]
  gap_h <- if (length(pos_diffs) > 0) mean(pos_diffs, na.rm = TRUE) else 0
  
  neg_diffs <- diffs[diffs <= 0]
  gap_l <- if (length(neg_diffs) > 0) mean(neg_diffs, na.rm = TRUE) else 0
  
  if (i %% 100 == 0) cat(i, "/", total_markets, "completed...\n")
  
  return(c(pr_w_earn_more = pr_val,
           gap_if_w_higher = gap_h,
           gap_if_w_lower = gap_l))
})

results_mat_pot <- do.call(rbind, sim_results_pot)

base_df_potential$pr_w_earn_more  <- results_mat_pot[, "pr_w_earn_more"]
base_df_potential$gap_if_w_higher <- results_mat_pot[, "gap_if_w_higher"]
base_df_potential$gap_if_w_lower  <- results_mat_pot[, "gap_if_w_lower"]

### construct final analysis dataset (simulation results + marriage rates + controls)

final_analysis_df <- base_df_potential %>%
  select(year, region_do, edu_group, age_group,
         pr_w_earn_more, gap_if_w_higher, gap_if_w_lower) %>%
  inner_join(male_marriage_rates, by = c("year", "region_do", "edu_group", "age_group")) %>%
  inner_join(controls, by = c("year", "region_do", "edu_group", "age_group")) %>%
  mutate(
    gap_higher = gap_if_w_higher,
    gap_lower  = gap_if_w_lower,
    market_id = as.factor(paste(region_do, edu_group, age_group, sep = "_"))
  )

nrow(final_analysis_df)
summary(final_analysis_df$pr_w_earn_more)
summary(final_analysis_df$gap_higher)
summary(final_analysis_df$gap_lower)

sumtable(final_analysis_df, vars = c("ShareMarried_mt", "pr_w_earn_more", "gap_if_w_higher", "gap_if_w_lower"))
sd(final_analysis_df$ShareMarried_mt)
sd(final_analysis_df$pr_w_earn_more)
sd(final_analysis_df$gap_higher)
sd(final_analysis_df$gap_lower)

### regression

res1_mean <- feols(
  ShareMarried_mt ~ pr_w_earn_more + gap_lower + gap_higher +
    log_avg_w_wage + log_avg_m_wage +
    log_w_p10 + log_w_p20 + log_w_p30 + log_w_p40 + log_w_p50 +
    log_w_p60 + log_w_p70 + log_w_p80 + log_w_p90 +
    log_m_p10 + log_m_p20 + log_m_p30 + log_m_p40 + log_m_p50 +
    log_m_p60 + log_m_p70 + log_m_p80 + log_m_p90 |
    market_id +
    year +
    year^region_do +
    year^edu_group +
    year^age_group,
  data = final_analysis_df,
  weights = ~num_men,
  cluster = ~region_do
)

res2_mean <- feols(
  ShareMarried_mt ~ pr_w_earn_more + share_w_income + gap_lower + gap_higher +
    log_avg_w_wage + log_avg_m_wage +
    log_w_p10 + log_w_p20 + log_w_p30 + log_w_p40 + log_w_p50 +
    log_w_p60 + log_w_p70 + log_w_p80 + log_w_p90 +
    log_m_p10 + log_m_p20 + log_m_p30 + log_m_p40 + log_m_p50 +
    log_m_p60 + log_m_p70 + log_m_p80 + log_m_p90 |
    market_id +
    year +
    year^region_do +
    year^edu_group +
    year^age_group,
  data = final_analysis_df,
  weights = ~num_men,
  cluster = ~region_do
)

res3_mean <- feols(
  ShareMarried_mt ~ pr_w_earn_more + share_w_income + gap_lower + gap_higher +
    log_avg_w_wage + log_avg_m_wage +
    log_w_p10 + log_w_p20 + log_w_p30 + log_w_p40 + log_w_p50 +
    log_w_p60 + log_w_p70 + log_w_p80 + log_w_p90 +
    log_m_p10 + log_m_p20 + log_m_p30 + log_m_p40 + log_m_p50 +
    log_m_p60 + log_m_p70 + log_m_p80 + log_m_p90 +
    avg_w_schooling + avg_m_schooling +
    sex_ratio |
    market_id +
    year +
    year^region_do +
    year^edu_group +
    year^age_group,
  data = final_analysis_df,
  weights = ~num_men,
  cluster = ~region_do
)


# export main regression table 
etable(res1_mean, res2_mean, res3_mean,
       tex = TRUE,
       file = file.path(out_dir, "regression_table.tex"), replace = TRUE,
       title = "Impact of Gender Norm Challenges and Violations on Marriage Rates",
       label = "tab:main",
       style.tex = style.tex("aer"),
       dict = c(ShareMarried_mt = "ShareMarried",
                pr_w_earn_more  = "PrFemaleEarnsMore",
                gap_lower       = "GapLower",
                gap_higher      = "GapHigher",
                share_w_income  = "Relative Income",
                avg_w_schooling = "Female Average Years of Education",
                avg_m_schooling = "Male Average Years of Education",
                sex_ratio       = "Sex Ratio",
                market_id       = "Marriage market",
                year            = "Year",
                region_do       = "Region",
                edu_group       = "Education group",
                age_group       = "Age group"),
       drop = "%^log_",   # hide income decile/average controls ("%" = match original variable names)
       extralines = list("Controls for deciles and averages of income" = rep("Yes", 3)),
       fitstat = ~ n + r2,
       notes = paste("Each observation represents a marriage market.",
                     "Estimates are obtained using a linear probability model, weighted by the number of men in each marriage market.",
                     "Standard errors clustered at the regional level are reported in parentheses.",
                     "Data Source: Local Area Labour Force Survey (Ministry of Data and Statistics, 2025b)."))

