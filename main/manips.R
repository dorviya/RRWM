
# CanD3 2024 - RRWM exercise
# Yann Dorville

# This scripts simulate a research project
# I load the 2016 Canadian Census of Population, and 
# output some methodologically unsound results for the sake of the exercise

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 0 - HEADER ----
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
rm(list=ls())

library(data.table) # Fast read
library(tidyverse)  # Data wrangling
library(magrittr)   # Pipe operator and aliases
library(xtable)     # Quick LateX tables

setwd("/Users/yann/Documents/ACADEMICS/ESGUQAM/CanD3/RRWM")



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1 - LOAD AND PREPARE DATA ----
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Read PUMF ----
rawdata = fread("data/pumf.csv") 

# Select some variables and recode ----
recoded_data = rawdata %>% 
  mutate(
    # Encode age in a silly way
    age = case_when(
      AGEGRP < 7 ~ NA, # Minors
      AGEGRP %>% between(7, 9) ~ "Young", 
      AGEGRP %>% between(10, 12) ~ "Middle", 
      AGEGRP %>% between(13, 16) ~ "Older", 
      AGEGRP > 17 ~ NA, 
    ),
    # Turn numeric sex into factor
    sex = factor(Sex, levels = c(1, 2), labels = c("Female", "Male")),
    # Encode visible minority as a dummy
    minority = case_when(
      VisMin == 13 ~ "No",
      VisMin < 13 ~ "Yes",
      VisMin > 13 ~ NA
    ),
    # Highest educational attainment
    education = case_when(
      HDGREE == 1 ~ "None",
      HDGREE %>% between(2, 8) ~ "Medium",
      HDGREE %>% between(9, 13) ~ "High",
      HDGREE > 13 ~ NA
    ),
    # Keep income, field of study, occupation, and industry
    income = EmpIn, 
    field_study = CIP2011,
    NOCS, 
    NAICS,
    LFACT,
    .keep = "none"
  )

# Filter unwanted observations ----
trimmed_data = recoded_data %>% 
  filter(
    # Keep only workers employed at work
    LFACT == 1,
    # Values 88 and 99 are NA
    NAICS < 88, NOCS < 88, field_study < 88,
    # Similar for income
    !(income %in% c(88888888, 99999999))) %>% 
  # Ignore rows with NAs (e.g. minors)
  na.omit 


# Apply preprocessing ----
# Build a name mapping for occupations 
nocs_names = tibble(
  NOCS = 1:10,
  NOCS_name = c("Management", 
                "Business & Finance", 
                "Sciences", 
                "Health", 
                "Social and Education", 
                "Arts", 
                "Sales and Services", 
                "Trades and Transport", 
                "Primary Industry", 
                "Manufacturing"))

# For industries
naics_names = tibble(
  NAICS = 1:19,
  NAICS_name = c(
    "Agriculture",
    "Mining & Extraction",
    "Utilities",
    "Construction",
    "Manufacturing",
    "Wholesale",
    "Retail",
    "Transportation",
    "Information and Cultural Industries",
    "Finance",
    "Real Estate",
    "Professional Services",
    "Administrative and Support",
    "Education",
    "Health Care",
    "Arts & Entertainment",
    "Accommodation and Food",
    "Other Services",
    "Public Administration"
  )
)

# For field of study
field_study_names = tibble(
  field_study = 1:13,
  field_study_name = c(
    "Education",
    "Communications",
    "Humanities",
    "Social sciences & Law",
    "Business & Management",
    "Life sciences",
    "Mathematics & Computer sciences",
    "Architecture & engineering",
    "Agriculture",
    "Health",
    "Transportation services",
    "Other",
    "No degree"
  )
)

design_data = trimmed_data %>% 
  # Convert income in thousands
  mutate(income = income / 1000) %>% 
  # Add meaningful occupation names
  left_join(nocs_names) %>% 
  # Add meaningful industry names
  left_join(naics_names) %>% 
  # Add meaningful FOS names
  left_join(field_study_names) %>% 
  # Compute median income by occupation
  group_by(NOCS, sex) %>%
  mutate(median_income = median(income)) 


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2 - SUMMARY STATISTICS ----
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Compute average income by occupation and sex
design_data %>% 
  group_by(sex, NOCS_name) %>% 
  summarise(
    avg = mean(income)) %>% 
  pivot_wider(names_from = sex, values_from = avg) %>% 
  xtable 

design_data %>% 
  group_by(sex, NOCS_name) %>% 
  summarise(
    std = sd(income)) %>% 
  pivot_wider(names_from = sex, values_from = std) %>% 
  xtable 

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 3 - PLOT ----
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

design_data %>% 
  # Base layer
  ggplot(aes(x = income, color = sex, group = sex, fill = sex)) + 
  # Density
  geom_density(adjust = 3, alpha = 0.2) +
  # Median income
  geom_vline(aes(xintercept = median_income, color = sex), alpha = 0.6, linetype = "dashed") +
  # Facet
  facet_wrap(~NOCS_name) + 
  # Style
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw(base_size = 20, base_family = "Times New Roman") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  # Text
  labs(x = "", y = "", fill = "", color = "", 
       title = "Income Distribution",
       subtitle = "by Gender and Occupational Group") + 
  # Zoom-in where there is more density
  xlim(c(0, 2e2))
# Export 
ggsave("doc/income_distr.png", dpi = 200)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 3 - REGRESSION ----
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Silly model
modl = income ~ sex + NAICS_name + NOCS_name + field_study_name + age + minority + education

# Get the results
result = design_data %>% 
  ungroup %>%
  mutate(across(-income, as.factor)) %>%
  lm(modl, data = .) %>% 
  summary
  
result$coefficients


