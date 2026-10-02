# ################################################################# #
### Counting the purchasing milk preference consumers            ####
# ################################################################# #
# Agnes af Sandeberg 4 February 2026
# R version 4.4.0 
#AIM, we could present: what percent in each (half and full gallon) purchase and/or consume plant-based milk;  
#how often they purchase each of the types of milk; how they consume each type of milk; 
#when they consume each type of milk; percent of households with vegans and dairy allergies.  
#A table that shows ever buyer and linkage to environmental concerns.

# 1. read packages
rm(list = ls())
# List of required packages
required_packages=c("data.table", "plyr", "dplyr", "lubridate", "support.CEs", "readxl", "xtable", 
                    "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "survival", "stats", "haven", 
                    "knitr", "kableExtra", "stringr", "tidyr", "ggplot2", "broom")
# Install missing packages and load them
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg) # Install if not already installed
  }
  library(pkg, character.only = TRUE) # Load the package
}

# 2. Set working directory
path <- Sys.getenv("PWD")
setwd("C:/Users/asor0002/git/DCE_data_management")
output <- "Analysis/Output/Tables/"


# 3. load the data
also_protein_sample <- read_dta("Analysis/Input/DCE_ready_for_analysis.dta")
full_sample <- also_protein_sample %>%
  filter(protein_treat != 1)



#4. Define subsamples which are the different milk buyers (ever) separated by gallon and half-gallon
# Create dummies where ever purchasers 1,2,3, 4,5 are coded as 1 and 6 are coded as zero
# 1= daily , 2=multiple times per week, 3=about once a week, 4= A couple of times per month, 5= Less than once per month 
# Create a binary matrix of milk buyers with gallon info
# 1. Set working directory
# Get most frequent (lowest) buyer value per ID

# 2. For each ID, get the lowest value for each buyer variable and their gallon size
milk_freq <- full_sample %>%
  group_by(ID) %>%
  summarise(
    dairy_score = min(dairy_buyer, na.rm = TRUE),
    almond_score = min(almond_buyer, na.rm = TRUE),
    oat_score = min(oat_buyer, na.rm = TRUE),
    soy_score = min(soy_buyer, na.rm = TRUE),
    gallon = first(gallon),  # assuming gallon is consistent per ID
    .groups = "drop"
  )

# 3. Find the minimum score across all milk types for each ID
milk_freq <- milk_freq %>%
  rowwise() %>%
  mutate(
    min_score = min(c(dairy_score, almond_score, oat_score, soy_score), na.rm = TRUE),
    dairy = dairy_score == min_score,
    almond = almond_score == min_score,
    oat = oat_score == min_score,
    soy = soy_score == min_score
  ) %>%
  ungroup()

# 4. Create a combination label based on the most frequently purchased milk types
milk_freq <- milk_freq %>%
  rowwise() %>%
  mutate(
    combo = paste0(sort(c(
      if (dairy) "Dairy" else NULL,
      if (almond) "Almond" else NULL,
      if (oat) "Oat" else NULL,
      if (soy) "Soy" else NULL
    )), collapse = ""),
    size = ifelse(gallon == 1, "Gallon", "Half-Gallon")
  ) %>%
  ungroup()

# 5. Count number of unique IDs per combination and size
combo_summary <- milk_freq %>%
  count(size, combo, name = "Number of People") %>%
  arrange(size, desc(`Number of People`))

# 6. Optional: pivot to wide format (each combo as a column)
combo_wide <- combo_summary %>%
  pivot_wider(names_from = combo, values_from = `Number of People`, values_fill = 0)

# View the result
print(combo_summary)

#######################################################################################
#Calculate the frequency of preference for one or several milk types
#######################################################################################

#Count the number of ties for most preferred
milk_freq <- milk_freq %>%
  rowwise() %>%
  mutate(
    n_pref = sum(c(dairy, almond, oat, soy))
  ) %>%
  ungroup()

#Calculate precentages by package size
pref_share <- milk_freq %>%
  count(size, n_pref, name = "N") %>%
  group_by(size) %>%
  mutate(
    percent = round(100 * N / sum(N), 1)
  ) %>%
  ungroup()

#Set it up
pref_share <- pref_share %>%
  mutate(
    preference_type = case_when(
      n_pref == 1 ~ "Single preferred milk",
      n_pref == 2 ~ "Two equally preferred milks",
      n_pref == 3 ~ "Three equally preferred milks",
      n_pref == 4 ~ "Four equally preferred milks"
    )
  )

#######################################################################################
#The percentage that has dairy among their most preferred and the percentage 
#that does NOT have dairy among their most preferred 
#######################################################################################
milk_freq <- milk_freq %>%
  mutate(
    dairy_pref = ifelse(dairy, "Dairy preferred", "Non-dairy preferred")
  )

dairy_share <- milk_freq %>%
  count(size, dairy_pref, name = "N") %>%
  group_by(size) %>%
  mutate(
    percent = round(100 * N / sum(N), 1)
  ) %>%
  ungroup()

dairy_table <- dairy_share %>%
  select(size, dairy_pref, percent) %>%
  pivot_wider(
    names_from = dairy_pref,
    values_from = percent
  )

