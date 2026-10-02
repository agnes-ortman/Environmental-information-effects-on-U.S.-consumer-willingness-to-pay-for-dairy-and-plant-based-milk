# ################################################################# #
###                  Visualizing milk preferences                ####
# ################################################################# #
# Agnes af Sandeberg May 29th 2025
# R version 4.5.1

# 1. read packages
rm(list = ls())
# List of required packages
required_packages=c("data.table", "plyr", "dplyr", "lubridate", "support.CEs", "readxl", "xtable", 
                    "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "survival", "stats", "haven", 
                    "knitr", "kableExtra", "apollo", "stringr", "tidyr", "ggplot2")
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
output <- "Analysis/Output/Plots/"


# 3. load the data
full_sample <- read_dta("Analysis/Input/DCE_ready_for_analysis.dta")



#4. Ensure numeric conversion for Stata-labelled variables
buyer_vars <- c("almond_buyer", "dairy_buyer", "oat_buyer", "soy_buyer", "other_buyer")
full_sample[buyer_vars] <- lapply(full_sample[buyer_vars], function(x) as.numeric(as.character(x)))

#5. Preserve original frequency values
full_sample <- full_sample %>%
  mutate(
    almond_freq = almond_buyer,
    dairy_freq  = dairy_buyer,
    oat_freq    = oat_buyer,
    soy_freq    = soy_buyer,
    other_freq  = other_buyer
  )

#6. Create often buyer dummy BEFORE collapsing
full_sample <- full_sample %>%
  mutate(
    almond_often_buyer = ifelse(almond_freq %in% 1:3, 1, 0),
    dairy_often_buyer  = ifelse(dairy_freq %in% 1:3, 1, 0),
    oat_often_buyer    = ifelse(oat_freq %in% 1:3, 1, 0),
    soy_often_buyer    = ifelse(soy_freq %in% 1:3, 1, 0)
  )

# Create any buyer dummy (1–5 = buyer, 6 = never)
full_sample <- full_sample %>%
  mutate(
    almond_buyer = ifelse(almond_freq == 6, 0, 1),
    dairy_buyer  = ifelse(dairy_freq == 6, 0, 1),
    oat_buyer    = ifelse(oat_freq == 6, 0, 1),
    soy_buyer    = ifelse(soy_freq == 6, 0, 1),
    other_buyer  = ifelse(other_freq == 6, 0, 1)
  )


# 7. get my gallon and half-gallon subsets
gallon_full <- subset(full_sample, gallon == 1)
halfgal_full <- subset(full_sample, gallon == 0)


# For gallon_full
gallon_summary <- gallon_full |>
  dplyr::group_by(ID) |>
  dplyr::summarise(
    almond = max(almond_buyer, na.rm = TRUE),
    dairy  = max(dairy_buyer, na.rm = TRUE),
    oat    = max(oat_buyer, na.rm = TRUE),
    soy    = max(soy_buyer, na.rm = TRUE),
    almond_often = max(almond_often_buyer, na.rm = TRUE),
    dairy_often  = max(dairy_often_buyer, na.rm = TRUE),
    oat_often    = max(oat_often_buyer, na.rm = TRUE),
    soy_often    = max(soy_often_buyer, na.rm = TRUE)
  ) |>
  dplyr::summarise(
    almond_share = mean(almond),
    dairy_share  = mean(dairy),
    oat_share    = mean(oat),
    soy_share    = mean(soy),
    almond_often_share = mean(almond_often),
    dairy_often_share  = mean(dairy_often),
    oat_often_share    = mean(oat_often),
    soy_often_share    = mean(soy_often)
  )

# For halfgal_full
halfgal_summary <- halfgal_full |>
  dplyr::group_by(ID) |>
  dplyr::summarise(
    almond = max(almond_buyer, na.rm = TRUE),
    dairy  = max(dairy_buyer, na.rm = TRUE),
    oat    = max(oat_buyer, na.rm = TRUE),
    soy    = max(soy_buyer, na.rm = TRUE),
    almond_often = max(almond_often_buyer, na.rm = TRUE),
    dairy_often  = max(dairy_often_buyer, na.rm = TRUE),
    oat_often    = max(oat_often_buyer, na.rm = TRUE),
    soy_often    = max(soy_often_buyer, na.rm = TRUE)
  ) |>
  dplyr::summarise(
    almond_share = mean(almond),
    dairy_share  = mean(dairy),
    oat_share    = mean(oat),
    soy_share    = mean(soy),
    almond_often_share = mean(almond_often),
    dairy_often_share  = mean(dairy_often),
    oat_often_share    = mean(oat_often),
    soy_often_share    = mean(soy_often)
  )


# ################################################################################################ #
### Plot the distribution of combinations of milk that each consumer ever purchase               ####
# ################################################################################################ #

# Function to generate combination labels
get_combination_label <- function(row) {
  milks <- c("almond", "dairy", "oat", "soy")
  purchased <- milks[as.logical(unlist(row))]
  if (length(purchased) == 0) return("none")
  paste(sort(purchased), collapse = "_")
}

# Function to create and save plot
create_plot <- function(data, title, filename) {
  # Deduplicate by ID and keep max value per buyer type
  data_unique <- data %>%
    group_by(ID) %>%
    summarise(across(ends_with("_buyer"), ~ max(.x, na.rm = TRUE)), .groups = "drop")
  
  # Generate combination labels
  buyer_cols <- c("almond_buyer", "dairy_buyer", "oat_buyer", "soy_buyer")
  data_unique$combination <- apply(data_unique[, buyer_cols], 1, get_combination_label)
  
  # Count combinations
  summary <- data_unique %>%
    group_by(combination) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  # Plot
  plot <- ggplot(summary, aes(x = reorder(combination, -count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = count), vjust = -0.5, size = 4) +
    labs(title = title, x = "Milk combination (ever) purchased", y = "Number of consumers") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks = element_line(color = "black")
    )

  
  # Save plot
  ggsave(filename, plot, width = 10, height = 6)
}

# Apply to gallon and half-gallon subsets
create_plot(gallon_full, "Distribution of milk buyers (Gallon)", paste0(output, "gallon_milk_distr.png"))
create_plot(halfgal_full, "Distribution of milk buyers (Half-Gallon)", paste0(output, "halfgal_milk_distr.png"))


# ################################################################################################ #
### Plot the distribution of combinations of frequently purchased milks                          ####
# ################################################################################################ #

# Function to generate combination labels for "often" buyers
get_combination_label <- function(row) {
  milks <- c("almond", "dairy", "oat", "soy")
  purchased <- milks[as.logical(unlist(row))]
  if (length(purchased) == 0) return("none")
  paste(sort(purchased), collapse = "_")
}

# Function to create and save plot
create_plot <- function(data, title, filename) {
  # Deduplicate by ID and keep max value per buyer type
  data_unique <- data %>%
    group_by(ID) %>%
    summarise(across(ends_with("_often_buyer"), ~ max(.x, na.rm = TRUE)), .groups = "drop")
  
  # Define the relevant columns
  buyer_cols <- c("almond_often_buyer", "dairy_often_buyer", "oat_often_buyer", "soy_often_buyer")
  
  # Generate combination labels
  data_unique$combination <- apply(data_unique[, buyer_cols], 1, get_combination_label)
  
  # Count combinations
  summary <- data_unique %>%
    group_by(combination) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  # Plot with clean white background and no gridlines
  plot <- ggplot(summary, aes(x = reorder(combination, -count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = count), vjust = -0.5, size = 4) +
    labs(title = title, x = "Milk combination frequently purchased", y = "Number of consumers") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks = element_line(color = "black")
    )
  
  # Save plot
  ggsave(filename, plot, width = 10, height = 6)
}

# Apply to gallon and half-gallon subsets
create_plot(gallon_full, "Distribution of frequently purchased milk types (Gallon)", paste0(output, "gallon_often_distr.png"))
create_plot(halfgal_full, "Distribution of frequently purchased milk types (Half-Gallon)", paste0(output, "halfgal_often_distr.png"))

###############################################################################
#Keep our soy milk purchasing consumers
###############################################################################


# Helper function to calculate descriptive statistics
get_summary_stats <- function(data) {
  hh_size <- data %>%
    group_by(ID) %>%
    summarise(hh_size = mean(adults + young_children + old_children, na.rm = TRUE)) %>%
    pull(hh_size)
  
  single <- data %>%
    group_by(ID) %>%
    summarise(adults = mean(adults, na.rm = TRUE),
              young_children = mean(young_children, na.rm = TRUE),
              old_children = mean(old_children, na.rm = TRUE)) %>%
    mutate(single = adults == 1 & young_children == 0 & old_children == 0) %>%
    pull(single)
  
  vegan <- data %>%
    group_by(ID) %>%
    summarise(vegan = first(vegan)) %>%
    pull(vegan)
  
  list(
    `Median household size` = format(round(median(hh_size, na.rm = TRUE), 1), nsmall = 1),
    `Single households (%)` = format(round(mean(single, na.rm = TRUE) * 100, 1), nsmall = 1),
    `Vegan in household (%)` = format(round(mean(vegan == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
    `Mean share chosen soy` = round(mean(data$share_chosen_soy, na.rm = TRUE), 2),
    `Mean share chosen dairy` = round(mean(data$share_chosen_dairy, na.rm = TRUE), 2),
    `Mean share chosen oat` = round(mean(data$share_chosen_oat, na.rm = TRUE), 2),
    `Mean share chosen almond` = round(mean(data$share_chosen_almond, na.rm = TRUE), 2)
  )
}

# Filter soy buyers
gallon_soy <- gallon_full %>%
  group_by(ID) %>%
  summarise(across(ends_with("_often_buyer"), ~ max(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(soy_often_buyer == 1) %>%
  inner_join(gallon_full, by = "ID")

halfgal_soy <- halfgal_full %>%
  group_by(ID) %>%
  summarise(across(ends_with("_often_buyer"), ~ max(.x, na.rm = TRUE)), .groups = "drop") %>%
  filter(soy_often_buyer == 1) %>%
  inner_join(halfgal_full, by = "ID")

# Define groups
groups <- list(
  `Control (g)` = subset(gallon_soy, control == 1),
  `General (g)` = subset(gallon_soy, general_treat == 1),
  `Protein (g)` = subset(gallon_soy, protein_treat == 1),
  `Control (hg)` = subset(halfgal_soy, control == 1),
  `General (hg)` = subset(halfgal_soy, general_treat == 1),
  `Protein (hg)` = subset(halfgal_soy, protein_treat == 1)
)

# Generate summary table
summary_table <- lapply(groups, get_summary_stats)
summary_df <- do.call(rbind, summary_table)
summary_df <- as.data.frame(summary_df)
summary_df


