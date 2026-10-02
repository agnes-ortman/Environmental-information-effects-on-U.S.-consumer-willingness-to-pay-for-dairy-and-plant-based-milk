# ################################################################# #
###     Descriptive statistics DCE                               ####
# ################################################################# #
# Agnes af Sandeberg 12 November 2025
# R version 4.5.1 

# 1. read packages
rm(list = ls())
# List of required packages
required_packages=c("data.table", "plyr", "dplyr", "lubridate", "support.CEs", "readxl", "xtable", 
                    "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "survival", "stats", "haven", 
                    "knitr", "kableExtra", "stringr")
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
database <- read_dta("Analysis/Input/DCE_ready_for_analysis.dta")

# 4. Drop the protein sample
database <- database %>%
  filter(protein_treat != 1)

# 5. Calculate the observation count
observation_count <- n_distinct(database$ID)

# 6. Create a summary table
create_summary_table <- function(data) {
  data %>%
    summarise(
      `Male` = format(round(mean(male == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Female` = format(round(mean(male == 0, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 18-24` = format(round(mean(age_cohort == 2, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 25-34` = format(round(mean(age_cohort == 3, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 35-44` = format(round(mean(age_cohort == 4, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 45-54` = format(round(mean(age_cohort == 5, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 55-64` = format(round(mean(age_cohort == 6, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Age 65+` = format(round(mean(age_cohort == 7, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Less than High School` = format(round(mean(education == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `High School degree` = format(round(mean(education == 2, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Some college` = format(round(mean(education == 3, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Bachelor's degree or higher` = format(round(mean(education == 4, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Less than \\$25K` = format(round(mean(income  == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `\\$25K-\\$50K` = format(round(mean(income == 2, na.rm = TRUE) * 100, 1), nsmall = 1),
      `\\$51K-\\$74K` = format(round(mean(income  == 3, na.rm = TRUE) * 100, 1), nsmall = 1),
      `\\$71K-\\$99K` = format(round(mean(income  == 4, na.rm = TRUE) * 100, 1), nsmall = 1),
      `\\$100K+` = format(round(mean(income == 5, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Urban` = format(round(mean(urban == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Rural` = format(round(mean(rural == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Suburban` = format(round(mean(suburban == 1, na.rm = TRUE) * 100, 1), nsmall = 1),
      `Median household size` = format(round(median(data %>%
                                                      group_by(ID) %>%
                                                      summarise(hh_size = mean(adults + young_children + old_children, na.rm = TRUE)) %>%
                                                      pull(hh_size),na.rm = TRUE),1),nsmall = 1),
      `Single households (\\%)` = format(round(mean(data %>%
                                                      group_by(ID) %>%
                                                      summarise(adults = mean(adults, na.rm = TRUE),young_children = mean(young_children, na.rm = TRUE), old_children = mean(old_children, na.rm = TRUE)) %>%
                                                      mutate(single = adults == 1 & young_children == 0 & old_children == 0) %>%
                                                      pull(single), na.rm = TRUE) * 100, 1), nsmall = 1)
    )
}


# Create the summary table
summary_table <- create_summary_table(database)

# Convert the summary table to a data frame
summary_df <- as.data.frame(summary_table)

# Convert all columns to character type
summary_df <- summary_df %>%
  mutate(across(everything(), as.character))


# Add the "Variable" column
summary_df <- summary_df %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Sample_N")

# Rename the column to dynamically include the observation count
colnames(summary_df)[colnames(summary_df) == "Sample_N"] <- paste0("All respondents (n = ", observation_count, ")")

colnames(summary_df)


# Add the "Census" column
summary_df$Census <- ""

# Add predefined percentages in the first 14 rows and empty for the rest
predefined_percentages <- c(49.1, 50.9, 11.9, 17.4, 16.9, 15.6, 16.2, 22.1, 10.5, 27.2, 29.3, 33.0, 16.0, 18.0, 16.0, 12.0, 38.0)
formatted_percentages <- format(predefined_percentages, nsmall = 1)
summary_df$Census[1:17] <- formatted_percentages


# Add headings within the "Variable" column with bold formatting
headings <- data.frame(
  Variable = c("\\textbf{Gender (\\%)}", "\\textbf{Age (\\%)}", "\\textbf{Education (\\%)}", "\\textbf{Income (\\%)}", "\\textbf{Community (\\%)}", "\\textbf{Household}"),
  `All respondents (n = )` = "",
  Census = ""
)

# Insert headings into the summary_df
summary_df <- bind_rows(
  headings[1, ],
  summary_df[1:2, ],
  headings[2, ],
  summary_df[3:8, ],
  headings[3, ],
  summary_df[9:12, ],
  headings[4, ],
  summary_df[13:17, ],
  headings[5, ],
  summary_df[18:20, ],
  headings[6, ],
  summary_df[21:nrow(summary_df), ]
)

# Escape percentage symbols in the "Variable" column
summary_df$Variable <- gsub("%", "\\%", summary_df$Variable)


summary_df$Variable <- gsub("\\$", "\\$", summary_df$Variable)


# Ensure rounding is applied to the summary_df data frame
summary_df <- summary_df %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 1), nsmall = 1)))

# Update the column name to include the observation count
colnames(summary_df)[colnames(summary_df) == "All respondents (n = )"] <- paste0("All respondents (n = ", observation_count, ")")



# Create a function to replace NA with empty strings for headings
replace_na_with_empty <- function(df) {
  df %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
}

# Apply the function to the summary_df
summary_df <- replace_na_with_empty(summary_df)


#Wrap long labels using \makecell and insert line breaks at spaces after a certain length
wrap_label <- function(label, width = 25) {
  # Skip wrapping for bold section headers
  if (grepl("^\\\\textbf", label)) {
    return(label)
  }
  
  # Only wrap if label is long
  if (nchar(label) > width) {
    words <- unlist(strsplit(label, " "))
    lines <- c()
    current_line <- ""
    for (word in words) {
      if (nchar(current_line) + nchar(word) + 1 <= width) {
        current_line <- paste(current_line, word)
      } else {
        lines <- c(lines, str_trim(current_line))
        current_line <- word
      }
    }
    lines <- c(lines, str_trim(current_line))
    return(paste0("\\makecell[l]{", paste(lines, collapse = " \\\\ "), "}"))
  } else {
    return(label)
  }
}


# Save the summary table to a TeX file in the specified location with reordered columns
output_path <- "Analysis/Output/Tables/dess_stats_no_protein.tex"

# Create the table without additional formatting
table_tex <- kable(summary_df[, c("Variable", paste0("All respondents (n = ", observation_count, ")"), "Census")], 
                   format = "latex", booktabs = TRUE, row.names = FALSE, align = 'l', escape = FALSE)

# Remove \addlinespace commands
table_tex <- gsub("\\\\addlinespace", "", table_tex)



# Add LaTeX commands to include the caption at the top of the table
table_tex <- paste0("\\begin{table}[!htbp]\n\\caption{Sample characteristics}\n\\centering\n", table_tex, "\n\\label{tab:descriptive_statistics}\n\\end{table}")


# Save the table to a file
cat(table_tex, file = output_path)


# -------------------------------
# Add subsample columns
# -------------------------------


# Define the subsamples
datasets <- list(
  `Control (g)` = subset(database, control == 1 & gallon == 1),
  `Treatment (g)` = subset(database, general_treat == 1 & gallon == 1),
  `Control (hg)` = subset(database, control == 1 & gallon == 0),
  `Treatment (hg)` = subset(database, general_treat == 1 & gallon == 0)
)

# Generate summary tables for each subsample
subsample_summaries <- lapply(datasets, function(data) {
  create_summary_table(data) %>%
    as.data.frame() %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "value")
})

# Combine all summaries into one data frame
for (name in names(subsample_summaries)) {
  summary_df <- summary_df %>%
    left_join(subsample_summaries[[name]], by = "Variable") %>%
    rename(!!name := value)
}

# Reorder columns: Variable, All respondents,Census, [6 new columns], 
summary_df <- summary_df %>%
  select(
    Variable,
    matches("^All respondents \\(n = "),
    Census,
    `Control (g)`, `Treatment (g)`, 
    `Control (hg)`, `Treatment (hg)`
  )

# Replace all NA values with "~" for LaTeX non-breaking space
summary_df <- summary_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "~", .)))


# Rename the All respondents column
colnames(summary_df)[grepl("^All respondents \\(n = ", colnames(summary_df))] <- "All respondents"

# Get observation counts for each subsample
subsample_counts <- sapply(datasets, function(df) n_distinct(df$ID))

# Rename columns to include N in a second header row
colnames(summary_df)[colnames(summary_df) == paste0("All respondents (n = ", observation_count, ")")] <- "All respondents"
names_with_n <- c(
  "All respondents" = paste0("n = ", observation_count),
  Census = "",
  `Control (g)` = paste0("n = ", subsample_counts["Control (g)"]),
  `Treatment (g)` = paste0("n = ", subsample_counts["Treatment (g)"]),
  `Control (hg)` = paste0("n = ", subsample_counts["Control (hg)"]),
  `Treatment (hg)` = paste0("n = ", subsample_counts["Treatment (hg)"])
)

# Create the two-row header

# Get the actual column names from summary_df
col_names_actual <- colnames(summary_df)

# Build the first row (labels) and second row (n= counts)
header_labels <- col_names_actual
# Build the second row of the header (n = ...)
header_counts <- sapply(colnames(summary_df), function(col) {
  if (col == "Variable") return("N =")
  if (col == "Census") return("~")
  if (col == "All respondents") return(format(observation_count, big.mark = ","))
  if (col %in% names(subsample_counts)) return(format(subsample_counts[[col]], big.mark = ","))
  return("~")
})


# Create named lists for both header rows
header_counts_named <- setNames(rep(1, length(header_counts)), header_counts)

# Get actual column names
actual_colnames <- colnames(summary_df)


# Create a simple named vector for the first header row (no wrapping)
header_labels_named <- setNames(rep(1, length(col_names_actual)), col_names_actual)


# Apply the wrapping only to non-header rows
summary_df$Variable <- sapply(summary_df$Variable, wrap_label)

# -------------------------------
# Save the extended table
# -------------------------------

# Add a grouped header for Gallon and Half-gallon
grouped_header <- c(
  " " = 3,  # For Variable All respondents, Census
  "Gallon consumers" = 2,
  "Half-gallon consumers" = 2
)


treatment_labels <- c(
  "Characteristic" = 1,
  "All respondents" = 1,
  "Census" = 1,
  "Control" = 1,
  "Treatment" = 1,
  "Control" = 1,
  "Treatment" = 1
)



# Rebuild LaTeX table with two-row header
table_tex_extended <- kable(summary_df,
                            format = "latex",
                            booktabs = TRUE,
                            row.names = FALSE,
                            align = 'l',
                            col.names = NULL,
                            escape = FALSE) %>%
  add_header_above(header_counts_named, escape = FALSE, line = FALSE, align = "l") %>%
  add_header_above(treatment_labels, escape = FALSE, line = FALSE, align = "l") %>%
  add_header_above(grouped_header, escape = FALSE, align = "l") %>%
  kable_styling()



# Remove any existing \begin{table} and \end{table} blocks
table_tex_extended <- gsub("\\\\begin\\{table\\}.*?\\\\centering", "", table_tex_extended)
table_tex_extended <- gsub("\\\\end\\{table\\}", "", table_tex_extended)


# Convert to character and insert \midrule manually after the second header row
table_tex_extended <- as.character(table_tex_extended)


# Split LaTeX table into lines
table_lines <- strsplit(table_tex_extended, "\n")[[1]]

# Find the index of the second header row (line ending with \\)
header_row_indices <- grep("\\\\\\\\$", table_lines)

# Insert \midrule after the second header row
if (length(header_row_indices) >= 3) {
  table_lines <- append(table_lines, "\\midrule", after = header_row_indices[3])
}

# Recombine the lines
table_tex_extended <- paste(table_lines, collapse = "\n")

table_tex_extended <- gsub("\\\\addlinespace", "", table_tex_extended)


table_tex_extended <- paste0(
  "\\setlength{\\tabcolsep}{4pt}\n",
  "\\renewcommand{\\arraystretch}{1.1}\n",
  "{\\small\n",  # <-- opens a group
  "\\begin{table}[!htbp]\n\\caption{Sample characteristics by subsample}\n\\centering\n",
  table_tex_extended,
  "\n\\label{tab:descriptive_statistics_extended}\n\\end{table}\n",
  "}"  # <-- closes the group
)



# Save the updated table
output_path_2 <- "Analysis/Output/Tables/des_stat_table_noprotein_extended.tex"
cat(table_tex_extended, file = output_path_2)


print(paste("Extended descriptive statistics table saved to", output_path_2))


########################################################################
# Create a descriptive table of the distribution across samples
########################################################################
# 1. Create subsamples
control_sample <- subset(database, control == 1)
treat_general <- subset(database, general_treat == 1)


# 2. Create gallon/half-gallon subsamples
control_gallon <- subset(control_sample, gallon == 1)
control_halfgal <- subset(control_sample, gallon == 0)
treat_gen_gallon <- subset(treat_general, gallon == 1)
treat_gen_halfgal <- subset(treat_general, gallon == 0)



# 3. Create the table with unique ID counts
resp_distr_df <- data.frame(
  "Information treatment subsample" = c("Treatment: $CO_{2}$ and water use feedback", 
                                        "No feedback", "Sample"),
  "Gallon" = c(n_distinct(treat_gen_gallon$ID), n_distinct(control_gallon$ID), sum(n_distinct(treat_gen_gallon$ID) + n_distinct(control_gallon$ID))),
  "Half-gallon" = c(n_distinct(treat_gen_halfgal$ID), n_distinct(control_halfgal$ID), sum((n_distinct(treat_gen_halfgal$ID) + n_distinct(control_halfgal$ID)))),
  "Total" = c(n_distinct(treat_general$ID), n_distinct(control_sample$ID), n_distinct(database$ID)),
  check.names = FALSE
)

# 4. Convert all columns to character type
resp_distr_df <- resp_distr_df %>%
  mutate(across(everything(), as.character))

# 5. Create the table without additional formatting
table_tex2 <- kable(resp_distr_df, format = "latex", booktabs = TRUE, row.names = FALSE, align = 'l', escape = FALSE)

# 6. Add LaTeX commands to include the caption at the top of the table
table_tex2 <- paste0("\\begin{table}[!htbp]\n\\caption{Respondent distribution across samples}\n\\centering\n", table_tex2, "\n\\label{tab:respondent_distribution}\n\\end{table}")

# 7. Save the table to a file
output_path <- "Analysis/Output/Tables/respondent_distribution_table_no_protein.tex"
cat(table_tex2, file = output_path)

