# ################################################################# #
###      Purchasing milk preferences tables                      ####
# ################################################################# #
# Agnes af Sandeberg 21 January 2026
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


#4. Define environmental concern variables
env_vars <- c("highco2_conc", "highclimchange_conc", "highnutri_conc", "highwatuse_conc")

#5. Define subsamples which are the different milk buyers (ever) separated by gallon and half-gallon
# Create dummies where ever purchasers 1,2,3, 4,5 are coded as 1 and 6 are coded as zero
# 1= daily , 2=multiple times per week, 3=about once a week, 4= A couple of times per month, 5= Less than once per month 
groups <- list(
  `Dairy buyer (g)` = full_sample %>% filter(dairy_buyer != 6, gallon == 1),
  `Almond buyer (g)` = full_sample %>% filter(almond_buyer != 6, gallon == 1),
  `Oat buyer (g)` = full_sample %>% filter(oat_buyer != 6, gallon == 1),
  `Soy buyer (g)` = full_sample %>% filter(soy_buyer != 6, gallon == 1),
  `Dairy buyer (hg)` = full_sample %>% filter(dairy_buyer != 6, gallon == 0),
  `Almond buyer (hg)` = full_sample %>% filter(almond_buyer != 6, gallon == 0),
  `Oat buyer (hg)` = full_sample %>% filter(oat_buyer != 6, gallon == 0),
  `Soy buyer (hg)` = full_sample %>% filter(soy_buyer != 6, gallon == 0)
)


#8. Get observation counts
group_counts <- sapply(groups, function(df) n_distinct(df$ID))

#9. Create a summary table: rows = env_vars, columns = groups
env_summary <- lapply(env_vars, function(var) {
  sapply(groups, function(df) {
    df_unique <- df %>% distinct(ID, .keep_all = TRUE)
    sprintf("%.1f", mean(df_unique[[var]], na.rm = TRUE) * 100)
  })
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

#10.Add row labels as a column called "Frequency (%)"
env_summary <- env_summary %>%
  tibble::rownames_to_column(var = "Frequency (%)")

#11. Add row names for clarity
env_summary$`Frequency (%)`<- c(
  "Carbon emissions",
  "Climate change",
  "Nutritional content",
  "Water use"
)


#12. Replace NA with "~" for LaTeX
env_summary[is.na(env_summary)] <- "~"


#13. Rename columns
colnames(env_summary) <- c(
  "\\makecell[l]{High concern (\\%)}",
  "\\makecell[c]{Dairy \\\\ buyers}",
  "\\makecell[c]{Almond \\\\ buyers}",
  "\\makecell[c]{Oat \\\\ buyers}",
  "\\makecell[c]{Soy \\\\ buyers}",
  "\\makecell[c]{Dairy \\\\ buyers}",
  "\\makecell[c]{Almond \\\\ buyers}",
  "\\makecell[c]{Oat \\\\ buyers}",
  "\\makecell[c]{Soy \\\\ buyers}"
)


grouped_header <- c(" " = 1,
                    "Gallon consumers" = 4,
                    "Half-gallon consumers" = 4)
# Create the N = row
n_row <- c("N =", as.character(group_counts))
names(n_row) <- colnames(env_summary)

# Append the N = row at the bottom of the summary table
env_summary <- rbind(env_summary, n_row)

#14. Format table
table_tex <- kable(env_summary, 
                   format = "latex", 
                   booktabs = TRUE, 
                   row.names = FALSE, 
                   align = 'l',
                   col.names = colnames(env_summary),
                   escape = FALSE) %>%
  add_header_above(grouped_header, escape = FALSE, align = "l") %>%
  kable_styling()

#15. Remove any existing \begin{table} and \end{table} blocks
table_tex_clean <- as.character(table_tex)
table_tex_clean <- gsub("\\\\addlinespace", "", table_tex_clean)
table_tex_clean <- gsub("\\\\begin\\{table\\}.*?\\\\centering", "", table_tex_clean)
table_tex_clean <- gsub("\\\\end\\{table\\}", "", table_tex_clean)

# Split into lines
table_lines <- unlist(strsplit(table_tex_clean, "\n"))

# Find the line that contains "N =" and insert \midrule before it
n_line_index <- grep("^N =\\s*&", table_lines)

if (length(n_line_index) == 1) {
  table_lines <- append(table_lines, "\\midrule", after = n_line_index - 1)
}

# Recombine the lines
table_tex_clean <- paste(table_lines, collapse = "\n")
#16. Wrap in LaTeX table environment
table_tex <- paste0(
  "\\setlength{\\tabcolsep}{4pt}\n",
  "\\renewcommand{\\arraystretch}{1.1}\n",
  "{\\small\n",
  "\\begin{table}[!htbp]\n\\caption{Frequency of moderate and extreme concern by milk type purchasing group}\n\\centering\n",
  table_tex_clean,
  "\n\\label{tab:env_conc}\n\\end{table}\n",
  "}"
)

#17. Save to .tex file
cat(table_tex, file = paste0(output, "environ_conc_milk_purch_no_protein.tex"))

# ------------------------------------------------------------
# Create the same table but now restrict the sample to the groups 
# to contain consumers that buy a specific milk type as their 
# most often purchase
# ------------------------------------------------------------

#1. We have a dummy for where 6 (6=never purchasers) are coded as zero and 1-5 (daily to less than once per month is coded as 1)
# Create dummies where often purchasers 1,2,3 are coded as 1 and 4-6 are coded as zero
# 1= daily , 2=multiple times per week, 3=about once a week


#2. Find the minimum value across the milk type columns for each unique ID
milk_columns <- c("almond_buyer", "dairy_buyer", "oat_buyer", "soy_buyer", "other_buyer")
full_sample$min_value <- apply(full_sample[milk_columns], 1, min, na.rm = TRUE)

#3. Create dummy variables for each milk type
for (milk in milk_columns) {
  milk_type <- gsub("_buyer", "", milk)  # Extract milk type name
  dummy_name <- paste0("pref_milk_", milk_type)
  full_sample[[dummy_name]] <- ifelse(full_sample[[milk]] == full_sample$min_value, 1, 0)
}



#7. Define subsamples which are the different milk buyers (preferred type) separated by gallon and half-gallon
groups_pref <- list(
  `Dairy buyer (g)` = full_sample %>% filter(pref_milk_dairy == 1, gallon == 1),
  `Almond buyer (g)` = full_sample %>% filter(pref_milk_almond == 1, gallon == 1),
  `Oat buyer (g)` = full_sample %>% filter(pref_milk_oat == 1, gallon == 1),
  `Soy buyer (g)` = full_sample %>% filter(pref_milk_soy == 1, gallon == 1),
  `Dairy buyer (hg)` = full_sample %>% filter(pref_milk_dairy == 1, gallon == 0),
  `Almond buyer (hg)` = full_sample %>% filter(pref_milk_almond == 1, gallon == 0),
  `Oat buyer (hg)` = full_sample %>% filter(pref_milk_oat == 1, gallon == 0),
  `Soy buyer (hg)` = full_sample %>% filter(pref_milk_soy == 1, gallon == 0)
)


# ----------------------------------------------------------------------------------
# Pairwise proportion tests for each environmental and nutritional concern variable
# ----------------------------------------------------------------------------------


# Split group names
gallon_groups <- names(groups_pref)[1:4]
halfgallon_groups <- names(groups_pref)[5:8]

# Function to run pairwise tests within a group list
run_pairwise_tests <- function(var, group_list) {
  results <- list()
  for (i in 1:(length(group_list) - 1)) {
    for (j in (i + 1):length(group_list)) {
      g1 <- group_list[i]
      g2 <- group_list[j]
      
      df1 <- groups_pref[[g1]] %>% distinct(ID, .keep_all = TRUE)
      df2 <- groups_pref[[g2]] %>% distinct(ID, .keep_all = TRUE)
      
      x1 <- sum(df1[[var]], na.rm = TRUE)
      n1 <- sum(!is.na(df1[[var]]))
      x2 <- sum(df2[[var]], na.rm = TRUE)
      n2 <- sum(!is.na(df2[[var]]))
      
      test <- prop.test(c(x1, x2), c(n1, n2))
      test_tidy <- tidy(test)
      
      meta <- tibble(
        Group1 = g1,
        Group2 = g2,
        Variable = var,
        Sample1 = n1,
        Sample2 = n2,
        x1 = x1,
        x2 = x2
      )
      
      results[[paste(g1, "vs", g2)]] <- bind_cols(test_tidy, meta)
    }
  }
  bind_rows(results)
}



run_gallon_vs_halfgal_tests <- function(var, milk_pairs) {
  results <- list()
  for (pair in milk_pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    
    df1 <- groups_pref[[g1]] %>% distinct(ID, .keep_all = TRUE)
    df2 <- groups_pref[[g2]] %>% distinct(ID, .keep_all = TRUE)
    
    x1 <- sum(df1[[var]], na.rm = TRUE)
    n1 <- sum(!is.na(df1[[var]]))
    x2 <- sum(df2[[var]], na.rm = TRUE)
    n2 <- sum(!is.na(df2[[var]]))
    
    test <- prop.test(c(x1, x2), c(n1, n2))
    
    test_tidy <- tidy(test)
    
    # Add metadata using bind_cols to avoid NA issues
    meta <- tibble(
      Group1 = g1,
      Group2 = g2,
      Variable = var,
      Sample1 = n1,
      Sample2 = n2,
      x1 = x1,
      x2 = x2
    )
    
    results[[paste(g1, "vs", g2)]] <- bind_cols(test_tidy, meta)
    
  }
  bind_rows(results)
}

#Now compare each milk across gallon and half-gallon
milk_type_pairs <- list(
  c("Dairy buyer (g)", "Dairy buyer (hg)"),
  c("Almond buyer (g)", "Almond buyer (hg)"),
  c("Oat buyer (g)", "Oat buyer (hg)"),
  c("Soy buyer (g)", "Soy buyer (hg)")
)

# Run tests for all variables within each group type
gallon_tests <- lapply(env_vars, run_pairwise_tests, group_list = gallon_groups) %>% bind_rows()
halfgallon_tests <- lapply(env_vars, run_pairwise_tests, group_list = halfgallon_groups) %>% bind_rows()
milktype_comparisons <- lapply(env_vars, run_gallon_vs_halfgal_tests, milk_pairs = milk_type_pairs) %>% bind_rows()

# Combine results
all_tests_filtered <- bind_rows(gallon_tests, halfgallon_tests, milktype_comparisons)

# Filter for statistically significant differences
significant_results <- all_tests_filtered %>%
  filter(p.value < 0.05) %>%
  arrange(Variable, p.value)

#----------------------------------------
# Create the table
#----------------------------------------

#9. Get observation counts
group_pref_counts <- sapply(groups_pref, function(df_pref) n_distinct(df_pref$ID))

#10. Create a summary table: rows = env_vars, columns = groups
env_pref_summary <- lapply(env_vars, function(var) {
  sapply(groups_pref, function(df_pref) {
    df_pref_unique <- df_pref %>% distinct(ID, .keep_all = TRUE)
    sprintf("%.1f", mean(df_pref_unique[[var]], na.rm = TRUE) * 100)
  })
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

#Add row labels as a column called "Frequency (%)"
env_pref_summary <- env_pref_summary %>%
  tibble::rownames_to_column(var = "Frequency (%)")

# Add row names for clarity
env_pref_summary$`Frequency (%)`<- c(
  "Carbon emissions",
  "Climate change",
  "Nutritional content",
  "Water use"
)


# Replace NA with "~" for LaTeX
env_pref_summary[is.na(env_pref_summary)] <- "~"

# Internal column names for mapping
internal_colnames <- c(
  "Dairy_g", "Almond_g", "Oat_g", "Soy_g",
  "Dairy_hg", "Almond_hg", "Oat_hg", "Soy_hg"
)
names(groups_pref) <- internal_colnames



###################################
#Add significance annotation
###################################

# Map original group names to internal names
group_name_map <- setNames(internal_colnames, c(
  "Dairy buyer (g)", "Almond buyer (g)", "Oat buyer (g)", "Soy buyer (g)",
  "Dairy buyer (hg)", "Almond buyer (hg)", "Oat buyer (hg)", "Soy buyer (hg)"
))

#Map group names to column indices
var_label_map <- c(
  "highco2_conc" = "Carbon emissions",
  "highclimchange_conc" = "Climate change",
  "highnut_conc" = "Nutritional content",
  "highwatuse_conc" = "Water use"
)

colnames(env_pref_summary) <- c("label", internal_colnames)

# Annotate
env_pref_annotated <- env_pref_summary

extract_numeric <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "~") return(NA_real_)
  x_clean <- gsub("\\$\\^\\{[ab]\\}\\$", "", x)
  x_clean <- gsub("[^0-9\\.]", "", x_clean)
  as.numeric(x_clean)
}

#Loop though results significant at the 5% level and annotate
for (i in seq_len(nrow(significant_results))) {
  row <- significant_results[i, ]
  var <- row$Variable
  g1 <- group_name_map[[row$Group1]]
  g2 <- group_name_map[[row$Group2]]
  
  # Find row index for the variable
  row_label <- var_label_map[[var]]
  row_index <- which(env_pref_annotated$label == row_label)
  
  # Always put superscript on group 2 (the non reference group)
  target_col <- group_name_map[[row$Group2]]
  
  # Get current cell value
  current_val <- as.character(env_pref_annotated[row_index, target_col])
  
  # Extract existing superscripts
  existing_superscripts <- gregexpr("\\$\\^\\{[a-d]\\}\\$", current_val)[[1]]
  found <- if (existing_superscripts[1] != -1) {
    regmatches(current_val, gregexpr("\\$\\^\\{[a-d]\\}\\$", current_val))[[1]]
  } else {
    character(0)
  }
  
  # Remove existing superscripts from value
  base_val <- gsub("\\$\\^\\{[a-d]\\}\\$", "", current_val)
  
  # Determine which superscripts to add
  to_add <- character(0)
  
  if (any(sapply(milk_type_pairs, function(pair) all(c(row$Group1, row$Group2) %in% pair)))) {
    to_add <- c(to_add, "$^{a}$")
  }
  if (grepl("Dairy buyer", row$Group1)) {
    to_add <- c(to_add, "$^{b}$")
  }
  if (grepl("Almond buyer", row$Group1)) {
    to_add <- c(to_add, "$^{c}$")
  }
  if (grepl("Oat buyer", row$Group1)) {
    to_add <- c(to_add, "$^{d}$")
  }
  
  # Combine existing and new superscripts, keeping only unique ones, in correct order
  ordered <- c("$^{a}$", "$^{b}$", "$^{c}$", "$^{d}$")
  final_superscripts <- ordered[ordered %in% unique(c(found, to_add))]
  
  # Rebuild the value
  current_val <- paste0(base_val, paste0(final_superscripts, collapse = ""))
  
  # Update the cell
  env_pref_annotated[row_index, target_col] <- current_val
}


  
#Rename columns
colnames(env_pref_annotated) <- c(
  "\\makecell[l]{High concern (\\%)}",
  "\\makecell[c]{Dairy \\\\ buyers}",
  "\\makecell[c]{Almond \\\\ buyers}",
  "\\makecell[c]{Oat \\\\ buyers}",
  "\\makecell[c]{Soy \\\\ buyers}",
  "\\makecell[c]{Dairy \\\\ buyers}",
  "\\makecell[c]{Almond \\\\ buyers}",
  "\\makecell[c]{Oat \\\\ buyers}",
  "\\makecell[c]{Soy \\\\ buyers}"
)


grouped_header <- c(" " = 1,
                    "Gallon consumers" = 4,
                    "Half-gallon consumers" = 4)

# Create the N = row
n_row <- c("N =", as.character(group_pref_counts))
names(n_row) <- colnames(env_pref_summary)
# Append the N = row at the bottom of the summary table
env_pref_annotated <- rbind(env_pref_annotated, n_row)


table_tex <- kable(env_pref_annotated,
                   format = "latex", 
                   booktabs = TRUE, 
                   row.names = FALSE, 
                   align = 'l',
                   col.names = colnames(env_pref_annotated),
                   escape = FALSE) %>%
  add_header_above(grouped_header, escape = FALSE, align = "l") %>%
  kable_styling()




# Remove any existing \begin{table} and \end{table} blocks
table_tex_clean <- as.character(table_tex)
table_tex_clean <- gsub("\\\\addlinespace", "", table_tex_clean)
table_tex_clean <- gsub("\\\\begin\\{table\\}.*?\\\\centering", "", table_tex_clean)
table_tex_clean <- gsub("\\\\end\\{table\\}", "", table_tex_clean)

# Split into lines
table_lines <- unlist(strsplit(table_tex_clean, "\n"))

# Find the line that contains "N =" and insert \midrule before it
n_line_index <- grep("^N =\\s*&", table_lines)

if (length(n_line_index) == 1) {
  table_lines <- append(table_lines, "\\midrule", after = n_line_index - 1)
}

# Recombine the lines
table_tex_clean <- paste(table_lines, collapse = "\n")



# Wrap in LaTeX table environment
table_tex <- paste0(
  "\\setlength{\\tabcolsep}{4pt}\n",
  "\\renewcommand{\\arraystretch}{1.1}\n",
  "{\\small\n",
  "\\begin{table}[!htbp]\n",
  "\\centering\n",
  "\\begin{threeparttable}\n",
  "\\caption{Frequency of moderate and extreme concern by most preferred milk type(s)}\n",
  table_tex_clean, "\n",
  "\\begin{tablenotes}[para,flushleft]\n",
  "\\small\n",
  "\\item $^{a}$ Share differ between gallon and half-gallon buyers of the same milk type (p<0.05).\n",
  "\\item $^{b}$ Share differ compared to dairy buyers of the same container size (p<0.05).\n",
  "\\item $^{c}$ Share differ compared to almond buyers of the same container size (p<0.05).\n",
  "\\end{tablenotes}\n",
  "\\label{tab:env_pref_conc}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n",
  "}"
)

# Save to .tex file
cat(table_tex, file = paste0(output, "env_pref_conc_milk_purch_no_protein.tex"))





