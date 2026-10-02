#####################################################
#RPL models from Nlogit DAIRY AS REFERENCE LEVEL
#####################################################
#In this file we import the RPL pref space coefficients from Nlogit, 
#Agnes af Sandeberg 18 December 2025

#1. read packages
rm(list = ls())
# List of required packages
required_packages<-c("data.table", "plyr","tidyr", "dplyr", "lubridate", "support.CEs", "readxl",
                     "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "logitr", "survival", 
                     "stats", "haven", "aod", "survival", "dfidx", "lmtest", "texreg", 
                     "kableExtra", "stargazer", "remotes", "cmdlr", "mded", "tibble")
# Install missing packages and load them
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    #FIXME Add special case for devtools::install_github("edsandorf/cmdlr")
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE) # Load the package
}


# ################################################################# #
#### LOAD PANEL DATA AND APPLY ANY TRANSFORMATIONS               ####
# ################################################################# #
#2. Set working directory
path <- Sys.getenv("PWD")
setwd("C:/Users/asor0002/git/DCE_data_management")


#3. load the data from the RPL models

# Define file path
rpl_models <- "Analysis/Input/ind_betas_from_correlated_models.xlsx"

# Define sheet names to import
sheet_names <- c(
  "Control_Gallon_OUTPUT",
  "Control_Half_Gallon_OUTPUT",
  "General_Gallon_OUTPUT",
  "General_Half_Gallon_OUTPUT"
)

# Read all sheets into a named list
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(rpl_models, sheet = sheet)
})
names(data_list) <- sheet_names


# Assign each to a variable in the global environment (create six dataframes)
list2env(data_list, envir = .GlobalEnv)


###################################################################

# Custom mapping from dataset names to model suffixes
model_suffix_map <- list(
  "Control_Gallon_OUTPUT" = "cg",
  "Control_Half_Gallon_OUTPUT" = "chg",
  "General_Gallon_OUTPUT" = "tgg",
  "General_Half_Gallon_OUTPUT" = "tghg"
)


# Initialize lists
coeff_list <- list()
se_list <- list()

# Loop through datasets
for (name in names(model_suffix_map)) {
  df <- get(name)
  suffix <- model_suffix_map[[name]]
  
  # Keep the "Variable" column along with Coefficient and Std.Error
  coeff_df <- df %>%
    select(Variable, contains("Coefficient")) %>%
    mutate(model = suffix)
  
  se_df <- df %>%
    select(Variable, contains("Std.Error")) %>%
    mutate(model = suffix)
  
  coeff_list[[name]] <- coeff_df
  se_list[[name]] <- se_df
}

# Combine into final data frames
coeff_df <- bind_rows(coeff_list)
ses_df <- bind_rows(se_list)

# Define the variables to keep and their new names
var_map <- c(
  "CDUM" = "asc",
  "TYPE_ALM" = "almond_dum",
  "TYPE_OAT" = "oat_dum",
  "TYPE_SOY" = "soy_dum",
  "PRICE_VA" = "price",
  "sdCDUM" = "sd_asc",
  "sdTYPE_A" = "sd_almond_dum",
  "sdTYPE_O" = "sd_oat_dum",
  "sdTYPE_S" = "sd_soy_dum"
)

# Merge coefficients and standard errors
merged_df <- left_join(coeff_df, ses_df, by = c("Variable", "model"))

# Filter to keep only the variables of interest
filtered_df <- merged_df %>%
  filter(Variable %in% names(var_map))

# Rename variables
filtered_df <- filtered_df %>%
  mutate(Variable = var_map[Variable])

# Split into six datasets
mxl_pref_cg   <- filtered_df %>% filter(model == "cg")
mxl_pref_chg  <- filtered_df %>% filter(model == "chg")
mxl_pref_tgg  <- filtered_df %>% filter(model == "tgg")
mxl_pref_tghg <- filtered_df %>% filter(model == "tghg")



# ################################################################# #
# # PREFERENCE space
# ################################################################# #

# ################################################################# #
# #4. Gallon
# ################################################################# #


######################################################################################
#REGRESSION TABLE  PREF SPACE- GALLON
######################################################################################


# Function to create texreg objects from data frames
create_texreg_object <- function(model_data, model_name) {
  coef_names <- model_data$Variable
  coefs <- model_data$Coefficient
  ses <- model_data$Std.Error
  
  # Compute p-values manually
  pvals <- 2 * (1 - pnorm(abs(coefs / ses)))
  
  texreg_obj <- createTexreg(
    coef.names = coef_names,
    coef = coefs,
    se = ses,
    pvalues = pvals,
    model.name = model_name
  )
  
  return(texreg_obj)
}


# Create texreg objects for each model
texreg_objects_pref_g <- list(
  create_texreg_object(mxl_pref_cg, "Control"),
  create_texreg_object(mxl_pref_tgg, "Treatment")
)

# Define custom model names
custom_model_names_pref_g <- c("Control", "Treatment")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_pref_g <- c(
  "asc",
  "price",
  "almond_dum",
  "oat_dum",
  "soy_dum",
  "sd_asc",
  "sd_almond_dum",
  "sd_oat_dum",
  "sd_soy_dum"
)

# Create a custom coefficient map
custom_coef_map_pref_g <- list(
  "price" = "Price",
  "almond_dum" = "Almond milk",
  "oat_dum" = "Oat milk",
  "soy_dum" = "Soy milk",
  "asc" = "No-purchase",
  "sd_almond_dum" = "STDEV (Almond)",
  "sd_oat_dum" = "STDEV (Oat)",
  "sd_soy_dum" = "STDEV (Soy)",
  "sd_asc" = "STDEV (No-purchase)"
)

# Define the output path
output_path_pref_g <- "Analysis/Output/Tables/Nlogit_rpl_gallon_table_no_protein.tex"



# Create the table with texreg
texreg(texreg_objects_pref_g,
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = custom_model_names_pref_g,
       custom.coef.names = custom_variable_names_pref_g,
       custom.coef.map = custom_coef_map_pref_g,
       file = output_path_pref_g,
       caption = "Random parameter logit model in preference space, gallon consumers",
       label = "tab:Pref_g",
       table = FALSE)

# Read the generated .tex file
table_tex_pref_g <- readLines(output_path_pref_g)



# Remove .00 from integer GOF values in LaTeX output
table_tex_pref_g <- gsub("\\$([0-9]+)\\.00\\$", "\\$\\1\\$", table_tex_pref_g)



# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_pref_g, 20), sep = "\n")

# Add LaTeX commands to include the caption at the top of the table
table_tex_pref_g <- c("\\begin{table}[!htbp]",
                      "\\caption{Random parameter logit model in preference space, gallon consumers}",
                      "\\centering",
                      table_tex_pref_g,
                      "\\label{tab:Pref_g}",
                      "\\end{table}")

# Save the updated table to the file
cat(table_tex_pref_g, file = output_path_pref_g, sep = "\n")



# ################################################################# #
# # Half-gallon
# ################################################################# #

# Function to create texreg objects from data frames
create_texreg_object <- function(model_data, model_name) {
  coef_names <- model_data$Variable
  coefs <- model_data$Coefficient
  ses <- model_data$Std.Error
  
  # Compute p-values manually
  pvals <- 2 * (1 - pnorm(abs(coefs / ses)))
  
  texreg_obj <- createTexreg(
    coef.names = coef_names,
    coef = coefs,
    se = ses,
    pvalues = pvals,
    model.name = model_name
  )
  
  return(texreg_obj)
}

# Create texreg objects for each model
texreg_objects_pref_hg <- list(
  create_texreg_object(mxl_pref_chg, "Control"),
  create_texreg_object(mxl_pref_tghg, "Treatment")
)

# Define custom model names
custom_model_names_pref_hg <- c("Control", "Treatment")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_pref_hg <- c(
  "asc",
  "price",
  "almond_dum",
  "oat_dum",
  "soy_dum",
  "sd_asc",
  "sd_almond_dum",
  "sd_oat_dum",
  "sd_soy_dum"
)

# Create a custom coefficient map
custom_coef_map_pref_hg <- list(
  "price" = "Price",
  "almond_dum" = "Almond milk",
  "oat_dum" = "Oat milk",
  "soy_dum" = "Soy milk",
  "asc" = "No-purchase",
  "sd_almond_dum" = "STDEV (Almond)",
  "sd_oat_dum" = "STDEV (Oat)",
  "sd_soy_dum" = "STDEV (Soy)",
  "sd_asc" = "STDEV (No-purchase)"
)

# Define the output path
output_path_pref_hg <- "Analysis/Output/Tables/Nlogit_rpl_halfgal_table_no_protein.tex"

# Create the table with texreg
texreg(texreg_objects_pref_hg,
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = custom_model_names_pref_hg,
       custom.coef.names = custom_variable_names_pref_hg,
       custom.coef.map = custom_coef_map_pref_hg,
       file = output_path_pref_hg,
       caption = "Random parameter logit model in preference space, half-gallon consumers",
       label = "tab:Pref_hg",
       table = FALSE)

# Read the generated .tex file
table_tex_pref_hg <- readLines(output_path_pref_hg)

# Remove .00 from integer GOF values in LaTeX output
table_tex_pref_hg <- gsub("\\$([0-9]+)\\.00\\$", "\\$\\1\\$", table_tex_pref_hg)

# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_pref_hg, 20), sep = "\n")

# Add LaTeX commands to include the caption at the top of the table
table_tex_pref_hg <- c("\\begin{table}[!htbp]",
                       "\\caption{Random parameter logit model in preference space, half-gallon consumers}",
                       "\\centering",
                       table_tex_pref_hg,
                       "\\label{tab:Pref_hg}",
                       "\\end{table}")

# Save the updated table to the file
cat(table_tex_pref_hg, file = output_path_pref_hg, sep = "\n")

# ################################################################# #

# ################################################################# #
# # Combined Gallon and Half-gallon table
# ################################################################# #

# ################################################################# #


# Define the combined texreg objects list
texreg_objects_combined <- list(
  create_texreg_object(mxl_pref_cg, "Control"),
  create_texreg_object(mxl_pref_tgg, "Treatment"),
  create_texreg_object(mxl_pref_chg, "Control"),
  create_texreg_object(mxl_pref_tghg, "Treatment")
)

#Create a df with values from Nicole's word document with output from Nlogit
combined_gof_df <- data.frame(
  Model = c(
    "gallon_control",
    "gallon_treatment",
    "half_gal_control",
    "half_gal_treatment"
  ),
  LogLikelihood = c(-1573.77840, -1827.93084, -1368.73724, -1420.00273),
  AIC           = c(3177.6, 3685.9, 2767.5, 2870.0),
  Respondents   = c(187, 196, 153, 162),
  Observations  = c(2244, 2352, 1836, 1944)
)

#Convert the data to gof code
loglik_row <- combined_gof_df$LogLikelihood
aic_row    <- combined_gof_df$AIC
resp_row   <- combined_gof_df$Respondents
obs_row    <- combined_gof_df$Observations

loglik_row <- as.integer(loglik_row)
aic_row    <- as.integer(aic_row)
resp_row   <- as.integer(resp_row)
obs_row    <- as.integer(obs_row)

custom_gof_rows_combined <- list(
  "Log-Likelihood" = loglik_row,
  "AIC"            = aic_row,
  "Respondents"    = resp_row,
  "Observations"   = obs_row
)


# Generate texreg output and save to file
output_path_combined <- "Analysis/Output/Tables/Nlogit_rpl_combined_table_no_protein.tex"

texreg(
  l = texreg_objects_combined,
  custom.model.names = rep(c("Control", "Treatment"), 2),
  custom.coef.map = list(
    "price" = "Price",
    "almond_dum" = "Almond milk",
    "oat_dum" = "Oat milk",
    "soy_dum" = "Soy milk",
    "asc" = "No-purchase",
    "sd_almond_dum" = "STDEV (Almond)",
    "sd_oat_dum" = "STDEV (Oat)",
    "sd_soy_dum" = "STDEV (Soy)",
    "sd_asc" = "STDEV (No-purchase)"
  ),
  custom.gof.rows = custom_gof_rows_combined,
  stars = c(0.01, 0.05, 0.1),
  table = FALSE,
  file = output_path_combined
)

# Step 2: Read and clean the LaTeX file
table_tex <- readLines(output_path_combined)

# Fix ampersands
table_tex <- gsub("&amp;", "&", table_tex)

# Remove .00 from integers
table_tex <- gsub("\\$([0-9]+)\\.00\\$", "\\$\\1\\$", table_tex)

# Find the header line (model names)
header_index <- grep("Control.*Treatment", table_tex)

# Replace the header with a custom two-row header
if (length(header_index) == 1) {
  table_tex <- append(
    table_tex[-header_index],  # Remove the old header
    values = c(
      " & \\multicolumn{2}{c}{Gallon consumers} & \\multicolumn{2}{c}{Half-gallon consumers} \\\\",
      "\\cline{2-3} \\cline{4-5}",
      "Attribute & Control & Treatment & Control & Treatment \\\\"
    ),
    after = header_index - 1
  )
}

# Remove texreg's default star line
table_tex <- table_tex[!grepl("\\\\multicolumn\\{.*\\}.*p<", table_tex)]

# Optional: fix alignment if needed
#table_tex <- gsub("\\\\begin\\{tabular\\}\\{l c c c c \\}", "\\\\begin{tabular}{l l l l l }", table_tex)

# Wrap in table environment
table_tex <- c(
  "\\begin{table}[!htbp]",
  "\\caption{Random parameter logit model in preference space by treatment group}",
  "\\label{tab:rpl_combined}",
  "\\centering",
  "\\begin{threeparttable}",
  table_tex,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item ***p< 0.01; **p<0.05; *p< 0.1. Standard errors in parenthesis.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# Save the updated table
cat(table_tex, file = output_path_combined, sep = "\n")

