###########################################################################################
#WTP TABLES FROM RPL models using imported preference space coefficient estimates 
#from Nlogit DAIRY AS REFERENCE LEVEL
##########################################################################################
#Calculate the wtp and construct the tables for the RPL preference space models from Nlogit
#Agnes af Sandeberg 18 DECEMBER 2025

#1. read packages
rm(list = ls())
# List of required packages
required_packages<-c("data.table", "plyr","dplyr", "tidyr", "lubridate", "support.CEs", "readxl",
                     "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "logitr", "survival", 
                     "stats", "haven", "aod", "survival", "dfidx", "lmtest", "texreg", 
                     "kableExtra", "stargazer", "remotes", "cmdlr", "mded", "zoo", 
                     "xtable", "mvtnorm", "purrr", "Matrix")
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
output_path_tables <- "Analysis/Output/Tables"

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

# Loop through datasets
for (name in names(model_suffix_map)) {
  df <- get(name)
  suffix <- model_suffix_map[[name]]
  
  # Extract COEFFICIENT
  coeffs <- df %>%
    filter(Variable %in% c("CDUM", "TYPE_ALM", "TYPE_OAT", "TYPE_SOY", "sdCDUM", "sdTYPE_A", "sdTYPE_O", "sdTYPE_S")) %>%
    transmute(
      Model = paste0("mxl_pref_", suffix),
      Attribute = case_when(
        Variable == "CDUM" ~ "asc",
        Variable == "TYPE_ALM" ~ "almond_dum",
        Variable == "TYPE_OAT" ~ "oat_dum",
        Variable == "TYPE_SOY" ~ "soy_dum",
        Variable == "sdCDUM" ~ "sd_asc",
        Variable == "sdTYPE_A" ~ "sd_almond_dum",
        Variable == "sdTYPE_O" ~ "sd_oat_dum",
        Variable == "sdTYPE_S" ~ "sd_soy_dum"
      ),
      Estimate = Coefficient,
      Std_Error = Std.Error
    )
  
  coeff_list[[name]] <- coeffs
}

# Combine into final data frames
coeff_df <- bind_rows(coeff_list)


# Split into four datasets
mxl_pref_cg   <- coeff_df %>% filter(Model == "mxl_pref_cg")
mxl_pref_chg  <- coeff_df %>% filter(Model == "mxl_pref_chg")
mxl_pref_tgg  <- coeff_df %>% filter(Model == "mxl_pref_tgg")
mxl_pref_tghg <- coeff_df %>% filter(Model == "mxl_pref_tghg")


################################################################
#Import covariance matrices
################################################################

# Define covariance sheet names
cov_sheet_names <- c(
  "Control_Gallon_Cov",
  "Control_Half_Gallon_Cov",
  "General_Gallon_Cov",
  "General_Half_Gallon_Cov"
)


# Read all covariance sheets into a named list
cov_list <- lapply(cov_sheet_names, function(sheet) {
  read_excel(rpl_models, sheet = sheet)
})
names(cov_list) <- cov_sheet_names



#################################################################
#Recreate variance-covariance matrices
################################################################

cov_suffix_map <- list(
  "Control_Gallon_Cov" = "cg",
  "Control_Half_Gallon_Cov" = "chg",
  "General_Gallon_Cov" = "tgg",
  "General_Half_Gallon_Cov" = "tghg"
)

# Clean and store covariance matrices
cov_matrix_list <- list()

for (name in names(cov_suffix_map)) {
  suffix <- cov_suffix_map[[name]]
  raw_cov <- cov_list[[name]]
  clean_cov <- raw_cov[, -1] |> as.matrix()
  rownames(clean_cov) <- raw_cov[[1]]
  cov_matrix_list[[suffix]] <- clean_cov
}



# Function to extract SDs from each model's WTP dataframe
extract_sds <- function(df) {
  df %>%
    filter(grepl("^sd_", Attribute)) %>%
    arrange(match(Attribute, c("sd_asc", "sd_almond_dum", "sd_oat_dum", "sd_soy_dum"))) %>%
    pull(Estimate)
}

# Create a named list of SD vectors
sds_list <- list(
  cg   = extract_sds(mxl_pref_cg),
  chg  = extract_sds(mxl_pref_chg),
  tgg  = extract_sds(mxl_pref_tgg),
  tghg = extract_sds(mxl_pref_tghg)
)


# ################################################################# #
# # Krinsky-Robb confidence intervals for our wtp estimates
# ################################################################# #


calculate_wtp <- function(coefs, price_var = "PRICE_VA") {
  price <- coefs[price_var]
  wtp <- c(
    asc = -coefs["CDUM"] / price,
    almond_dum = -2 * coefs["TYPE_ALM"] / price,
    oat_dum = -2 * coefs["TYPE_OAT"] / price,
    soy_dum = -2 * coefs["TYPE_SOY"] / price
  )
  return(wtp)
}

wtp_delta_cov <- function(coefs, cov_matrix, price_var = "PRICE_VA") {
  price <- coefs[price_var]
  
  # Gradient matrix (Jacobian)
  grad <- matrix(0, nrow = 4, ncol = length(coefs))
  colnames(grad) <- names(coefs)
  rownames(grad) <- c("asc", "almond_dum", "oat_dum", "soy_dum")
  
  grad["asc", "CDUM"] <- -1 / price
  grad["asc", price_var] <- coefs["CDUM"] / price^2
  
  grad["almond_dum", "TYPE_ALM"] <- -2 / price
  grad["almond_dum", price_var] <- 2 * coefs["TYPE_ALM"] / price^2
  
  grad["oat_dum", "TYPE_OAT"] <- -2 / price
  grad["oat_dum", price_var] <- 2 * coefs["TYPE_OAT"] / price^2
  
  grad["soy_dum", "TYPE_SOY"] <- -2 / price
  grad["soy_dum", price_var] <- 2 * coefs["TYPE_SOY"] / price^2
  
  # Delta method: Var(WTP) = J * Cov(beta) * J'
  wtp_cov <- grad %*% cov_matrix %*% t(grad)
  return(wtp_cov)
}


set.seed(123)

# Krinsky-Robb summary function
krinsky_robb_wtp <- function(wtp_means, wtp_cov, n_sim = 10000) {
  wtp_cov_pd <- as.matrix(nearPD(wtp_cov)$mat)
  
  draws <- MASS::mvrnorm(n = n_sim, mu = wtp_means, Sigma = wtp_cov_pd, tol = 1e-6)
  
  as.data.frame(draws) %>%
    pivot_longer(cols = everything(), names_to = "Attribute", values_to = "WTP") %>%
    group_by(Attribute) %>%
    summarise(
      lower = quantile(WTP, 0.025),
      upper = quantile(WTP, 0.975),
      mean = mean(WTP),
      .groups = "drop"
    )
}

# Krinsky-Robb raw draws function
krinsky_robb_draws <- function(wtp_means, wtp_cov, n_sim = 10000) {
  wtp_cov_pd <- as.matrix(nearPD(wtp_cov)$mat)
  MASS::mvrnorm(n = n_sim, mu = wtp_means, Sigma = wtp_cov_pd)
}

# Store results
kr_results_list <- list()
kr_draws_list <- list()

# Loop through each model
for (model_name in names(model_suffix_map)) {
  suffix <- model_suffix_map[[model_name]]
  
  # Extract coefficients
  df <- data_list[[model_name]]
  coefs <- df %>%
    filter(Variable %in% c("CDUM", "TYPE_ALM", "TYPE_OAT", "TYPE_SOY", "PRICE_VA")) %>%
    select(Variable, Coefficient) %>%
    deframe()
  
  # Extract covariance matrix
  cov_matrix <- cov_matrix_list[[suffix]]
  vars <- c("CDUM","TYPE_ALM","TYPE_OAT","TYPE_SOY","PRICE_VA")
  
  # Ensure required variables are present
  if (!all(c(vars) %in% rownames(cov_matrix))) {
    warning(paste("Skipping model", model_name, "- covariance matrix missing required variables."))
    next
  }
  
  # Subset covariance to the coefficients used
  coef_cov <- cov_matrix[vars, vars, drop = FALSE]
  
  
  # Calculate WTP and covariance
  wtp_means <- calculate_wtp(coefs)
  wtp_cov <- wtp_delta_cov(coefs, coef_cov)
  
  # Store raw draws
  kr_draws_list[[suffix]] <- krinsky_robb_draws(wtp_means, wtp_cov)
  
  # Run Krinsky-Robb simulation
  kr_result <- krinsky_robb_wtp(wtp_means, wtp_cov)
  kr_result$Model <- paste0("mxl_pref_", suffix)
  
  # Store summary results
  kr_results_list[[suffix]] <- kr_result
}

# Combine all results into one dataframe
kr_results_df <- bind_rows(kr_results_list)

# Clean up attribute names
kr_results_df <- kr_results_df %>%
  mutate(Attribute = case_when(
    Attribute == "CDUM" ~ "asc",
    Attribute == "TYPE_ALM" ~ "almond_dum",
    Attribute == "TYPE_OAT" ~ "oat_dum",
    Attribute == "TYPE_SOY" ~ "soy_dum",
    TRUE ~ Attribute
  )) %>%
  mutate(Attribute = sub("\\..*$", "", Attribute))

# Merge with wtp coefficient data
wtp_table_df <- kr_results_df %>%
  mutate(
    Model = recode(Model,
                   mxl_pref_cg  = "mxl_pref_cg",
                   mxl_pref_tgg = "mxl_pref_tgg")
  ) %>%
  select(Model, Attribute, mean, lower, upper) %>%
  rename(
    Estimate = mean
  )



# ################################################################# #
# # Gallon estimations
# ################################################################# #

# ################################################################# #
# # WTP coefficients poe test gallon
# ################################################################# #

poe_test <- function(draws1, draws2) {
  mean(draws1 > draws2)
}

attributes <- c("asc", "almond_dum", "oat_dum", "soy_dum")
poe_results_g <- matrix(NA, nrow = length(attributes), ncol = 1)
colnames(poe_results_g) <- c("C>T")
rownames(poe_results_g) <- attributes

# Extract draws
draws_cg <- kr_draws_list[["cg"]]
draws_tgg <- kr_draws_list[["tgg"]]


for (i in seq_along(attributes)) {
  poe_results_g[i, "C>T"] <- poe_test(draws_cg[, i], draws_tgg[, i])
}

print(round(poe_results_g, 1))

# ################################################################# #
# # WTP estimates, gallon table add poe test p-values
# ################################################################# #

# Convert POE matrix to data frame
poe_stats_g_df <- as.data.frame(poe_results_g)

# Add attribute names as a proper column
poe_stats_g_df$Attribute <- rownames(poe_stats_g_df)

# Reorder to match desired output
desired_order <- c("almond_dum", "oat_dum", "soy_dum", "asc")
poe_stats_g_df <- poe_stats_g_df[match(desired_order, poe_stats_g_df$Attribute), ]

# Convert only numeric columns
poe_stats_g_df[, "C>T"] <- as.numeric(poe_stats_g_df[, "C>T"])


# Function to create texreg objects from data frames
create_texreg_object <- function(model_data, model_name) {
  coef_names <- model_data$Attribute
  coefs <- model_data$Estimate
  
  # Estimate standard error from 95% CI: SE ≈ (upper - lower) / (2 * 1.96)
  ses <- (model_data$upper - model_data$lower) / (2 * 1.96)
  
  # Compute p-values assuming normality
  pvals <- 2 * (1 - pnorm(abs(coefs / ses)))
  
  new("texreg",
      coef.names = coef_names,
      coef = coefs,
      se = ses,
      pvalues = pvals,
      model.name = model_name
  )
}


# List of model names
model_names_g <- c("mxl_pref_cg", "mxl_pref_tgg")


# Create texreg objects for each model
texreg_objects_wtp_g <- lapply(seq_along(model_names_g), function(i) {
  model_name <- model_names_g[i]
  model_data <- subset(wtp_table_df, Model == model_name)
  create_texreg_object(model_data, model_name)
})


epsilon <- 1e-6
se_dummy <- rep(epsilon, nrow(poe_stats_g_df))

poe_control_vs_general_texreg <- new("texreg",
                                     coef.names = as.character(poe_stats_g_df$Attribute),
                                     coef = as.numeric(poe_stats_g_df[["C>T"]]),
                                     se = numeric(0), # Remove SEs
                                     pvalues = rep(1, nrow(poe_stats_g_df)),
                                     model.name = "C>T"
)



# Define custom model names
custom_model_names_g <- c("Control (C)", "Treatment (T)", "C>T")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_g <- c(
  "asc",
  "almond_dum",
  "oat_dum" ,
  "soy_dum",
  "sd_asc",
  "sd_almond_dum",
  "sd_oat_dum",
  "sd_soy_dum"
)

# Create a custom coefficient map
custom_coef_map_g <- list(
  "almond_dum" = "Almond milk",
  "oat_dum" = "Oat milk",
  "soy_dum" = "Soy milk",
  "asc" = "No-purchase"  
)


# Number of models
n_models <- length(texreg_objects_wtp_g) + 1 # 1 POE model


# Combine all texreg objects into one list
all_texreg_objects_g <- c(
  texreg_objects_wtp_g, 
  list(
    poe_control_vs_general_texreg
  )
)


# Define the output path
output_path_g <- "Analysis/Output/Tables/Nlogit_wtp_gallon_table_no_protein.tex"


# Create the table with texreg (without caption, label, or note)
texreg(
  l = all_texreg_objects_g,
  custom.model.names = custom_model_names_g,
  custom.coef.map = custom_coef_map_g,
  file = output_path_g,
  caption = NULL,  # I add caption manually
  label = NULL,    # I  add label manually
  custom.note = NULL, #I add table note manually
  stars = NULL,  # Disable significance stars legend
  ci.force = c(FALSE, FALSE, FALSE),
  table = FALSE    # Keep it as tabular only
)

wtp_lookup <- wtp_table_df %>%
  mutate(
    coef = sprintf("%.2f", Estimate),
    ci   = sprintf("\\left[%.2f,\\,%.2f\\right]", lower, upper)
  ) %>%
  select(Model, Attribute, coef, ci)



# Read the LaTeX file into memory
table_tex_g <- readLines(output_path_g)


for (attr in unique(wtp_lookup$Attribute)) {
  
  label <- custom_coef_map_g[[attr]]
  if (is.null(label)) next
  
  row_idx <- grep(paste0("^\\s*", label, "\\s*&"), table_tex_g)
    if (length(row_idx) != 1) next
  
  coef_C <- wtp_lookup %>%
    filter(Model == "mxl_pref_cg", Attribute == attr) %>%
    pull(coef)
  
  coef_G <- wtp_lookup %>%
    filter(Model == "mxl_pref_tgg", Attribute == attr) %>%
    pull(coef)
  
  ci_C <- wtp_lookup %>%
    filter(Model == "mxl_pref_cg", Attribute == attr) %>%
    pull(ci)
  
  ci_G <- wtp_lookup %>%
    filter(Model == "mxl_pref_tgg", Attribute == attr) %>%
    pull(ci)
  
  # First row: coefficients (keep label)
  parts <- strsplit(table_tex_g[row_idx], " & ")[[1]]
  parts[2] <- paste0("$", coef_C, "$")
  parts[3] <- paste0("$", coef_G, "$")
  table_tex_g[row_idx] <- paste(parts, collapse = " & ")
  
  # Second row: confidence intervals (no label)
  ci_row <- paste0(
    " & $", ci_C, "$ & $", ci_G, "$ & \\\\"
  )
  
  # Insert CI row directly below
  table_tex_g <- append(table_tex_g, ci_row, after = row_idx)
}



# Remove texreg-generated SD-only rows
table_tex_g <- table_tex_g[!grepl("^\\s*& \\$\\(", table_tex_g)]

table_tex_g <- gsub("\\$\\$", "~", table_tex_g) # Replace $$ with ~

# Remove .00 from integer GOF values
table_tex_g <- gsub("\\$([0-9]+)\\.00\\$", "\\$\\1\\$", table_tex_g)


# Replace $0.00$ with $<0.01$ in POE columns
table_tex_g <- gsub("\\$(0|0\\.00)\\$", "\\$<0.01\\$", table_tex_g)

# Replace $1.00$ with $>0.99$ in POE columns
table_tex_g <- gsub("\\$1\\$", "\\$>0.99\\$", table_tex_g)

# Remove texreg's default CI note line
table_tex_g <- table_tex_g[!grepl("\\\\multicolumn\\{.*Null hypothesis value outside the confidence interval", table_tex_g)]


# Find the index of the header row
header_index <- which(grepl("Control \\(C\\).*Treatment \\(T\\).*C>T", table_tex_g))

# Replace the old header with the new two-row header
if (length(header_index) == 1) {
  table_tex_g <- append(
    table_tex_g[-header_index], # Remove the old header
    values = c(
      " & Control (C) & Treatment (T) & \\multicolumn{1}{c}{Poe test (p-value)} \\\\",
      "\\cline{4-4}",
      " Milk Type$^a$ &\\multicolumn{1}{l}{\\$/gallon} &\\multicolumn{1}{l}{\\$/gallon} & \\multicolumn{1}{c}{C>T$^b$}  \\\\"
    ),
    after = header_index - 1
  )
}


# Replace column alignment from 'l c c c ' to 'l l l  c '
table_tex_g <- gsub("\\\\begin\\{tabular\\}\\{l c c c \\}", "\\\\begin{tabular}{l l l c }", table_tex_g)


# Insert spacing before the final \hline
hline_indices <- grep("^\\\\hline$", table_tex_g)
if (length(hline_indices) > 0) {
  # Insert the rule before the last \hline
  table_tex_g <- append(
    table_tex_g,
    values = "\\addlinespace[0.5ex]",
    after = hline_indices[length(hline_indices)] - 1
  )
}

# Remove texreg's tabular environment lines
table_tex_g <- table_tex_g[!grepl("^\\\\begin\\{tabular\\}|^\\\\end\\{tabular\\}", table_tex_g)]

#Remove unintended spaces
table_tex_g <- table_tex_g[!grepl("^\\s*$|\\\\par", table_tex_g)]

# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_g, 20), sep = "\n")

# Wrap the table in a threeparttable environment
wrapped_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Willingness to pay for milk attributes, gallon consumers}",
  "\\label{tab:WTP_g}",
  "\\begin{tabular}{l l l c }",  # Adjust alignment dynamically if needed
  table_tex_g,                        # Insert texreg table content
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item $^a$ Dairy milk is the reference alternative. 95\\% confidence intervals in brackets.",
  "\\item $^b$ P-values from Poe tests, where values >0.95 or < 0.05 indicate different WTP at the 5 per
cent confidence level.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# Save the wrapped table back to file
writeLines(wrapped_table, output_path_g)



###################END OF GALLON TABLE ########################################################################


# ################################################################# #
# # Half-gallon estimations
# ################################################################# #

# ################################################################# #
# # WTP coefficients poe test half-gallon
# ################################################################# #
poe_test <- function(draws1, draws2) {
  mean(draws1 > draws2)
}

attributes <- c("asc", "almond_dum", "oat_dum", "soy_dum")
poe_results_hg <- matrix(NA, nrow = length(attributes), ncol = 1)
colnames(poe_results_hg) <- c("C>T")
rownames(poe_results_hg) <- attributes

# Extract draws
draws_chg <- kr_draws_list[["chg"]]
draws_tghg <- kr_draws_list[["tghg"]]

for (i in seq_along(attributes)) {
  poe_results_hg[i, "C>T"] <- poe_test(draws_chg[, i], draws_tghg[, i])
}

print(round(poe_results_hg, 1))

# ################################################################# #
# # WTP estimates, half-gallon table add poe test p-values
# ################################################################# #

# Convert POE matrix to data frame
poe_stats_hg_df <- as.data.frame(poe_results_hg)

# Add attribute names as a proper column
poe_stats_hg_df$Attribute <- rownames(poe_stats_hg_df)

# Reorder to match desired output
desired_order <- c("almond_dum", "oat_dum", "soy_dum", "asc")
poe_stats_hg_df <- poe_stats_hg_df[match(desired_order, poe_stats_hg_df$Attribute), ]

# Convert only numeric columns
poe_stats_hg_df[, "C>T"] <- as.numeric(poe_stats_hg_df[, "C>T"])


# Function to create texreg objects from data frames
create_texreg_object <- function(model_data, model_name) {
  coef_names <- model_data$Attribute
  coefs <- model_data$Estimate
  
  # Estimate standard error from 95% CI: SE ≈ (upper - lower) / (2 * 1.96)
  ses <- (model_data$upper - model_data$lower) / (2 * 1.96)
  
  # Compute p-values assuming normality
  pvals <- 2 * (1 - pnorm(abs(coefs / ses)))
  
  new("texreg",
      coef.names = coef_names,
      coef = coefs,
      se = ses,
      pvalues = pvals,
      model.name = model_name
  )
}


# List of model names
model_names_hg <- c("mxl_pref_chg", "mxl_pref_tghg")


# Create texreg objects for each model
texreg_objects_wtp_hg <- lapply(seq_along(model_names_hg), function(i) {
  model_name <- model_names_hg[i]
  model_data <- subset(wtp_table_df, Model == model_name)
  create_texreg_object(model_data, model_name)
})


epsilon <- 1e-6
se_dummy <- rep(epsilon, nrow(poe_stats_hg_df))

poe_control_vs_general_texreg <- new("texreg",
                                     coef.names = as.character(poe_stats_hg_df$Attribute),
                                     coef = as.numeric(poe_stats_hg_df[["C>T"]]),
                                     se = numeric(0), # Remove SEs
                                     pvalues = rep(1, nrow(poe_stats_hg_df)),
                                     model.name = "C>T"
)



# Define custom model names
custom_model_names_hg <- c("Control (C)", "Treatment (T)", "C>T")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_hg <- c(
  "asc",
  "almond_dum",
  "oat_dum" ,
  "soy_dum",
  "sd_asc",
  "sd_almond_dum",
  "sd_oat_dum",
  "sd_soy_dum"
)

# Create a custom coefficient map
custom_coef_map_hg <- list(
  "almond_dum" = "Almond milk",
  "oat_dum" = "Oat milk",
  "soy_dum" = "Soy milk",
  "asc" = "No-purchase"  
)


# Number of models
n_models <- length(texreg_objects_wtp_hg) + 1 # 1 POE model


# Combine all texreg objects into one list
all_texreg_objects_hg <- c(
  texreg_objects_wtp_hg, 
  list(
    poe_control_vs_general_texreg
  )
)


# Define the output path
output_path_hg <- "Analysis/Output/Tables/Nlogit_wtp_halfgal_table_no_protein.tex"


# Create the table with texreg (without caption, label, or note)
texreg(
  l = all_texreg_objects_hg,
  custom.model.names = custom_model_names_hg,
  custom.coef.map = custom_coef_map_hg,
  file = output_path_hg,
  caption = NULL,  # I add caption manually
  label = NULL,    # I  add label manually
  custom.note = NULL, #I add table note manually
  stars = NULL,  # Disable significance stars legend
  ci.force = c(FALSE, FALSE, FALSE),
  table = FALSE    # Keep it as tabular only
)

wtp_lookup <- wtp_table_df %>%
  mutate(
    coef = sprintf("%.2f", Estimate),
    ci   = sprintf("\\left[%.2f,\\,%.2f\\right]", lower, upper)
  ) %>%
  select(Model, Attribute, coef, ci)



# Read the LaTeX file into memory
table_tex_hg <- readLines(output_path_hg)


for (attr in unique(wtp_lookup$Attribute)) {
  
  label <- custom_coef_map_hg[[attr]]
  if (is.null(label)) next
  
  row_idx <- grep(paste0("^\\s*", label, "\\s*&"), table_tex_hg)
  if (length(row_idx) != 1) next
  
  coef_C <- wtp_lookup %>%
    filter(Model == "mxl_pref_chg", Attribute == attr) %>%
    pull(coef)
  
  coef_G <- wtp_lookup %>%
    filter(Model == "mxl_pref_tghg", Attribute == attr) %>%
    pull(coef)
  
  ci_C <- wtp_lookup %>%
    filter(Model == "mxl_pref_chg", Attribute == attr) %>%
    pull(ci)
  
  ci_G <- wtp_lookup %>%
    filter(Model == "mxl_pref_tghg", Attribute == attr) %>%
    pull(ci)
  
  # First row: coefficients (keep label)
  parts <- strsplit(table_tex_hg[row_idx], " & ")[[1]]
  parts[2] <- paste0("$", coef_C, "$")
  parts[3] <- paste0("$", coef_G, "$")
  table_tex_hg[row_idx] <- paste(parts, collapse = " & ")
  
  # Second row: confidence intervals (no label)
  ci_row <- paste0(
    " & $", ci_C, "$ & $", ci_G, "$ & \\\\"
  )
  
  # Insert CI row directly below
  table_tex_hg <- append(table_tex_hg, ci_row, after = row_idx)
}



# Remove texreg-generated SD-only rows
table_tex_hg <- table_tex_hg[!grepl("^\\s*& \\$\\(", table_tex_hg)]

table_tex_hg <- gsub("\\$\\$", "~", table_tex_hg) # Replace $$ with ~

# Remove .00 from integer GOF values
table_tex_hg <- gsub("\\$([0-9]+)\\.00\\$", "\\$\\1\\$", table_tex_hg)


# Replace $0.00$ with $<0.01$ in POE columns
table_tex_hg <- gsub("\\$(0|0\\.00)\\$", "\\$<0.01\\$", table_tex_hg)

# Replace $1.00$ with $>0.99$ in POE columns
table_tex_hg <- gsub("\\$1\\$", "\\$>0.99\\$", table_tex_hg)

# Remove texreg's default CI note line
table_tex_hg <- table_tex_hg[!grepl("\\\\multicolumn\\{.*Null hypothesis value outside the confidence interval", table_tex_hg)]


# Find the index of the header row
header_index <- which(grepl("Control \\(C\\).*Treatment \\(T\\).*C>T", table_tex_hg))

# Replace the old header with the new two-row header
if (length(header_index) == 1) {
  table_tex_hg <- append(
    table_tex_hg[-header_index], # Remove the old header
    values = c(
      " & Control (C) & Treatment (T) & \\multicolumn{1}{c}{Poe test (p-value)} \\\\",
      "\\cline{4-4}",
      " Milk Type$^a$ &\\multicolumn{1}{l}{\\$/half-gallon} &\\multicolumn{1}{l}{\\$/half-gallon} & \\multicolumn{1}{c}{C>T$^b$}  \\\\"
    ),
    after = header_index - 1
  )
}



# Replace column alignment from 'l c c c ' to 'l l l  c '
table_tex_hg <- gsub("\\\\begin\\{tabular\\}\\{l c c c \\}", "\\\\begin{tabular}{l l l c }", table_tex_hg)


# Insert spacing before the final \hline
hline_indices <- grep("^\\\\hline$", table_tex_hg)
if (length(hline_indices) > 0) {
  # Insert the rule before the last \hline
  table_tex_hg <- append(
    table_tex_hg,
    values = "\\addlinespace[0.5ex]",
    after = hline_indices[length(hline_indices)] - 1
  )
}

# Remove texreg's tabular environment lines
table_tex_hg <- table_tex_hg[!grepl("^\\\\begin\\{tabular\\}|^\\\\end\\{tabular\\}", table_tex_hg)]

#Remove unintended spaces
table_tex_hg <- table_tex_hg[!grepl("^\\s*$|\\\\par", table_tex_hg)]

# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_hg, 20), sep = "\n")

# Wrap the table in a threeparttable environment
wrapped_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Willingness to pay for milk attributes, half-gallon consumers}",
  "\\label{tab:WTP_hg}",
  "\\begin{tabular}{l l l  c }",  # Adjust alignment dynamically if needed
  table_tex_hg,                        # Insert texreg table content
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item $^a$ Dairy milk is the reference alternative. 95\\% confidence intervals in brackets.",
  "\\item $^b$ P-values from Poe tests, where values >0.95 or < 0.05 indicate different WTP at the 5 per
cent confidence level.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# Save the wrapped table back to file
writeLines(wrapped_table, output_path_hg)


