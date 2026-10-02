#####################################################
#Datasetup for MNL and LR using logitr in R, 
#we are using effects coding in this file
#####################################################
#Run the logitr package and estimate the models
#Agnes af Sandeberg 

#1. read packages
rm(list = ls())
# List of required packages
required_packages=c("data.table", "plyr","dplyr", "lubridate", "support.CEs", "readxl", 
                    "openxlsx", "tidyverse", "idefix", "MASS", "mlogit", "logitr", "survival", 
                    "stats", "haven", "aod", "survival", "dfidx", "lmtest", "texreg", "stringr")
# Install missing packages and load them
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg) # Install if not already installed
  }
  library(pkg, character.only = TRUE) # Load the package
}



# ################################################################# #
#### LOAD PANEL DATA AND APPLY ANY TRANSFORMATIONS               ####
# ################################################################# #
#2. Set working directory
path <- Sys.getenv("PWD")
print(path)
setwd("C:/Users/asor0002/git/DCE_data_management")

#3. load the data
full_sample <- read_dta("Analysis/Input/DCE_ready_for_analysis.dta")

#4. Create my subsamples
#Move my index columns first
full_sample <- full_sample %>%
  select(str,alt, everything())
full_sample <- full_sample %>%
  filter(protein_treat != 1)

control_sample <- subset(full_sample, control == 1)
treat_general <- subset(full_sample, general_treat== 1)

#5. Create my gallon/half-gallon subsamples
control_gallon <- subset(control_sample, gallon == 1)
control_halfgal <- subset(control_sample, gallon == 0)
treat_gen_gallon <- subset(treat_general, gallon == 1)
treat_gen_halfgal <- subset(treat_general, gallon == 0)

#Create my full gallon and full half-gallon sample
gallon_full <- subset(full_sample, gallon == 1)
halfgal_full <- subset(full_sample, gallon == 0)


###################################################################################
#CONTROL
###################################################################################
#6.1 Run basic MNL models for the gallon/half-gallon samples and see if we can pool them (for each of the control and treatment groups)
#First the pooled model for the control group with gallon and half-gallon consumers combined
#Mixed logit models
#The model in preference space

mnl_pref_cpool <- logitr(
  data    = control_sample,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_cpool)

# Extract the log-likelihood score for the pooled model
log_lik_cpool <- logLik(mnl_pref_cpool)


mnl_pref_cg <- logitr(
  data    = control_gallon,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_cg)


# Extract the log-likelihood score for the gallon model
log_lik_cg <- logLik(mnl_pref_cg)


mnl_pref_chg <- logitr(
  data    = control_halfgal,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_chg)

# Extract the log-likelihood score for the half-gallon model
log_lik_chg <- logLik(mnl_pref_chg)


# Likelihood ratio test

# Define the samples using the log-likelihood scores
pooledll <- as.numeric(log_lik_cpool) 
gallonll <- as.numeric(log_lik_cg)
halfgalll <- as.numeric(log_lik_chg)

# Calculate the likelihood ratio test statistic
LRT_stat_control <- -2 * (pooledll - (gallonll + halfgalll))

# Print the result
print(LRT_stat_control)


# Interpretation of likelihood ratio test results
# Calculate the degrees of freedom
df_cont <- (length(coef(mnl_pref_cg)) + length(coef(mnl_pref_chg))) - length(coef(mnl_pref_cpool))


# Define the critical value for the chi-squared test with df degrees of freedom at 0.05 significance level
critical_value_cont <- qchisq(0.95, df = df_cont)


# Interpretation of likelihood ratio test results
if (LRT_stat_control > critical_value_cont) {
  cat("Reject the null hypothesis. The models cannot be pooled.\n")
} else {
  cat("Fail to reject the null hypothesis. The models can be pooled.\n")
}

###################################################################################
#GENERAL TREATMENT
###################################################################################
#6.2 For the general treatment: Run basic MNL models for the gallon/half-gallon samples and see if we can pool them (for each of the control and treatment groups)
#First the pooled model for the general treatment group with gallon and half-gallon consumers combined


mnl_pref_tgpool <- logitr(
  data    = treat_general,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_tgpool)

# Extract the log-likelihood score for the pooled model
log_lik_tgpool <- logLik(mnl_pref_tgpool)


mnl_pref_tgg <- logitr(
  data    = treat_gen_gallon,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_tgg)

# Extract the log-likelihood score for the gallon model
log_lik_tgg <- logLik(mnl_pref_tgg)


mnl_pref_tghg <- logitr(
  data    = treat_gen_halfgal,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_tghg)

# Extract the log-likelihood score for the half-gallon model
log_lik_tghg <- logLik(mnl_pref_tghg)


# Likelihood ratio test

# Define the samples using the log-likelihood scores
tgpooledll <- as.numeric(log_lik_tgpool) 
tggallonll <- as.numeric(log_lik_tgg)
tghalfgalll <- as.numeric(log_lik_tghg)

# Calculate the likelihood ratio test statistic
LRT_stat_tg <- -2 * (tgpooledll - (tggallonll + tghalfgalll))

# Print the result
print(LRT_stat_tg)


# Interpretation of likelihood ratio test results
# Calculate the degrees of freedom
df_tg <- (length(coef(mnl_pref_tgg)) + length(coef(mnl_pref_tghg))) - length(coef(mnl_pref_tgpool))


# Define the critical value for the chi-squared test with df degrees of freedom at 0.05 significance level
critical_value_tg <- qchisq(0.95, df = df_tg)


# Interpretation of likelihood ratio test results
if (LRT_stat_tg > critical_value_tg) {
  cat("Reject the null hypothesis. The models cannot be pooled.\n")
} else {
  cat("Fail to reject the null hypothesis. The models can be pooled.\n")
}


###################################################################################
#TEST POOLING OF TREATMENTS AND CONTROL FOR GALLON CONSUMERS
###################################################################################
#7.1 Test if we can pool our treatment and control samples for our gallon respondents

mnl_pref_infogpool <- logitr(
  data    = gallon_full,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_infogpool)

# Extract the log-likelihood score for the pooled gallon model
log_lik_infogpool <- logLik(mnl_pref_infogpool)


# Control gallon model
mnl_pref_contg <- logitr(
  data    = control_gallon,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_contg)

# Extract the log-likelihood score for the control gallon model
log_lik_contg <- logLik(mnl_pref_contg)

#Treatment gallon model
mnl_pref_gtgal <- logitr(
  data    = treat_gen_gallon,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_gtgal)

# Extract the log-likelihood score for the  general treat gallon model
log_lik_gtgal <- logLik(mnl_pref_gtgal)


# Likelihood ratio test

# Define the samples using the log-likelihood scores
infogpoolll <- as.numeric(log_lik_infogpool) 
gcongallonll <- as.numeric(log_lik_contg)
ggtgallonll <- as.numeric(log_lik_gtgal)

# Calculate the likelihood ratio test statistic
LRT_stat_infogallon <- -2 * (infogpoolll - (gcongallonll + ggtgallonll))

# Print the result
print(LRT_stat_infogallon)


# Interpretation of likelihood ratio test results
# Calculate the degrees of freedom
df_infog <- (length(coef(mnl_pref_contg)) + length(coef(mnl_pref_gtgal))) - length(coef(mnl_pref_infogpool))


# Define the critical value for the chi-squared test with df degrees of freedom at 0.05 significance level
critical_value_infog <- qchisq(0.95, df = df_infog)


# Interpretation of likelihood ratio test results
if (LRT_stat_infogallon > critical_value_infog) {
  cat("Reject the null hypothesis. The models cannot be pooled.\n")
} else {
  cat("Fail to reject the null hypothesis. The models can be pooled.\n")
}


###################################################################################
#TEST POOLING OF TREATMENTS AND CONTROL HALF-GALLON
###################################################################################
#7.2 Test if we can pool our treatment and control samples for our half-gallon respondents


mnl_pref_infohgpool <- logitr(
  data    = halfgal_full,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_infohgpool)

# Extract the log-likelihood score for the pooled half-gallon model
log_lik_infohgpool <- logLik(mnl_pref_infohgpool)

mnl_pref_conthg <- logitr(
  data    = control_halfgal,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_conthg)

# Extract the log-likelihood score for the contol half-gallon model
log_lik_conthg <- logLik(mnl_pref_conthg)


mnl_pref_gthgal <- logitr(
  data    = treat_gen_halfgal,
  outcome = "choicedum",
  obsID   = "str",
  panelID  = "ID",
  pars    = c("asc", "price_value", "type_almond", "type_oat", "type_soy"),
  numCores = 1
)
summary(mnl_pref_gthgal)

# Extract the log-likelihood score for the  general treat half-gallon model
log_lik_gthgal <- logLik(mnl_pref_gthgal)


# Likelihood ratio test

# Define the samples using the log-likelihood scores
infohgpoolll <- as.numeric(log_lik_infohgpool) 
gconhgallonll <- as.numeric(log_lik_conthg)
ggthgallonll <- as.numeric(log_lik_gthgal)


# Calculate the likelihood ratio test statistic
LRT_stat_infohgallon <- -2 * (infohgpoolll - (gconhgallonll + ggthgallonll))

# Print the result
print(LRT_stat_infohgallon)


# Interpretation of likelihood ratio test results
# Calculate the degrees of freedom
df_infohg <- (length(coef(mnl_pref_conthg)) + length(coef(mnl_pref_gthgal))) - length(coef(mnl_pref_infohgpool))


# Define the critical value for the chi-squared test with df degrees of freedom at 0.05 significance level
critical_value_infohg <- qchisq(0.95, df = df_infohg)


# Interpretation of likelihood ratio test results
if (LRT_stat_infohgallon > critical_value_infohg) {
  cat("Reject the null hypothesis. The models cannot be pooled.\n")
} else {
  cat("Fail to reject the null hypothesis. The models can be pooled.\n")
}



###################################################################################
#REGRESSION OUTPUT FOR MNL MODELS
###################################################################################
#PREFERENCE SPACE
######################################################################################
#REGRESSION TABLE MNL - GALLON PREF SPACE
######################################################################################


# List the models used in the table
models_mnl_g <- list(mnl_pref_cg, mnl_pref_tgg)

# Define custom model names
custom_model_names_mnl_g <- c("Control", "Treatment")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_mnl_g <- c(
  "asc",
  "price_value",
  "type_almond",
  "type_oat",
  "type_soy"
)

# Create a custom coefficient map
custom_coef_map_mnl_g <- list(
  "price_value" = "Price",
  "type_almond" = "Almond milk",
  "type_oat" = "Oat milk",
  "type_soy" = "Soy milk",
  "asc" = "No purchase"
)



# Define the output path
output_path_mnl_g <- "Analysis/Output/Tables/mnl_gallon_table_no_protein.tex"

# Create the table with texreg
texreg(models_mnl_g,
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = custom_model_names_mnl_g,
       custom.coef.names = custom_variable_names_mnl_g,
       custom.coef.map = custom_coef_map_mnl_g,
       file = output_path_mnl_g,
       caption = "MNL model, gallon consumers",
       label = "tab:MNL_g",
       table = FALSE)

# Read the generated .tex file
table_tex_mnl_g <- readLines(output_path_mnl_g)

# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_mnl_g, 20), sep = "\n")

# Add LaTeX commands to include the caption at the top of the table
table_tex_mnl_g <- c("\\begin{table}[!htbp]",
                     "\\caption{MNL model, gallon consumers}",
                     "\\centering",
                     table_tex_mnl_g,
                     "\\label{tab:MNL_g}",
                     "\\end{table}")

# Save the updated table to the file
cat(table_tex_mnl_g, file = output_path_mnl_g, sep = "\n")




######################################################################################
#REGRESSION TABLE MNL - HALF-GALLON pref space
######################################################################################


# List the models used in the table
models_mnl_hg <- list(mnl_pref_chg, mnl_pref_tghg)

# Define custom model names
custom_model_names_mnl_hg <- c("Control", "Treatment")

# Define custom variable names (ensure it matches the number of coefficients)
custom_variable_names_mnl_hg <- c(
  "asc",
  "price_value",
  "type_almond",
  "type_oat",
  "type_almond"
)


# Create a custom coefficient map
custom_coef_map_mnl_hg <- list(
  "price_value" = "Price",
  "type_almond" = "Almond milk",
  "type_oat" = "Oat milk",
  "type_soy" = "Soy milk",
  "asc" = "No purchase"
)



# Define the output path
output_path_mnl_hg <- "Analysis/Output/Tables/mnl_halfgal_table_no_protein.tex"

# Create the table with texreg
texreg(models_mnl_hg,
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = custom_model_names_mnl_hg,
       custom.coef.names = custom_variable_names_mnl_hg,
       custom.coef.map = custom_coef_map_mnl_hg,
       file = output_path_mnl_hg,
       caption = "MNL model, half-gallon consumers",
       label = "tab:MNL_hg",
       table = FALSE)

# Read the generated .tex file
table_tex_mnl_hg <- readLines(output_path_mnl_hg)

# Print the first few lines of the LaTeX file for debugging
cat("Original LaTeX file content:\n")
cat(head(table_tex_mnl_hg, 20), sep = "\n")


# Add LaTeX commands to include the caption at the top of the table
table_tex_mnl_hg <- c("\\begin{table}[!htbp]",
                      "\\caption{MNL model, half-gallon consumers}",
                      "\\centering",
                      table_tex_mnl_hg,
                      "\\label{tab:MNL_hg}",
                      "\\end{table}")

# Save the updated table to the file
cat(table_tex_mnl_hg, file = output_path_mnl_hg, sep = "\n")


