# ################################################################# #
### Respondent Consumption and shopping habits and preferences   ####
# ################################################################# #
# Agnes af Sandeberg 12 March 2026
# R version 4.4.0 
#AIM, we could present: Divided by container size (gallon, half-gallon) frequency of consumers milk drinking
#We can test differences across half-gallon and gallon

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

gallon_sample <- full_sample %>%
  filter(gallon==1)

halfgal_sample <- full_sample %>%
  filter(gallon==0)

n_obs <- n_distinct(full_sample$ID)

n_gallon <- n_distinct(gallon_sample$ID)

n_halfgal <- n_distinct(halfgal_sample$ID)


###################################################################################
#Building panel a, frequency of consumption of each milk type including "other"
###################################################################################

# 4. Define variables and labels
milk_vars <- c(
  "dairy_buyer",
  "almond_buyer",
  "oat_buyer",
  "soy_buyer",
  "other_buyer"
)

freq_labels <- c(
  "Once or more a week",
  "A couple of times per month",
  "Less than once a month",
  "Never"
)


freq_percent <- function(data, var) {
  tab <- table(factor(data[[var]], levels = 1:6))
  grouped <- c(
    sum(tab[1:3]),
    tab[4],
    tab[5],
    tab[6]
  )
  
  round(100 * grouped / sum(grouped), 0)
}

# ----------------------------------------------------------------------------------
# Pairwise proportion tests for each frequency and milk type
# ----------------------------------------------------------------------------------
group_freq <- function(x) {
  case_when(
    x %in% 1:3 ~ "weekly",
    x == 4 ~ "monthly",
    x == 5 ~ "rare",
    x == 6 ~ "never",
    TRUE ~ NA_character_
  )
}

full_sample <- full_sample %>%
  mutate(across(all_of(milk_vars), group_freq))

resp_sample <- full_sample %>%
  select(ID, gallon, all_of(milk_vars)) %>%
  distinct()


run_freq_tests <- function(var, freq_level) {
  
  df <- resp_sample %>%
    filter(!is.na(.data[[var]]))
  
  x1 <- sum(df[[var]] == freq_level & df$gallon == 1)
  n1 <- sum(df$gallon == 1)
  
  x2 <- sum(df[[var]] == freq_level & df$gallon == 0)
  n2 <- sum(df$gallon == 0)
  
  test <- prop.test(c(x1, x2), c(n1, n2))
  
  tibble(
    milk_type = var,
    frequency = freq_level,
    x_gallon = x1,
    n_gallon = n1,
    x_halfgal = x2,
    n_halfgal = n2,
    p_value = test$p.value
  )
}

freq_levels <- c("weekly","monthly","rare","never")

all_tests <- expand.grid(
  var = milk_vars,
  freq = freq_levels,
  stringsAsFactors = FALSE
) %>%
  pmap_dfr(~ run_freq_tests(..1, ..2))

all_tests <- all_tests %>%
  mutate(p_adj = p.adjust(p_value, method = "holm"),
         signif = ifelse(p_value < 0.05, "*", "")
         )


# ----------------------------------------------------------------------------------
# Chi-square test of the full distribution within a milk type- null hypothesis is 
# that the distribution is the same for the gallon and half-gallon sample
# ----------------------------------------------------------------------------------


# Function to run chi-square test per milk type
run_chisq_test <- function(var) {
  
  tab <- table(
    resp_sample$gallon,   # 0 = half-gallon, 1 = gallon
    resp_sample[[var]]    # frequency column
  )
  
  test <- chisq.test(tab)
  
  tibble(
    milk_type = var,
    chi_sq = as.numeric(test$statistic),
    df = as.numeric(test$parameter),
    p_value = test$p.value,
    expected = list(test$expected)  # for diagnostics if needed
  )
}

#Run a Chi2 test for all milk types
freq_vars <- milk_vars

chisq_results <- lapply(freq_vars, run_chisq_test) %>%
  bind_rows() %>%
  mutate(
    milk_type_clean = gsub("_buyer", "", milk_type),
    signif = ifelse(p_value < 0.05, "*", ""),
    milk_label = paste0(milk_type_clean, signif)
  )



# For dairy milk
tab <- table(resp_sample$gallon, resp_sample$dairy_buyer)
chisq.test(tab)$expected
# For dairy milk
tab_almond <- table(resp_sample$gallon, resp_sample$almond_buyer)
chisq.test(tab_almond)$expected
# For dairy milk
tab_oat <- table(resp_sample$gallon, resp_sample$oat_buyer)
chisq.test(tab_oat)$expected
# For dairy milk
tab_soy <- table(resp_sample$gallon, resp_sample$soy_buyer)
chisq.test(tab_soy)$expected
# For dairy milk
tab_other <- table(resp_sample$gallon, resp_sample$other_buyer)
chisq.test(tab_other)$expected




######### Panel #########

milk_table_gallon <- data.frame(
  Milk_Product = c("Dairy milk", "Almond milk", "Oat milk", "Soy milk", "Other milk(s)"),
  t(sapply(milk_vars, freq_percent, data = gallon_sample))
)

milk_table_halfgal <- data.frame(
  Milk_Product = c("Dairy milk", "Almond milk", "Oat milk", "Soy milk", "Other milk(s)"),
  t(sapply(milk_vars, freq_percent, data = halfgal_sample))
)

panel_a <- data.frame(
  Milk_Product = milk_table_gallon$Milk_Product,
  milk_table_gallon[, -1],
  milk_table_halfgal[, -1]
)

sig_lookup <- chisq_results %>%
  transmute(
    Milk_Type = case_when(
      milk_type_clean == "dairy"  ~ "Dairy milk",
      milk_type_clean == "almond" ~ "Almond milk",
      milk_type_clean == "oat"    ~ "Oat milk",
      milk_type_clean == "soy"    ~ "Soy milk",
      milk_type_clean == "other"  ~ "Other milk(s)"
    ),
    Milk_Type_sig = paste0(Milk_Type, signif)
  )


#Add results from the chi2 test
# Build named vector for lookup
sig_map <- setNames(sig_lookup$Milk_Type_sig, sig_lookup$Milk_Type)

# Replace Milk_Product values with starred versions if significant
panel_a$Milk_Product <- ifelse(
  panel_a$Milk_Product %in% names(sig_map),
  sig_map[panel_a$Milk_Product],
  panel_a$Milk_Product
)

colnames(panel_a) <- c(
  "Milk Type",
  rep(c("Once or more a week", "A couple of times per month", "Less than once a month", "Never"), 2)
)

panel_a_no_colnames <- panel_a
colnames(panel_a_no_colnames) <- rep("", ncol(panel_a_no_colnames))


panel_a_tex <- tempfile(fileext = ".tex")

kable(
  panel_a_no_colnames,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  row.names = FALSE
) %>%
  save_kable(panel_a_tex)

panel_a_tex <- readLines(panel_a_tex)

# Clean up LaTeX artifacts 
panel_a_tex <- panel_a_tex[!grepl("^\\\\begin\\{table\\}|^\\\\end\\{table\\}", panel_a_tex)]
panel_a_tex <- panel_a_tex[!grepl("^\\\\begin\\{tabular\\}|^\\\\end\\{tabular\\}", panel_a_tex)]
panel_a_tex <- panel_a_tex[!grepl("^\\s*$", panel_a_tex)]

#Remove toprule
panel_a_tex <- panel_a_tex[!grepl("\\\\toprule", panel_a_tex)]

# Remove kable-generated empty header + surrounding midrules
panel_a_tex <- panel_a_tex[
  !grepl("^\\\\midrule$", panel_a_tex)
]

panel_a_tex <- panel_a_tex[
  !grepl("^\\s*&\\s*&\\s*&\\s*&\\s*&\\s*&\\s*\\\\\\\\$", panel_a_tex)
]

#Remove empty rows
panel_a_tex <- panel_a_tex[
  !grepl("^\\s*(&\\s*)+\\\\\\\\$", panel_a_tex)
]


panel_a_header <- c(
  "\\addlinespace[0.5ex]",
  "\\multicolumn{1}{c}{} &",
  "\\multicolumn{4}{c}{\\textit{Gallon consumers n = ", n_gallon,"}} &",
  "\\multicolumn{4}{c}{\\textit{Half-gallon consumers  n = ", n_halfgal,"}} \\\\",
  "\\cmidrule(lr){2-5}",
  "\\cmidrule(lr){6-9}",
  "\\addlinespace[0.25ex]",
  "\\multicolumn{1}{c}{\\makecell[l]{Milk Type}} &",
  "\\multicolumn{1}{c}{\\makecell[c]{Once or\\\\more a week}} &",
  "\\multicolumn{1}{c}{\\makecell[c]{A couple\\\\of times\\\\per month}} &",
  "\\multicolumn{1}{c}{\\makecell[c]{Less than\\\\once a month}} &",
  "\\multicolumn{1}{c}{Never} &",
  "\\multicolumn{1}{c}{\\makecell[c]{Once or\\\\more a week}} &",
  "\\multicolumn{1}{c}{\\makecell[c]{A couple\\\\ of times\\\\per month}} &",
  "\\multicolumn{1}{c}{\\makecell[c]{Less than\\\\once a month}} &",
  "\\multicolumn{1}{c}{Never} \\\\",
  "\\midrule",
  "\\addlinespace[0.5ex]"
)



panel_a_tex_fixed <- c(
  panel_a_header,
  panel_a_tex
)




###################################################################################
#Put table together
###################################################################################



final_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\setlength{\\tabcolsep}{2pt}",
  "\\renewcommand{\\arraystretch}{1.1}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  paste0(
    "\\caption{Respondent milk purchasing behavior}"
  ),
  "\\label{tab:milk_type_purch_frequency}",
  "\\begin{tabularx}{\\textwidth}{>{\\raggedright\\arraybackslash}p{2.5cm} *{9}{>{\\centering\\arraybackslash}X}}",
  "\\toprule",
  "\\addlinespace[0.5ex]",
  "\\multicolumn{9}{l}{\\textit{Frequency household purchase of specific milk types (\\%), n = ", n_obs,"}} \\\\",
  "\\midrule",
  "\\addlinespace[0.5ex]",
  panel_a_tex_fixed,
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item  $^{*}$ Rejects the null hypothesis of equal purchasing-frequency distributions between gallon and half-gallon consumers within each milk type at the 5\\% significance level.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)


writeLines(
  final_table,
  file.path(output, "only_milk_purchase_frequency_gal_halfgal.tex")
)



