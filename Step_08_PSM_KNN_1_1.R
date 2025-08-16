

###################################################################
############ PSM Nearest Neighbour + Exact Sector Match ###########
############     Structural variables only               ###########
###################################################################

# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)

# --- Step 1: Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Step 2: Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Step 3: Read all control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"))
})
control_df <- bind_rows(control_df_list)

# --- Step 4: Remove treated firms from control set based on Company name ---
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Step 5: Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0

# --- Step 6: Combine datasets ---
combined_df <- bind_rows(ipcei_df, control_df)

# --- Step 7: Create sector variable with first two digits of NACE Rev. 2 core code ---
combined_df <- combined_df %>%
  rename_with(str_trim) %>%  # clean column names
  mutate(sector_2digit = str_sub(`NACE Rev. 2, core code (4 digits)`, 1, 2))

# --- Step 8: Select variables for matching ---
matching_vars <- c("Age", "std_Number of employees 2023", "sector_2digit")

# Remove rows with missing values in matching variables
psm_df <- combined_df %>%
  select(Treatment, `Company name`, all_of(matching_vars)) %>%
  na.omit()

# --- Step 9: Run Propensity Score Matching with exact sector matching ---
set.seed(123)
match_model <- matchit(
  Treatment ~ Age + `std_Number of employees 2023`,
  data = psm_df,
  method = "nearest",
  ratio = 1,
  exact = ~ sector_2digit
)

# --- Step 10: Extract matched data ---
matched_data <- match.data(match_model)

# --- Step 11: Save matched dataset ---
output_path <- file.path(root_path, "Matched_IPCEI_Control_Structural_ExactSector.csv")
write_delim(matched_data, output_path, delim = ";")
cat("✔ Matched dataset saved to:\n", output_path, "\n")

# --- Step 12: Show balance diagnostics ---
cat("\n Balance diagnostics:\n")
print(summary(match_model, standardize = TRUE))
love.plot(match_model, stats = "mean.diffs", threshold = 0.1)




###################################################################################
############Propensity Score & Nearest Neighbour Matching, 1:1 ##########################
############Structural and financial variables without sector matching #############
####################################################################################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)

# --- Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")
output_dir <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching")

# --- Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Read all control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"))
})
control_df <- bind_rows(control_df_list)

# --- Remove treated firms from control set ---
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0

# --- Combine datasets ---
combined_df <- bind_rows(ipcei_df, control_df)

# --- Define specifications ---
specifications <- list(
  Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
            "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018"),
  Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
            "std_Operating revenue (Turnover) th EUR 2018"),
  Spec3 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018", 
            "std_ROA using Profit (Loss) before tax 2018"),
  Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018")
)

# --- Run matching for each specification ---
for (spec_name in names(specifications)) {
  cat("\n=======================\nRunning", spec_name, "\n=======================\n")
  
  vars <- specifications[[spec_name]]
  
  # Prepare matching dataset
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, all_of(vars)) %>%
    na.omit()
  
  # Quote variables with spaces
  rhs_vars <- paste0("`", vars[-1], "`", collapse = " + ")
  formula_text <- paste("Treatment ~", rhs_vars)
  formula_obj <- as.formula(formula_text)
  
  # Run PSM
  set.seed(123)
  match_model <- matchit(
    formula_obj,
    data = psm_df,
    method = "nearest",
    ratio = 1
  )
  
  # Extract matched data
  matched_data <- match.data(match_model)
  
  # Save matched dataset
  output_file <- paste0("Matched_IPCEI_Control_Structural_Financial_withoutSector_", spec_name, ".csv")
  write_delim(matched_data, file.path(output_dir, output_file), delim = ";")
  cat("✔ Matched dataset saved to:", output_file, "\n")
  
  # Print diagnostics
  cat("\n📊 Balance diagnostics:\n")
  print(summary(match_model, standardize = TRUE))
  love.plot(match_model, stats = "mean.diffs", threshold = 0.1, 
            abs = TRUE, var.order = "unadjusted", 
            title = paste("Love Plot -", spec_name))
}

###################################################################
############ Propensity Score & Nearest Neighbour Matching ########
############ Structural and financial variables ####################
############# with Exact Sector (2-digit) Match ######################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)

# --- Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")
output_dir <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching")

# --- Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Read all control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"))
})
control_df <- bind_rows(control_df_list)

# --- Remove treated firms from control set ---
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0

# --- Combine datasets ---
combined_df <- bind_rows(ipcei_df, control_df)

# --- Clean column names and create 2-digit sector code ---
combined_df <- combined_df %>%
  rename_with(str_trim) %>%
  mutate(sector_2digit = str_sub(`NACE Rev. 2, core code (4 digits)`, 1, 2))

# --- Define matching specifications ---
specifications <- list(
  Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
            "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018"),
  Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
            "std_Operating revenue (Turnover) th EUR 2018"),
  Spec3 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018", 
            "std_ROA using Profit (Loss) before tax 2018"),
  Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018")
)

# --- Run matching for each specification ---
for (spec_name in names(specifications)) {
  cat("\n=======================\nRunning", spec_name, "with exact sector matching\n=======================\n")
  
  vars <- specifications[[spec_name]]
  
  # Prepare matching data with sector included for filtering
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, sector_2digit, all_of(vars)) %>%
    na.omit()
  
  # Quote RHS variable names
  rhs_vars <- paste0("`", vars[-1], "`", collapse = " + ")
  formula_text <- paste("Treatment ~", rhs_vars)
  formula_obj <- as.formula(formula_text)
  
  # Run PSM with exact sector matching
  set.seed(123)
  match_model <- matchit(
    formula_obj,
    data = psm_df,
    method = "nearest",
    ratio = 1,
    exact = ~ sector_2digit
  )
  
  # Extract matched data
  matched_data <- match.data(match_model)
  
  # Save matched dataset
  output_file <- paste0("Matched_IPCEI_Control_Control_Structural_Financial_", spec_name, "_ExactSector.csv")
  write_delim(matched_data, file.path(output_dir, output_file), delim = ";")
  cat("✔ Matched dataset saved to:", output_file, "\n")
  
  # Print diagnostics
  cat("\n📊 Balance diagnostics:\n")
  print(summary(match_model, standardize = TRUE))
  love.plot(match_model, stats = "mean.diffs", threshold = 0.1, 
            abs = TRUE, var.order = "unadjusted", 
            title = paste("Love Plot -", spec_name, "(Exact Sector)"))
}



#################################################################
############ Propensity Score & Nearest Neighbour Matching ######
########### without sector matching, structural and R&D##########
###################################################################

# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)

# --- Step 1: Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
output_path <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching", "Matched_IPCEI_Control_Structural_RD.csv")
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Step 2: Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Step 3: Read all control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"))
})
control_df <- bind_rows(control_df_list)

# --- Step 4: Remove treated firms from control set based on Company name ---
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Step 5: Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0

# --- Step 6: Combine datasets ---
combined_df <- bind_rows(ipcei_df, control_df)

# --- Step 7: Create matching dataset ---
matching_vars <- c("Age", "std_Number of employees 2023", "std_Research & development expenses th EUR 2018")
psm_df <- combined_df %>%
  rename_with(str_trim) %>%
  select(Treatment, `Company name`, all_of(matching_vars)) %>%
  na.omit()

# --- Step 8: Run Propensity Score Matching (Nearest Neighbour) ---
set.seed(123)
match_model <- matchit(
  Treatment ~ Age + `std_Number of employees 2023` + `std_Research & development expenses th EUR 2018`,
  data = psm_df,
  method = "nearest",
  ratio = 1
)

# --- Step 9: Extract matched data ---
matched_data <- match.data(match_model)

# --- Step 10: Save matched dataset ---
write_delim(matched_data, output_path, delim = ";")
cat("✔ Matched dataset saved to:\n", output_path, "\n")

# --- Step 11: Show balance diagnostics ---
cat("\nBalance diagnostics:\n")
print(summary(match_model, standardize = TRUE))
love.plot(match_model, stats = "mean.diffs", threshold = 0.1)

#####################################################################################
############ Propensity Score & Nearest Neighbour Matching, 1:1 #####################
############ Structural, Financial with expanded research variables #################
############ without sector matching ################################################
#####################################################################################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)
library(patchwork)

# --- Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")
output_dir <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching")

# --- Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Read all control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"))
})
control_df <- bind_rows(control_df_list)

# --- Remove treated firms from control set ---
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0

# --- Combine datasets ---
combined_df <- bind_rows(ipcei_df, control_df)

# --- Define specifications with R&D variable included ---
specifications <- list(
  Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018",
            "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018",
            "std_Research & development expenses th EUR 2018"),
  Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018",
            "std_Operating revenue (Turnover) th EUR 2018", "std_Research & development expenses th EUR 2018"),
  Spec3 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018",
            "std_ROA using Profit (Loss) before tax 2018", "std_Research & development expenses th EUR 2018"),
  Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018",
            "std_Research & development expenses th EUR 2018")
)

# --- Initialize list to store Love plots ---
love_plots <- list()

# --- Run matching for each specification ---
for (spec_name in names(specifications)) {
  cat("\n=======================\nRunning", spec_name, "\n=======================\n")

  vars <- specifications[[spec_name]]

  # Prepare matching dataset
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, all_of(vars)) %>%
    na.omit()

  # Quote variables with backticks in formula
  rhs_vars <- paste0("`", vars, "`", collapse = " + ")
  formula_text <- paste("Treatment ~", rhs_vars)
  formula_obj <- as.formula(formula_text)

  # Run PSM
  set.seed(123)
  match_model <- matchit(
    formula_obj,
    data = psm_df,
    method = "nearest",
    ratio = 1
  )

  # Extract matched data
  matched_data <- match.data(match_model)

  # Save matched dataset
  output_file <- paste0("Matched_IPCEI_Control_Structural_Financial_R&D_", spec_name, ".csv")
  write_delim(matched_data, file.path(output_dir, output_file), delim = ";")
  cat("✔ Matched dataset saved to:", output_file, "\n")

  # Print diagnostics
  cat("\n📊 Balance diagnostics:\n")
  print(summary(match_model, standardize = TRUE))

  # Generate and store Love plot
  plot <- love.plot(match_model,
                    stats = "mean.diffs",
                    threshold = 0.1,
                    abs = TRUE,
                    var.order = "unadjusted",
                    stars = "none",
                    binary = "raw",
                    title = paste("Love Plot -", spec_name))
  love_plots[[spec_name]] <- plot
}

# --- Combine and display all Love plots in 2x2 grid ---
combined_plot <- (love_plots[[1]] | love_plots[[2]]) / (love_plots[[3]] | love_plots[[4]])
print(combined_plot)
