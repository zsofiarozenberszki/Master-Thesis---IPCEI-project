# --- Load necessary libraries ---
library(dplyr)
library(readr)
library(stringr)
library(MatchIt)
library(cobalt)
library(tibble)
library(writexl)

# --- Define file paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")
output_dir <- file.path(root_path, "z_Matching_result_files", "Optimal_pair_matching")

# --- Read treatment and control data ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1")))
control_df <- bind_rows(control_df_list)
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]
ipcei_df$Treatment <- 1
control_df$Treatment <- 0
combined_df <- bind_rows(ipcei_df, control_df) %>%
  rename_with(str_trim) %>%
  mutate(sector_2digit = str_sub(`NACE Rev. 2, core code (4 digits)`, 1, 2))

# --- Define all options and specs ---
options_list <- list(
  OPTION1 = list(Spec1 = c("Age", "std_Number of employees 2023")),
  OPTION2 = list(
    Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018"),
    Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018"),
    Spec3 = c("Age", "std_Number of employees 2023", "std_ROA using Profit (Loss) before tax 2018", 
              "std_Operating revenue (Turnover) th EUR 2018"),
    Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018")
  ),
  OPTION3 = list(
    Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018"),
    Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018"),
    Spec3 = c("Age", "std_Number of employees 2023", "std_ROA using Profit (Loss) before tax 2018", 
              "std_Operating revenue (Turnover) th EUR 2018"),
    Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018")
  ),
  OPTION4 = list(Spec1 = c("Age", "std_Number of employees 2023", "std_Research & development expenses th EUR 2018")),
  OPTION5 = list(
    Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018", 
              "std_Research & development expenses th EUR 2018"),
    Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018", 
              "std_Operating revenue (Turnover) th EUR 2018", "std_Research & development expenses th EUR 2018"),
    Spec3 = c("Age", "std_Number of employees 2023", "std_ROA using Profit (Loss) before tax 2018", 
              "std_Operating revenue (Turnover) th EUR 2018", "std_Research & development expenses th EUR 2018"),
    Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018", 
              "std_Research & development expenses th EUR 2018")
  )
)

# --- Loop through all options/specs and collect balance metrics ---
results_list <- list()

for (opt in names(options_list)) {
  for (spec in names(options_list[[opt]])) {
    vars <- options_list[[opt]][[spec]]
    
    psm_df <- combined_df %>%
      select(Treatment, `Company name`, all_of(vars), sector_2digit) %>%
      na.omit()
    
    formula_obj <- as.formula(paste("Treatment ~", paste0("`", vars, "`", collapse = " + ")))
    
    # Optimal matching with or without exact sector match
    if (opt == "OPTION1") {
      match_model <- matchit(formula_obj, data = psm_df, method = "optimal", exact = ~ sector_2digit)
    } else if (opt == "OPTION3") {
      match_model <- matchit(formula_obj, data = psm_df, method = "optimal", exact = ~ sector_2digit)
    } else {
      match_model <- matchit(formula_obj, data = psm_df, method = "optimal")
    }
    
    # Get balance stats
    bal <- summary(match_model, standardize = TRUE)$sum.matched
    
    bal_tbl <- as_tibble(bal, rownames = "Variable") %>%
      select(Variable, `Std. Mean Diff.`, `Var. Ratio`) %>%
      mutate(Option = opt, Specification = spec) %>%
      relocate(Option, Specification)
    
    results_list[[paste(opt, spec, sep = "_")]] <- bal_tbl
  }
}

# --- Combine and export as Excel ---
final_balance_df <- bind_rows(results_list)
output_excel <- file.path(output_dir, "Matching_Balance_Summary_Table_OPTIMAL.xlsx")
writexl::write_xlsx(final_balance_df, output_excel)

cat("✅ Optimal Matching summary table saved to:\n", output_excel, "\n")
