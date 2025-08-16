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
output_dir <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching")
output_file <- file.path(output_dir, "Matching_Balance_Summary_Table_1to1.xlsx")

# --- Read treatment and control data ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df <- bind_rows(lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1"))))
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
  OPTION3 = list(  # same vars as OPTION2, but with exact sector match
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

# --- Loop through options/specs, run matchit with ratio = 1, and collect balance stats ---
results_list <- list()

for (opt in names(options_list)) {
  for (spec in names(options_list[[opt]])) {
    vars <- options_list[[opt]][[spec]]
    psm_df <- combined_df %>%
      select(Treatment, `Company name`, sector_2digit, all_of(vars)) %>%
      na.omit()

    formula_obj <- as.formula(paste("Treatment ~", paste0("`", vars, "`", collapse = " + ")))

    # Apply exact matching only where relevant
    if (opt == "OPTION1" || opt == "OPTION3") {
      match_model <- matchit(formula_obj, data = psm_df, method = "nearest", ratio = 1, exact = ~ sector_2digit)
    } else {
      match_model <- matchit(formula_obj, data = psm_df, method = "nearest", ratio = 1)
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

# --- Combine all results and export ---
final_balance_df <- bind_rows(results_list)
write_xlsx(final_balance_df, output_file)

cat("✅ Summary table saved to:\n", output_file, "\n")
