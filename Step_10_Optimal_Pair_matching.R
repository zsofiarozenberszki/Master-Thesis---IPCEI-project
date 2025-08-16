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
output_dir <- file.path(root_path, "z_Matching_result_files", "Optimal_pair_matching")

# --- Read IPCEI treatment group ---
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"))

# --- Read control group files ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
control_df_list <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1")))
control_df <- bind_rows(control_df_list)
control_df <- control_df[!control_df$`Company name` %in% ipcei_df$`Company name`, ]

# --- Add treatment indicator ---
ipcei_df$Treatment <- 1
control_df$Treatment <- 0
combined_df <- bind_rows(ipcei_df, control_df)

# --- Clean column names and extract sector codes ---
combined_df <- combined_df %>%
  rename_with(str_trim) %>%
  mutate(sector_2digit = str_sub(`NACE Rev. 2, core code (4 digits)`, 1, 2))

# ========================
# OPTION 1: Structural + Exact Sector Matching
# ========================
cat("\n================ OPTION 1 ================\n")
psm_df <- combined_df %>%
  select(Treatment, `Company name`, sector_2digit, Age, `std_Number of employees 2023`) %>%
  na.omit()

set.seed(123)
match_model <- matchit(
  Treatment ~ Age + `std_Number of employees 2023`,
  data = psm_df,
  method = "optimal",
  exact = ~ sector_2digit
)
matched_data <- match.data(match_model)
write_delim(matched_data, file.path(output_dir, "Matched_OPTION1_Structural_ExactSector.csv"), delim = ";")
cat("\n📊 Balance diagnostics OPTION 1:\n")
print(summary(match_model, standardize = TRUE))
love.plot(match_model, stats = "mean.diffs", threshold = 0.1, abs = TRUE, title = "OPTION 1: Structural + Sector")

# ========================
# OPTION 2: Structural + Financial (No Sector)
# ========================
cat("\n================ OPTION 2 ================\n")
specifications_2 <- list(
  Spec1 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018",
            "std_Operating revenue (Turnover) th EUR 2018", "std_ROA using Profit (Loss) before tax 2018"),
  Spec2 = c("Age", "std_Number of employees 2023", "std_Total assets th EUR 2018",
            "std_Operating revenue (Turnover) th EUR 2018"),
  Spec3 = c("Age", "std_Number of employees 2023", "std_ROA using Profit (Loss) before tax 2018",
            "std_Operating revenue (Turnover) th EUR 2018"),
  Spec4 = c("Age", "std_Number of employees 2023", "std_Operating revenue (Turnover) th EUR 2018")
)

plots_2 <- list()
for (spec in names(specifications_2)) {
  vars <- specifications_2[[spec]]
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, all_of(vars)) %>%
    na.omit()
  formula_obj <- as.formula(paste("Treatment ~", paste0("`", vars, "`", collapse = " + ")))
  match_model <- matchit(formula_obj, data = psm_df, method = "optimal")
  matched_data <- match.data(match_model)
  write_delim(matched_data, file.path(output_dir, paste0("Matched_OPTION2_", spec, ".csv")), delim = ";")
  cat(paste0("\n📊 Balance diagnostics OPTION 2 - ", spec, ":\n"))
  print(summary(match_model, standardize = TRUE))
  plots_2[[spec]] <- love.plot(match_model, stats = "mean.diffs", threshold = 0.1, abs = TRUE, title = paste("OPTION 2:", spec))
}
((plots_2[[1]] | plots_2[[2]]) / (plots_2[[3]] | plots_2[[4]]))

# ========================
# OPTION 3: Structural + Financial + Sector
# ========================
cat("\n================ OPTION 3 ================\n")
specifications_3 <- specifications_2  # reuse
plots_3 <- list()
for (spec in names(specifications_3)) {
  vars <- specifications_3[[spec]]
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, sector_2digit, all_of(vars)) %>%
    na.omit()
  formula_obj <- as.formula(paste("Treatment ~", paste0("`", vars, "`", collapse = " + ")))
  match_model <- matchit(formula_obj, data = psm_df, method = "optimal", exact = ~ sector_2digit)
  matched_data <- match.data(match_model)
  write_delim(matched_data, file.path(output_dir, paste0("Matched_OPTION3_", spec, ".csv")), delim = ";")
  cat(paste0("\n📊 Balance diagnostics OPTION 3 - ", spec, ":\n"))
  print(summary(match_model, standardize = TRUE))
  plots_3[[spec]] <- love.plot(match_model, stats = "mean.diffs", threshold = 0.1, abs = TRUE, title = paste("OPTION 3:", spec))
}
((plots_3[[1]] | plots_3[[2]]) / (plots_3[[3]] | plots_3[[4]]))

# ========================
# OPTION 4: Structural + R&D (No Sector)
# ========================
cat("\n================ OPTION 4 ================\n")
matching_vars <- c("Age", "std_Number of employees 2023", "std_Research & development expenses th EUR 2018")
psm_df <- combined_df %>%
  select(Treatment, `Company name`, all_of(matching_vars)) %>%
  na.omit()
formula_obj <- as.formula(paste("Treatment ~", paste0("`", matching_vars, "`", collapse = " + ")))
match_model <- matchit(formula_obj, data = psm_df, method = "optimal")
matched_data <- match.data(match_model)
write_delim(matched_data, file.path(output_dir, "Matched_OPTION4_Structural_RD.csv"), delim = ";")
cat("\n📊 Balance diagnostics OPTION 4:\n")
print(summary(match_model, standardize = TRUE))
love.plot(match_model, stats = "mean.diffs", threshold = 0.1, abs = TRUE, title = "OPTION 4: Structural + R&D")

# ========================
# OPTION 5: Structural + Financial + R&D (No Sector)
# ========================
cat("\n================ OPTION 5 ================\n")
specifications_5 <- list(
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

plots_5 <- list()
for (spec in names(specifications_5)) {
  vars <- specifications_5[[spec]]
  psm_df <- combined_df %>%
    select(Treatment, `Company name`, all_of(vars)) %>%
    na.omit()
  formula_obj <- as.formula(paste("Treatment ~", paste0("`", vars, "`", collapse = " + ")))
  match_model <- matchit(formula_obj, data = psm_df, method = "optimal")
  matched_data <- match.data(match_model)
  write_delim(matched_data, file.path(output_dir, paste0("Matched_OPTION5_", spec, ".csv")), delim = ";")
  cat(paste0("\n📊 Balance diagnostics OPTION 5 - ", spec, ":\n"))
  print(summary(match_model, standardize = TRUE))
  plots_5[[spec]] <- love.plot(match_model, stats = "mean.diffs", threshold = 0.1, abs = TRUE, title = paste("OPTION 5:", spec))
}
((plots_5[[1]] | plots_5[[2]]) / (plots_5[[3]] | plots_5[[4]]))
