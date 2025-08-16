###########################################################################
########## Combined csv for Option 2 specification 1####################
# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(writexl)

# --- Define paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
matched_file <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching", "Matched_IPCEI_Control_Structural_Financial_withoutSector_Spec1.csv")
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Load data ---
matched_df <- read_delim(matched_file, delim = ";", locale = locale(encoding = "latin1"))
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1")) %>% 
  rename_with(str_trim)

# --- Get matched treated company names (exactly 120 unique names) ---
treated_names <- matched_df %>% filter(Treatment == 1) %>% pull(`Company name`) %>% unique()

# --- Filter IPCEI file to treated companies that appear in matched file, only once each ---
treated_final <- ipcei_df %>%
  filter(`Company name` %in% treated_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Get matched control company names (exactly 120 unique names) ---
control_names <- matched_df %>% filter(Treatment == 0) %>% pull(`Company name`) %>% unique()

# --- Load ORBIS country files for control company metadata ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
orbis_df <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1"))) %>%
  bind_rows() %>%
  rename_with(str_trim)

# --- Filter only matched control companies (one row per company), join subclass ---
control_final <- orbis_df %>%
  filter(`Company name` %in% control_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% filter(Treatment == 0) %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Combine and export ---
final_df <- bind_rows(treated_final, control_final)

# --- Save as Excel ---
output_file <- file.path(root_path, "Matched_Treated_Control_firms_all_variables_option2_spec1.csv")
write.csv2(final_df, output_file)



###########################################################################
########## Combined csv for Option 2 specification 3####################

# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(writexl)

# --- Define paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
matched_file <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching", "Matched_IPCEI_Control_Structural_Financial_withoutSector_Spec3.csv")
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Load data ---
matched_df <- read_delim(matched_file, delim = ";", locale = locale(encoding = "latin1"))
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1")) %>% 
  rename_with(str_trim)

# --- Get matched treated company names (exactly 120 unique names) ---
treated_names <- matched_df %>% filter(Treatment == 1) %>% pull(`Company name`) %>% unique()

# --- Filter IPCEI file to treated companies that appear in matched file, only once each ---
treated_final <- ipcei_df %>%
  filter(`Company name` %in% treated_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Get matched control company names (exactly 120 unique names) ---
control_names <- matched_df %>% filter(Treatment == 0) %>% pull(`Company name`) %>% unique()

# --- Load ORBIS country files for control company metadata ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
orbis_df <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1"))) %>%
  bind_rows() %>%
  rename_with(str_trim)

# --- Filter only matched control companies (one row per company), join subclass ---
control_final <- orbis_df %>%
  filter(`Company name` %in% control_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% filter(Treatment == 0) %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Combine and export ---
final_df <- bind_rows(treated_final, control_final)

# --- Save as Excel ---
output_file <- file.path(root_path, "Matched_Treated_Control_firms_all_variables_option2_spec3.csv")
write.csv2(final_df, output_file)

###########################################################################
########## Combined csv for Option 5 specification 3 ####################

# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(writexl)

# --- Define paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
matched_file <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching", "Matched_IPCEI_Control_Structural_Financial_R&D_Spec3.csv")
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Load data ---
matched_df <- read_delim(matched_file, delim = ";", locale = locale(encoding = "latin1"))
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1")) %>% 
  rename_with(str_trim)

# --- Get matched treated company names (exactly 120 unique names) ---
treated_names <- matched_df %>% filter(Treatment == 1) %>% pull(`Company name`) %>% unique()

# --- Filter IPCEI file to treated companies that appear in matched file, only once each ---
treated_final <- ipcei_df %>%
  filter(`Company name` %in% treated_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Get matched control company names (exactly 120 unique names) ---
control_names <- matched_df %>% filter(Treatment == 0) %>% pull(`Company name`) %>% unique()

# --- Load ORBIS country files for control company metadata ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
orbis_df <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1"))) %>%
  bind_rows() %>%
  rename_with(str_trim)

# --- Filter only matched control companies (one row per company), join subclass ---
control_final <- orbis_df %>%
  filter(`Company name` %in% control_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% filter(Treatment == 0) %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Combine and export ---
final_df <- bind_rows(treated_final, control_final)

# --- Save as Excel ---
output_file <- file.path(root_path, "Matched_Treated_Control_firms_all_variables_option5_spec3.csv")
write.csv2(final_df, output_file)

###########################################################################
########## Combined csv for Option 3 specification 3####################

# --- Load libraries ---
library(dplyr)
library(readr)
library(stringr)
library(writexl)

# --- Define paths ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
matched_file <- file.path(root_path, "z_Matching_result_files", "Nearest_Neighbour_matching", "Matched_IPCEI_Control_Structural_Financial_Spec3_ExactSector.csv")
ipcei_file <- file.path(root_path, "IPCEI_Companies_standardised.csv")

# --- Load data ---
matched_df <- read_delim(matched_file, delim = ";", locale = locale(encoding = "latin1"))
ipcei_df <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1")) %>% 
  rename_with(str_trim)

# --- Get matched treated company names (exactly 120 unique names) ---
treated_names <- matched_df %>% filter(Treatment == 1) %>% pull(`Company name`) %>% unique()

# --- Filter IPCEI file to treated companies that appear in matched file, only once each ---
treated_final <- ipcei_df %>%
  filter(`Company name` %in% treated_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Get matched control company names (exactly 120 unique names) ---
control_names <- matched_df %>% filter(Treatment == 0) %>% pull(`Company name`) %>% unique()

# --- Load ORBIS country files for control company metadata ---
control_files <- list.files(path = root_path, pattern = "_merged_standardised\\.csv$", recursive = TRUE, full.names = TRUE)
orbis_df <- lapply(control_files, function(f) read_delim(f, delim = ";", locale = locale(encoding = "latin1"))) %>%
  bind_rows() %>%
  rename_with(str_trim)

# --- Filter only matched control companies (one row per company), join subclass ---
control_final <- orbis_df %>%
  filter(`Company name` %in% control_names) %>%
  distinct(`Company name`, .keep_all = TRUE) %>%
  left_join(matched_df %>% filter(Treatment == 0) %>% select(`Company name`, Treatment, subclass), by = "Company name") %>%
  relocate(`Company name`, Treatment, subclass)

# --- Combine and export ---
final_df <- bind_rows(treated_final, control_final)

# --- Save as Excel ---
output_file <- file.path(root_path, "Matched_Treated_Control_firms_all_variables_option3_spec3.csv")
write.csv2(final_df, output_file)


