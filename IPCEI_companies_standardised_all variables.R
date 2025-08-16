library(dplyr)
library(readr)

# --- Paths ---
root_path_1 <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/CSV_files/Merged data"
ipcei_file <- file.path(root_path_1, "IPCEI_Projects_Battery_Hydrogen_Microelectronics_comprehensive.csv")
root_path_2 <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
out_file   <- file.path(root_path_2, "IPCEI_Companies_standardised.csv")

# --- Read IPCEI list ---
ipcei <- read_delim(ipcei_file, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  select(Company_name_orbis, everything())

# --- List all *_merged_standardised.csv files ---
std_files <- list.files(
  path = root_path,
  pattern = "_merged_standardised\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# --- Read and combine all standardised files ---
std_data <- lapply(std_files, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE)
}) %>%
  bind_rows()

# --- Join on exact Company_name_orbis ---
# Make sure the column names match
if (!"Company name" %in% names(std_data)) {
  stop("Column 'Company name' not found in merged standardised files.")
}

ipcei_std <- ipcei %>%
  left_join(std_data, by = c("Company_name_orbis" = "Company name"))

# --- Remove duplicates (keep first occurrence) ---
ipcei_std <- ipcei_std %>%
  distinct(Company_name_orbis, .keep_all = TRUE)

# --- Save result ---
write_delim(ipcei_std, out_file, delim = ";")
cat("✔ Created:", out_file, "\n")
