# Load required packages
library(dplyr)
library(readr)

# Set root path
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"

# Get list of all *_merged_calculated.csv files
calculated_files <- list.files(
  path = root_path,
  pattern = "_merged_calculated\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Define variables to standardise (excluding Age)
variables_to_standardise <- c(
  "Number of employees 2023", "Number of publications",
  paste0("Research & development expenses th EUR ", 2018:2023),
  paste0("Total assets th EUR ", 2018:2023),
  paste0("Total liabilities th EUR ", 2018:2023),
  paste0("Operating revenue (Turnover) th EUR ", 2018:2023),
  paste0("ROA using Profit (Loss) before tax ", 2018:2023),
  paste0("Leverage ", 2018:2023)
)

# Loop through all files
for (file in calculated_files) {
  df <- read_delim(file, delim = ";", locale = locale(encoding = "UTF-8"),
                   col_types = cols(.default = "c"), trim_ws = TRUE)

  # Convert variables to numeric
  for (col in variables_to_standardise) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(gsub(",", "", df[[col]])))
    }
  }

  # Take absolute values for R&D expense columns BEFORE standardising
  rd_cols <- grep("^Research & development expenses th EUR", names(df), value = TRUE)
  for (col in rd_cols) {
    df[[col]] <- abs(df[[col]])
  }

  # Standardise and add new columns with prefix "std_"
  for (col in variables_to_standardise) {
    if (col %in% names(df)) {
      values <- df[[col]]
      std_col <- scale(values)
      df[[paste0("std_", col)]] <- as.numeric(std_col)
    }
  }

  # Save updated file
  output_file <- sub("_merged_calculated.csv$", "_merged_standardised.csv", file)
  write_delim(df, output_file, delim = ";")
  cat("✔ Saved:", output_file, "\n")
}

# OPTIONAL: Check one example file for mean and SD of standardised columns
df <- read_delim(
  "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/Austria/Austria_merged_standardised.csv",
  delim = ";",
  locale = locale(encoding = "latin1"),
  col_types = cols(.default = "d")
)

std_cols <- grep("^std_", names(df), value = TRUE)
summary_stats <- data.frame(
  Column = std_cols,
  Mean = round(sapply(df[std_cols], mean, na.rm = TRUE), 4),
  Std_Dev = round(sapply(df[std_cols], sd, na.rm = TRUE), 4)
)

print(summary_stats)
