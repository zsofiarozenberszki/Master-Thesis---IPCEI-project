library(dplyr)
library(readr)
library(stringr)

# --- Step 1: Define root path ---
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"

# --- Step 2: Find all *_merged.csv files recursively ---
merged_files <- list.files(
  path = root_path,
  pattern = "_merged\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# --- Step 3: Define which columns to convert to numeric ---
numeric_cols <- c(
  "Number of employees 2023", "Number of publications",
  paste0("Research & development expenses th EUR ", 2018:2023),
  paste0("Total assets th EUR ", 2018:2023),
  paste0("Total liabilities th EUR ", 2018:2023),
  paste0("Operating revenue (Turnover) th EUR ", 2018:2023),
  paste0("ROA using Profit (Loss) before tax ", 2018:2023)
)

# --- Step 4: Function to extract year and calculate Age ---
extract_year <- function(date_string) {
  year_match <- regmatches(date_string, regexpr("\\d{4}", date_string))
  if (length(year_match) > 0 && !is.na(year_match)) {
    return(2023 - as.numeric(year_match))
  } else {
    return(NA)
  }
}

# --- Step 5: Loop through each file and process ---
for (file_path in merged_files) {
  cat("Processing:", file_path, "\n")
  
  df <- tryCatch({
    read_delim(
      file_path,
      delim = ";",
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = "c"),
      trim_ws = TRUE
    )
  }, error = function(e) {
    warning(paste("❌ Failed to read:", file_path))
    return(NULL)
  })
  
  if (is.null(df)) next  # Skip if file failed to load

  # Convert defined columns to numeric
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- df[[col]] |>
        str_replace_all("[^0-9,.-]", "") |>
        str_replace_all(",", "") |>
        as.numeric()
    }
  }

  # Add Age column
  if ("Date of incorporation" %in% names(df)) {
    df$Age <- sapply(df$`Date of incorporation`, extract_year)
  }

  # Add Leverage columns
  for (year in 2018:2023) {
    liabilities_col <- paste0("Total liabilities th EUR ", year)
    assets_col     <- paste0("Total assets th EUR ", year)
    leverage_col   <- paste0("Leverage ", year)
    
    if (liabilities_col %in% names(df) && assets_col %in% names(df)) {
      df[[leverage_col]] <- ifelse(
        !is.na(df[[assets_col]]) & df[[assets_col]] != 0,
        df[[liabilities_col]] / df[[assets_col]],
        NA
      )
    }
  }

  # Save the result to new file
  output_file <- sub("_merged.csv$", "_merged_calculated.csv", file_path)
  write_delim(df, output_file, delim = ";")
  cat("✔ Saved:", output_file, "\n\n")
}
