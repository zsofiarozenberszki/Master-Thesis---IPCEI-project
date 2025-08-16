library(dplyr)

# Cleaning function to normalize strings
clean_string <- function(x) {
  x <- iconv(x, to = "UTF-8")        # Normalize encoding
  x <- gsub("[[:cntrl:]]", "", x)    # Remove control characters
  x <- gsub("\\s+", " ", x)          # Collapse multiple spaces
  trimws(tolower(x))
}

# IPCEI file path
ipcei_file <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/CSV_files/Merged data/IPCEI_Projects_Battery_Hydrogen_Microelectronics_comprehensive.csv"

# Folder with country merged files
country_folder <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"

# Read IPCEI companies and clean
ipcei_df <- read.csv2(ipcei_file, stringsAsFactors = FALSE, check.names = FALSE)
ipcei_companies <- unique(clean_string(ipcei_df$Company_name_Orbis))

# Collect and clean all company names from all country files
country_files <- list.files(path = country_folder, pattern = "_merged\\.csv$", recursive = TRUE, full.names = TRUE)

control_companies <- unique(unlist(lapply(country_files, function(file) {
  df <- read.csv2(file, stringsAsFactors = FALSE, check.names = FALSE)
  clean_string(df$`Company name`)
})))

# Compare
unmatched_companies <- setdiff(ipcei_companies, control_companies)

# Output result
if (length(unmatched_companies) == 0) {
  cat("✅ All IPCEI companies are present in the country merged files.\n")
} else {
  cat("⚠️ The following IPCEI companies are NOT found:\n\n")
  print(unmatched_companies)
}
