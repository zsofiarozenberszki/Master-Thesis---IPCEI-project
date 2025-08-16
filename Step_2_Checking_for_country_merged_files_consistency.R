# Load required package
library(dplyr)

# Set the path to the folder with all *_merged.csv files
merged_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"

# Desired final column order
desired_order <- c(
  "Company name",
  "Country ISO code",
  "NACE Rev. 2, core code (4 digits)",
  "Date of incorporation",
  "Number of employees 2023",
  "Number of publications",
  "Research & development expenses th EUR 2023", "Research & development expenses th EUR 2022",
  "Research & development expenses th EUR 2021", "Research & development expenses th EUR 2020",
  "Research & development expenses th EUR 2019", "Research & development expenses th EUR 2018",
  "Total assets th EUR 2023", "Total assets th EUR 2022", "Total assets th EUR 2021",
  "Total assets th EUR 2020", "Total assets th EUR 2019", "Total assets th EUR 2018",
  "Total liabilities th EUR 2023", "Total liabilities th EUR 2022", "Total liabilities th EUR 2021",
  "Total liabilities th EUR 2020", "Total liabilities th EUR 2019", "Total liabilities th EUR 2018",
  "Operating revenue (Turnover) th EUR 2023", "Operating revenue (Turnover) th EUR 2022",
  "Operating revenue (Turnover) th EUR 2021", "Operating revenue (Turnover) th EUR 2020",
  "Operating revenue (Turnover) th EUR 2019", "Operating revenue (Turnover) th EUR 2018",
  "ROA using Profit (Loss) before tax 2023", "ROA using Profit (Loss) before tax 2022",
  "ROA using Profit (Loss) before tax 2021", "ROA using Profit (Loss) before tax 2020",
  "ROA using Profit (Loss) before tax 2019", "ROA using Profit (Loss) before tax 2018"
)

# List all *_merged.csv files
merged_files <- list.files(path = merged_path, pattern = "_merged\\.csv$", recursive = TRUE, full.names = TRUE)

# Initialize tracking
col_check_results <- list()

# Check each file
for (file in merged_files) {
  df <- read.csv2(file, check.names = FALSE, stringsAsFactors = FALSE)
  file_name <- basename(file)
  
  file_cols <- colnames(df)
  missing_cols <- setdiff(desired_order, file_cols)
  extra_cols <- setdiff(file_cols, desired_order)
  
  if (length(missing_cols) > 0 || length(extra_cols) > 0) {
    col_check_results[[file_name]] <- list(
      missing = missing_cols,
      extra = extra_cols
    )
  }
}

# Report results
if (length(col_check_results) == 0) {
  cat("✅ All files have consistent columns.\n")
} else {
  cat("⚠️ Mismatched column names found in the following files:\n\n")
  for (fname in names(col_check_results)) {
    cat("•", fname, "\n")
    if (length(col_check_results[[fname]]$missing) > 0) {
      cat("   → Missing columns:\n     ", paste(col_check_results[[fname]]$missing, collapse = ", "), "\n")
    }
    if (length(col_check_results[[fname]]$extra) > 0) {
      cat("   → Extra columns:\n     ", paste(col_check_results[[fname]]$extra, collapse = ", "), "\n")
    }
    cat("\n")
  }
