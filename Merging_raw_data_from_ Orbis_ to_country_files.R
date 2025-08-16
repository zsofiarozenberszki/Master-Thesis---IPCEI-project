library(dplyr)

# Root folder path
root_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group"
country_folders <- list.dirs(path = root_path, recursive = FALSE)

# Desired final column order
desired_order <- c(
  "Country ISO code",
  "NACE Rev. 2, core code (4 digits)",
  "Date of incorporation",
  "Number of employees 2023",
  "Number of publications", 
  "Research & development expenses th EUR 2023",
  "Research & development expenses th EUR 2022",
  "Research & development expenses th EUR 2021",
  "Research & development expenses th EUR 2020",
  "Research & development expenses th EUR 2019",
  "Research & development expenses th EUR 2018",  
  "Total assets th EUR 2023",
  "Total assets th EUR 2022",
  "Total assets th EUR 2021",
  "Total assets th EUR 2020",
  "Total assets th EUR 2019",
  "Total assets th EUR 2018",  
  "Total liabilities th EUR 2023",
  "Total liabilities th EUR 2022",
  "Total liabilities th EUR 2021",
  "Total liabilities th EUR 2020",
  "Total liabilities th EUR 2019",
  "Total liabilities th EUR 2018",  
  "Operating revenue (Turnover) th EUR 2023",
  "Operating revenue (Turnover) th EUR 2022",
  "Operating revenue (Turnover) th EUR 2021",
  "Operating revenue (Turnover) th EUR 2020",
  "Operating revenue (Turnover) th EUR 2019",
  "Operating revenue (Turnover) th EUR 2018",  
  "ROA using Profit (Loss) before tax 2023",
  "ROA using Profit (Loss) before tax 2022",
  "ROA using Profit (Loss) before tax 2021",
  "ROA using Profit (Loss) before tax 2020",
  "ROA using Profit (Loss) before tax 2019",
  "ROA using Profit (Loss) before tax 2018"
)

# Main processing function
process_folder_columnwise <- function(folder_path) {
  folder_name <- basename(folder_path)
  
  tryCatch({
    csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    # Read first file to get company names
    first_df <- read.csv(csv_files[1], check.names = FALSE, stringsAsFactors = FALSE)
    first_df <- first_df[-1]  # Remove index column
    company_names <- first_df$`Company name`
    
    # Attempt to extract Number of publications from matching file
    pub_file <- grep("publication", csv_files, ignore.case = TRUE, value = TRUE)[1]
    
    if (!is.na(pub_file)) {
      pub_df <- read.csv(pub_file, check.names = FALSE, stringsAsFactors = FALSE)
      pub_df <- pub_df[-1]
      if ("Number of publications" %in% names(pub_df)) {
        number_of_pubs <- pub_df$`Number of publications`
      } else {
        number_of_pubs <- rep(NA, length(company_names))
      }
    } else {
      number_of_pubs <- rep(NA, length(company_names))
    }
    
    # Function to extract metrics (excluding Company name and row index)
    read_metrics_only <- function(file) {
      df <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
      df <- df[-1]
      df <- df[ , !(names(df) %in% "Company name")]
      return(df)
    }
    
    metrics_list <- lapply(csv_files, read_metrics_only)
    merged_metrics <- do.call(cbind, metrics_list)
    
    # Merge Company name, publications, and metrics
    final_df <- cbind(`Company name` = company_names,
                      `Number of publications` = number_of_pubs,
                      merged_metrics)
    
    # Reorder columns: "Company name" first, then as per desired order
    available_cols <- desired_order[desired_order %in% names(final_df)]
    ordered_df <- final_df[ , c("Company name", available_cols)]
    
    # Save output as semicolon-separated CSV
    output_file <- file.path(folder_path, paste0(folder_name, "_merged.csv"))
    write.csv2(ordered_df, file = output_file, row.names = FALSE)
    
    cat("✔ Saved semicolon-separated merged file for:", folder_name, "\n")
    return("success")
    
  }, error = function(e) {
    message <- conditionMessage(e)
    cat("✖ Error in folder:", folder_name, "\n  →", message, "\n")
    return(message)
  })
}

# Run on all country folders
results <- setNames(lapply(country_folders, process_folder_columnwise),
                    basename(country_folders))
