# --- Load libraries ---
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# --- Helper function ---
analyze_outcome <- function(data, var_label, pretty_name) {
  treated <- data %>% filter(Treatment == 1) %>% pull(var_label)
  control <- data %>% filter(Treatment == 0) %>% pull(var_label)
  
  ttest <- t.test(treated, control, var.equal = FALSE)
  
  results <- data.frame(
    Variable   = pretty_name,
    ATT        = mean(treated, na.rm = TRUE) - mean(control, na.rm = TRUE),
    p_value    = ttest$p.value,
    CI_lower   = ttest$conf.int[1],
    CI_upper   = ttest$conf.int[2],
    N_treated  = sum(!is.na(treated)),
    N_control  = sum(!is.na(control)),
    Mean_Treat = mean(treated, na.rm = TRUE),
    Mean_Ctrl  = mean(control, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  boxplot <- ggplot(data, aes(x = factor(Treatment, labels = c("Control", "Treated")),
                              y = .data[[var_label]])) +
    geom_boxplot(fill = "lightblue", outlier.color = "red") +
    labs(title = paste0("Boxplot – ", pretty_name, " (2023)"),
         x = "Group", y = pretty_name) +
    theme_minimal()
  
  density <- ggplot(data, aes(x = .data[[var_label]],
                              fill = factor(Treatment, labels = c("Control", "Treated")))) +
    geom_density(alpha = 0.5) +
    labs(title = paste0("Density – ", pretty_name, " (2023)"),
         x = pretty_name, fill = "Group") +
    theme_minimal()
  
  list(results = results,
       combined = boxplot | density)
}

# =====================================================
# --- File 1: option3/spec3 ---
# =====================================================
file1 <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option3_spec3.csv"
df1 <- read_delim(file1, delim = ";", locale = locale(decimal_mark = ","))

turnover_out_f1 <- analyze_outcome(df1,
  "std_Operating revenue (Turnover) th EUR 2023",
  "Turnover (standardised)")

roa_out_f1 <- analyze_outcome(df1,
  "std_ROA using Profit (Loss) before tax 2023",
  "ROA (standardised)")

# =====================================================
# --- File 2: option5/spec3 ---
# =====================================================
file2 <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option5_spec3.csv"
df2 <- read_delim(file2, delim = ";", locale = locale(decimal_mark = ","))

rd_out_f2 <- analyze_outcome(df2,
  "std_Research & development expenses th EUR 2023",
  "R&D Expenses (standardised)")

turnover_out_f2 <- analyze_outcome(df2,
  "std_Operating revenue (Turnover) th EUR 2023",
  "Turnover (standardised)")

roa_out_f2 <- analyze_outcome(df2,
  "std_ROA using Profit (Loss) before tax 2023",
  "ROA (standardised)")

# =====================================================
# --- Print specific plots ---
# =====================================================

# ROA plots
print(roa_out_f1$combined)   # from Option 3 spec 3
print(roa_out_f2$combined)   # from Option 5 spec 3

# Turnover plots
print(turnover_out_f1$combined) # from Option 3 spec 3
print(turnover_out_f2$combined) # from Option 5 spec 3

# R&D plot (only from Option 5 spec 3)
print(rd_out_f2$combined)

all_results <- dplyr::bind_rows(
  dplyr::bind_cols(File = "Option3_spec3", turnover_out_f1$results),
  dplyr::bind_cols(File = "Option3_spec3", roa_out_f1$results),
  dplyr::bind_cols(File = "Option5_spec3", rd_out_f2$results),
  dplyr::bind_cols(File = "Option5_spec3", turnover_out_f2$results),
  dplyr::bind_cols(File = "Option5_spec3", roa_out_f2$results)
)

print(all_results)
