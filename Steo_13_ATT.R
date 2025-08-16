# --- Load libraries ---
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# --- Load dataset ---
file_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option5_spec3.csv"
df <- read_delim(file_path, delim = ";", locale = locale(decimal_mark = ","))

# --- Define outcome variable ---
outcome_var <- "std_Research & development expenses th EUR 2023"

# --- ATT and t-test ---
treated <- df %>% filter(Treatment == 1) %>% pull(outcome_var)
control <- df %>% filter(Treatment == 0) %>% pull(outcome_var)

ttest <- t.test(treated, control, var.equal = FALSE)
att_results_df <- data.frame(
  Variable = outcome_var,
  ATT = mean(treated, na.rm = TRUE) - mean(control, na.rm = TRUE),
  p_value = ttest$p.value,
  CI_lower = ttest$conf.int[1],
  CI_upper = ttest$conf.int[2]
)

print(att_results_df)

# --- Create boxplot ---
boxplot_rnd <- ggplot(df, aes(x = factor(Treatment, labels = c("Control", "Treated")), 
                              y = .data[[outcome_var]])) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot – Standardised R&D Expenses (2023)", 
       x = "Group", y = "Standardised R&D Expenses") +
  theme_minimal()
print(boxplot_rnd)

# --- Create density plot ---
density_rnd <- ggplot(df, aes(x = .data[[outcome_var]], fill = factor(Treatment, labels = c("Control", "Treated")))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density – Standardised R&D Expenses (2023)", 
       x = "Standardised R&D Expenses", fill = "Group") +
  theme_minimal()
print(density_rnd)

# --- Combine plots side by side ---
boxplot_rnd | density_rnd
