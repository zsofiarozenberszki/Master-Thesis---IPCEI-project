###########################################################################
########## Paralell trends for Option 2 specification 1####################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(patchwork)

# --- Load data ---
file_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option2_spec1.csv"
df <- read_delim(file_path, delim = ";", locale = locale(encoding = "latin1"))

# --- Ensure correct numeric conversion (comma to dot for decimals) ---
numeric_cols <- grep("^std_|^ROA|^Operating revenue", names(df), value = TRUE)
df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(",", ".", x)))

# --- Clean IPCEI_Type formatting ---
df$IPCEI_Type <- trimws(as.character(df$IPCEI_Type))

# --- Define plotting function ---
make_plot <- function(df, var_prefix, var_label, ipcei_type, vline_years) {
  df_long <- df %>%
    filter(IPCEI_Type == ipcei_type) %>%
    select(`Company name`, Treatment, IPCEI_Type, starts_with(var_prefix)) %>%
    pivot_longer(cols = starts_with(var_prefix), names_to = "Year", values_to = "value") %>%
    mutate(
      Year = as.numeric(str_extract(Year, "\\d{4}")),
      Treatment = factor(Treatment, levels = c(0, 1), labels = c("Control", "Treated"))
    ) %>%
    filter(!is.na(value), Year >= 2018, Year <= 2023)

  ggplot(df_long, aes(x = Year, y = value, color = Treatment)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
    geom_vline(xintercept = vline_years, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("Control" = "steelblue", "Treated" = "darkred")) +
    labs(
      title = paste("Parallel Trends –", ipcei_type, "|", var_label),
      x = "Year",
      y = paste("Mean", var_label, "(standardised)"),
      color = "Group"
    ) +
    theme_minimal(base_size = 13)
}

# --- Define vertical lines for each IPCEI type ---
vlines <- list(
  Battery = c(2019, 2021),
  Hydrogen = c(2022, 2024),
  Microelectronics = c(2018, 2023)
)

# --- Generate plots ---
ipcei_types <- unique(na.omit(df$IPCEI_Type))
plots <- list()

for (type in ipcei_types) {
  p1 <- make_plot(df, "std_Operating revenue (Turnover) th EUR", "Turnover", type, vlines[[type]])
  p2 <- make_plot(df, "std_ROA using Profit (Loss) before tax", "ROA", type, vlines[[type]])
  p3 <- make_plot(df, "std_Total assets th EUR", "Total Assets", type, vlines[[type]])
  plots[[type]] <- p1 / p2 / p3
}

# --- Display plots ---
plots$Battery
plots$Hydrogen
plots$Microelectronics

###########################################################################
########## Paralell trends for Option 2 specification 3####################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(patchwork)

# --- Load data ---
file_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option2_spec3.csv"
df <- read_delim(file_path, delim = ";", locale = locale(encoding = "latin1"))

# --- Ensure correct numeric conversion (comma to dot for decimals) ---
numeric_cols <- grep("^std_|^ROA|^Operating revenue", names(df), value = TRUE)
df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(",", ".", x)))

# --- Define plotting function ---
make_plot <- function(df, var_prefix, var_label, ipcei_type, vline_years) {
  df_long <- df %>%
    filter(IPCEI_Type == ipcei_type) %>%
    select(`Company name`, Treatment, IPCEI_Type, starts_with(var_prefix)) %>%
    pivot_longer(cols = starts_with(var_prefix), names_to = "Year", values_to = "value") %>%
    mutate(Year = as.numeric(str_extract(Year, "\\d{4}"))) %>%
    filter(!is.na(value), Year >= 2018, Year <= 2023)

  ggplot(df_long, aes(x = Year, y = value, color = factor(Treatment))) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
    geom_vline(xintercept = vline_years, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("0" = "steelblue", "1" = "darkred"),
                       labels = c("Control", "Treated")) +
    labs(
      title = paste("Parallel Trends –", ipcei_type, "|", var_label),
      x = "Year",
      y = paste("Mean", var_label, "(standardised)"),
      color = "Group"
    ) +
    theme_minimal(base_size = 13)
}

# --- Define vertical lines for each IPCEI type ---
vlines <- list(
  Battery = c(2019, 2021),
  Hydrogen = c(2022, 2024),
  Microelectronics = c(2018, 2023)
)

# --- Generate plots ---
ipcei_types <- unique(na.omit(df$IPCEI_Type))
plots <- list()

for (type in ipcei_types) {
  p1 <- make_plot(df, "std_Operating revenue (Turnover) th EUR", "Turnover", type, vlines[[type]])
  p2 <- make_plot(df, "std_ROA using Profit (Loss) before tax", "ROA", type, vlines[[type]])
  plots[[type]] <- p1 / p2
}

# --- Display plots ---
plots$Battery
plots$Hydrogen
plots$Microelectronics

###########################################################################
########## Paralell trends for Option 5 specification 3####################

# --- Load required libraries ---
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(patchwork)

# --- Load data ---
file_path <- "C:/Users/zsofi/OneDrive - TUM/Desktop/TUM/01_Master Thesis/ICPEI data/Control group/z_Matching_result_files/Matched_Treated_Control_firms_all_variables_option5_spec3.csv"
df <- read_delim(file_path, delim = ";", locale = locale(encoding = "latin1"))

# --- Ensure correct numeric conversion (comma to dot for decimals) ---
numeric_cols <- grep("^std_|^ROA|^Operating revenue", names(df), value = TRUE)
df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(",", ".", x)))

# --- Clean up IPCEI type formatting ---
df$IPCEI_Type <- trimws(as.character(df$IPCEI_Type))

# --- Define plotting function ---
make_plot <- function(df, var_prefix, var_label, ipcei_type, vline_years) {
  df_long <- df %>%
    filter(IPCEI_Type == ipcei_type) %>%
    select(`Company name`, Treatment, IPCEI_Type, starts_with(var_prefix)) %>%
    pivot_longer(cols = starts_with(var_prefix), names_to = "Year", values_to = "value") %>%
    mutate(
      Year = as.numeric(str_extract(Year, "\\d{4}")),
      Treatment = factor(Treatment, levels = c(0, 1), labels = c("Control", "Treated"))
    ) %>%
    filter(!is.na(value), Year >= 2018, Year <= 2023)

  ggplot(df_long, aes(x = Year, y = value, color = Treatment)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
    geom_vline(xintercept = vline_years, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("Control" = "steelblue", "Treated" = "darkred")) +
    labs(
      title = paste("Parallel Trends –", ipcei_type, "|", var_label),
      x = "Year",
      y = paste("Mean", var_label, "(standardised)"),
      color = "Group"
    ) +
    theme_minimal(base_size = 13)
}

# --- Define vertical lines for each IPCEI type ---
vlines <- list(
  Battery = c(2019, 2021),
  Hydrogen = c(2022, 2024),
  Microelectronics = c(2018, 2023)
)

# --- Generate plots ---
ipcei_types <- unique(na.omit(df$IPCEI_Type))
plots <- list()

for (type in ipcei_types) {
  p1 <- make_plot(df, "std_Operating revenue (Turnover) th EUR", "Turnover", type, vlines[[type]])
  p2 <- make_plot(df, "std_ROA using Profit (Loss) before tax", "ROA", type, vlines[[type]])
  p3 <- make_plot(df, "std_Research & development expenses th EUR", "R&D Expenses", type, vlines[[type]])
  plots[[type]] <- p1 / p2 / p3
}

# --- Display plots ---
plots$Battery
plots$Hydrogen
plots$Microelectronics

