# Install required packages if not already installed
install.packages(c("tidyverse", "rnaturalearth", "rnaturalearthdata", "sf"))

# Load libraries
library(tidyverse); library(rnaturalearth); library(rnaturalearthdata); library(sf)

# Load the data
IPCEI_Projects <- read.csv2("C://Users//zsofi//OneDrive - TUM//Desktop//TUM//01_Master Thesis//ICPEI data//CSV_files//Merged data////IPCEI_Projects_Battery_Hydrogen_Microelectronics_comprehensive.csv")

colnames(IPCEI_Projects)
########################################################################################################
################SPATIAL DISTRIBUTION MAPS OF IPCEI PROJECTS#########################################################

# Load necessary libraries (assuming already installed)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)

# Function to plot IPCEI map by project round
plot_ipcei_map <- function(data, project_round, title, low_color, high_color) {
  # Filter and count projects by country and round
  df <- data %>%
    filter(IPCEI_Round == project_round) %>%
    count(Country, name = "Project_Count")
  
  # Load and filter Europe (excluding Russia)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  europe <- world %>%
    filter(region_un == "Europe", name != "Russia")
  
  # Merge country shapes with project data
  europe_data <- left_join(europe, df, by = c("name" = "Country"))
  
  # Generate the map
  ggplot(europe_data) +
    geom_sf(aes(fill = Project_Count)) +
    scale_fill_gradient(low = low_color, high = high_color, na.value = "gray90") +
    labs(title = title, fill = "Projects") +
    coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
    theme_minimal()
}

# Create maps for each IPCEI round
map_hydrogen <- plot_ipcei_map(IPCEI_Projects, "Hydrogen", "Hydrogen Round",
                                low_color = "#00a8d5", high_color = "#003B45")

map_battery <- plot_ipcei_map(IPCEI_Projects, "Battery", "Battery Round",
                               low_color = "#c1d11f", high_color = "#00610e")

map_microelectronics <- plot_ipcei_map(IPCEI_Projects, "Microelectronics", "Microelectronics Round",
                                        low_color = "#ffe1b5", high_color = "#cc5500")

# Display individual maps
print(map_hydrogen)
print(map_battery)
print(map_microelectronics)

# Combine maps into one layout
combined_map <- map_hydrogen + map_battery + map_microelectronics +
  plot_layout(ncol = 3) +
  plot_annotation(title = "IPCEI Project Distribution by Country and Round")

# Display combined map
print(combined_map)

# Function to compute summary stats by IPCEI round
summarise_ipcei_round <- function(data, project_round) {
  df <- data %>%
    filter(IPCEI_Round == project_round) %>%
    count(Country, name = "Project_Count")
  
  total_projects <- sum(df$Project_Count)
  num_countries <- n_distinct(df$Country)
  top_country <- df %>% arrange(desc(Project_Count)) %>% slice(1)
  mean_projects <- mean(df$Project_Count)
  median_projects <- median(df$Project_Count)
  
  cat("\n--------------------------------------------------\n")
  cat("Summary for IPCEI Round:", project_round, "\n")
  cat("Total number of projects:", total_projects, "\n")
  cat("Number of participating countries:", num_countries, "\n")
  cat("Country with the most projects:", top_country$Country, 
      "(", top_country$Project_Count, "projects )\n")
  cat("Mean number of projects per country:", round(mean_projects, 2), "\n")
  cat("Median number of projects per country:", median_projects, "\n")
  cat("--------------------------------------------------\n")
}

# Print summary statistics for each round
summarise_ipcei_round(IPCEI_Projects, "Hydrogen")
summarise_ipcei_round(IPCEI_Projects, "Battery")
summarise_ipcei_round(IPCEI_Projects, "Microelectronics")


########################################################################################################
################COUNTRY AND YEAR DISTRIBUTION CHARTS#########################################################

# Function to create stacked bar chart by country and year per IPCEI round
plot_ipcei_barchart <- function(data, ipcei_round, title, fill_colors) {
  df <- data %>%
    filter(IPCEI_Round == ipcei_round) %>%
    count(Country, Year) %>%
    rename(Project_Count = n)

  # Ensure Year is treated as an ordered factor
  df$Year <- factor(df$Year, levels = sort(unique(df$Year)))

  ggplot(df, aes(x = Country, y = Project_Count, fill = Year)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = fill_colors) +
    labs(title = title, x = "Country", y = "Number of Projects", fill = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Define fill colors for each round's years
hydrogen_colors <- c("2022" = "#00a8d5", "2024" = "#003B45")
battery_colors <- c("2019" = "#c1d11f", "2021" = "#00610e")
microelectronics_colors <- c("2018" = "#ffe1b5", "2023" = "#cc5500")

# Generate bar charts
bar_hydrogen <- plot_ipcei_barchart(IPCEI_Projects, "Hydrogen", "Hydrogen", hydrogen_colors)
bar_battery <- plot_ipcei_barchart(IPCEI_Projects, "Battery", "Battery", battery_colors)
bar_microelectronics <- plot_ipcei_barchart(IPCEI_Projects, "Microelectronics", "Microelectronics", microelectronics_colors)

# Display charts
print(bar_hydrogen)
print(bar_battery)
print(bar_microelectronics)

# Combine with patchwork
library(patchwork)
combined_barcharts <- bar_hydrogen + bar_battery + bar_microelectronics +
  plot_layout(ncol = 3) +
  plot_annotation(title = "IPCEI Projects by Country and Year")

print(combined_barcharts)


########################################################################################################
################COMPANY SIZE DISTRIBUTION CHARTS#########################################################

# Define company size levels in logical order
size_levels <- c("0-10", "11-50", "51-200", "201-500", "501-1000",
                 "1000-5000", "5000-10000", "10000+")

# Color schemes for each IPCEI round
hydrogen_size_colors <- c(
  "0-10" = "#ccecf5", "11-50" = "#99d8eb", "51-200" = "#66c3e0",
  "201-500" = "#33afd6", "501-1000" = "#009bcc",
  "1000-5000" = "#007fa6", "5000-10000" = "#006280", "10000+" = "#003B45"
)
battery_size_colors <- c(
  "0-10" = "#e6edbd", "11-50" = "#d3e58d", "51-200" = "#c1d11f",
  "201-500" = "#99b71a", "501-1000" = "#779f16",
  "1000-5000" = "#558712", "5000-10000" = "#336f0e", "10000+" = "#00610e"
)
micro_size_colors <- c(
  "0-10" = "#fff0d9", "11-50" = "#ffe1b5", "51-200" = "#ffd28e",
  "201-500" = "#ffc468", "501-1000" = "#ffb541",
  "1000-5000" = "#ff9c1a", "5000-10000" = "#e07300", "10000+" = "#cc5500"
)

# Updated plotting function
plot_company_size_distribution <- function(data, project_round, title, fill_colors) {
  df <- data %>%
    filter(IPCEI_Round == project_round,
           !is.na(Year)) %>%
    rename(Employee_Size = Employee_number_LinkedIn) %>%
    filter(!is.na(Employee_Size)) %>%
    count(Year, Employee_Size) %>%
    rename(Count = n) %>%
    mutate(Employee_Size = factor(Employee_Size, levels = size_levels))

  ggplot(df, aes(x = as.factor(Year), y = Count, fill = Employee_Size)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = fill_colors, drop = FALSE) +
    labs(title = title, x = "Year", y = "Number of Companies", fill = "Company Size") +
    theme_minimal()
}

# Create plots for each IPCEI round
size_hydrogen <- plot_company_size_distribution(IPCEI_Projects, "Hydrogen", "Hydrogen", hydrogen_size_colors)
size_battery <- plot_company_size_distribution(IPCEI_Projects, "Battery", "Battery", battery_size_colors)
size_micro <- plot_company_size_distribution(IPCEI_Projects, "Microelectronics", "Microelectronics", micro_size_colors)

# Combine the three plots
library(patchwork)
combined_sizes <- size_hydrogen + size_battery + size_micro +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Company Size Distribution per IPCEI Type")

print(combined_sizes)

# Function to generate ranked size group and yearly breakdown
summarise_company_size_ranking_and_breakdown <- function(data, project_round) {
  df <- data %>%
    filter(IPCEI_Round == project_round, !is.na(Year)) %>%
    rename(Employee_Size = Employee_number_LinkedIn) %>%
    filter(!is.na(Employee_Size)) %>%
    mutate(Employee_Size = factor(Employee_Size, levels = size_levels))

  # Ranked size groups by total company count
  ranked_sizes <- df %>%
    count(Employee_Size) %>%
    arrange(desc(n)) %>%
    mutate(Rank = row_number())

  cat("\n--------------------------------------------------\n")
  cat("IPCEI Round:", project_round, "\n")
  cat("Ranked Company Size Groups:\n")
  print(ranked_sizes)

  # Yearly breakdown of companies per size group
  yearly_breakdown <- df %>%
    count(Year, Employee_Size) %>%
    pivot_wider(names_from = Year, values_from = n, values_fill = 0)

  cat("\nBreakdown of Company Size by Year:\n")
  print(yearly_breakdown)
  cat("--------------------------------------------------\n")
}

# Generate stats for all rounds
summarise_company_size_ranking_and_breakdown(IPCEI_Projects, "Hydrogen")
summarise_company_size_ranking_and_breakdown(IPCEI_Projects, "Battery")
summarise_company_size_ranking_and_breakdown(IPCEI_Projects, "Microelectronics")


########################################################################################################
################OPERATING REVENUE CHARTS#########################################################

plot_ranked_turnover_points <- function(data, project_round, point_color) {
  df <- data %>%
    # Filter to relevant round, with valid company and revenue values
    filter(IPCEI_Round == project_round,
           !is.na(`Operating.revenue..Turnover..th.EUR.2023`),
           !is.na(Company_Name)) %>%
    
    # Clean and convert turnover to numeric
    mutate(Revenue_2023 = as.numeric(gsub(",", "", `Operating.revenue..Turnover..th.EUR.2023`))) %>%
    
    # One point per company (sum if duplicated)
    group_by(Company_Name) %>%
    summarise(Revenue_2023 = sum(Revenue_2023, na.rm = TRUE)) %>%
    arrange(desc(Revenue_2023)) %>%
    mutate(Rank = row_number())

  # Create dot plot
  ggplot(df, aes(x = Rank, y = Revenue_2023)) +
    geom_point(color = point_color, size = 3, alpha = 0.8) +
    labs(
      title = paste("Ranked Turnover (2023) –", project_round),
      x = "Company Rank (Highest to Lowest)",
      y = "Turnover (th EUR)"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
}

dot_hydrogen <- plot_ranked_turnover_points(IPCEI_Projects, "Hydrogen", "#00a8d5")
dot_battery <- plot_ranked_turnover_points(IPCEI_Projects, "Battery", "#c1d11f")
dot_micro <- plot_ranked_turnover_points(IPCEI_Projects, "Microelectronics", "#cc5500")

# Show individually
print(dot_hydrogen)
print(dot_battery)
print(dot_micro)

# Combine (optional)
library(patchwork)
combined_dots <- dot_hydrogen / dot_battery / dot_micro +
  plot_annotation(title = "Company Turnover in 2023 by IPCEI Round (Ranked - Dot Plot)")

print(combined_dots)

###Summary statistics

summarise_turnover_descriptive <- function(data, project_round) {
  # Prepare data with numeric turnover
  df <- data %>%
    filter(IPCEI_Round == project_round) %>%
    mutate(
      Revenue_2023 = as.numeric(gsub(",", "", `Operating.revenue..Turnover..th.EUR.2023`))
    )

  # Aggregate to one row per company
  df_clean <- df %>%
    filter(!is.na(Company_Name)) %>%
    group_by(Company_Name) %>%
    summarise(Revenue_2023 = sum(Revenue_2023, na.rm = TRUE), .groups = "drop")

  # Count total NAs in the original column (not per company, but total values)
  na_count <- sum(is.na(df$`Operating.revenue..Turnover..th.EUR.2023`))

  with_data <- sum(!is.na(df_clean$Revenue_2023))
  mean_turnover <- round(mean(df_clean$Revenue_2023, na.rm = TRUE), 2)
  median_turnover <- round(median(df_clean$Revenue_2023, na.rm = TRUE), 2)
  min_turnover <- round(min(df_clean$Revenue_2023, na.rm = TRUE), 2)
  max_turnover <- round(max(df_clean$Revenue_2023, na.rm = TRUE), 2)

  # Print summary
  cat("\n--------------------------------------------------\n")
  cat("Turnover Summary for IPCEI Round:", project_round, "\n\n")
  cat("Number of companies with turnover data: ", with_data, "\n")
  cat("Number of NA values in 2023 turnover column: ", na_count, "\n\n")
  cat("Mean turnover (2023): ", format(mean_turnover, big.mark = ","), "th EUR\n")
  cat("Median turnover (2023): ", format(median_turnover, big.mark = ","), "th EUR\n")
  cat("Minimum turnover (2023): ", format(min_turnover, big.mark = ","), "th EUR\n")
  cat("Maximum turnover (2023): ", format(max_turnover, big.mark = ","), "th EUR\n")
  cat("--------------------------------------------------\n")
}

summarise_turnover_descriptive(IPCEI_Projects, "Hydrogen")
summarise_turnover_descriptive(IPCEI_Projects, "Battery")
summarise_turnover_descriptive(IPCEI_Projects, "Microelectronics")

###############################################################################################
############ HISTOGRAM OF COMPANY AGE##########################################################

library(dplyr)
library(ggplot2)
library(stringr)

# Compute Incorporation Year and Age
IPCEI_Projects <- IPCEI_Projects %>%
  mutate(
    Incorporation_Year = str_extract(Date.of.incorporation, "\\d{4}") %>% as.integer(),
    Company_Age = 2025 - Incorporation_Year
  )

# Function to plot histogram of company ages
plot_company_age_distribution <- function(data, round_name, fill_color) {
  df <- data %>%
    filter(IPCEI_Round == round_name, !is.na(Company_Age)) %>%
    distinct(Company_Name, Company_Age)

  ggplot(df, aes(x = Company_Age)) +
    geom_histogram(binwidth = 5, fill = fill_color, color = "white", boundary = 0) +
    labs(
      title = paste("Company Age Distribution –", round_name),
      x = "Company Age (Years)",
      y = "Number of Companies"
    ) +
    theme_minimal()
}

# Generate plots for each IPCEI round
age_hydrogen <- plot_company_age_distribution(IPCEI_Projects, "Hydrogen", "#00a8d5")
age_battery <- plot_company_age_distribution(IPCEI_Projects, "Battery", "#c1d11f")
age_micro <- plot_company_age_distribution(IPCEI_Projects, "Microelectronics", "#cc5500")

# Display individually
print(age_hydrogen)
print(age_battery)
print(age_micro)

# Optional: Combine into one view
library(patchwork)
combined_age_plot <- age_hydrogen / age_battery / age_micro +
  plot_annotation(title = "Company Age Distribution by IPCEI Round")

print(combined_age_plot)

#####Summary statistics
library(dplyr)
library(stringr)

summarise_company_age_descriptive <- function(data, round_name) {
  df <- data %>%
    filter(IPCEI_Round == round_name, !is.na(Company_Name)) %>%
    mutate(
      Incorporation_Year = str_extract(`Date.of.incorporation`, "\\d{4}") %>% as.integer(),
      Company_Age = 2025 - Incorporation_Year
    ) %>%
    distinct(Company_Name, Company_Age)  # 1 row per company

  # Separate valid and missing age
  age_data <- df %>% filter(!is.na(Company_Age))
  na_count <- df %>% filter(is.na(Company_Age)) %>% nrow()

  # Summary stats
  total <- nrow(age_data)
  mean_age <- round(mean(age_data$Company_Age), 1)
  median_age <- median(age_data$Company_Age)
  min_age <- min(age_data$Company_Age)
  max_age <- max(age_data$Company_Age)

  # Print summary
  cat("\n--------------------------------------------------\n")
  cat("Company Age Summary for IPCEI Round:", round_name, "\n\n")
  cat("Number of companies with valid age data:", total, "\n")
  cat("Number of NA values (missing or invalid year):", na_count, "\n\n")
  cat("Mean company age: ", mean_age, "years\n")
  cat("Median company age: ", median_age, "years\n")
  cat("Youngest company: ", min_age, "years old\n")
  cat("Oldest company: ", max_age, "years old\n")
  cat("--------------------------------------------------\n")
}

summarise_company_age_descriptive(IPCEI_Projects, "Hydrogen")
summarise_company_age_descriptive(IPCEI_Projects, "Battery")
summarise_company_age_descriptive(IPCEI_Projects, "Microelectronics")


##################################################################################
#############NUMBER OF PUBLICATIONS###############################################

plot_ranked_publications <- function(data, round_name, point_color) {
  df <- data %>%
    filter(IPCEI_Round == round_name,
           !is.na(Number.of.publications),
           !is.na(Company_Name)) %>%
    mutate(Publications = as.numeric(Number.of.publications)) %>%
    group_by(Company_Name) %>%
    summarise(Publications = sum(Publications, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Publications)) %>%
    mutate(Rank = row_number())

  ggplot(df, aes(x = Rank, y = Publications)) +
    geom_point(color = point_color, size = 3, alpha = 0.8) +
    labs(
      title = paste("Ranked Number of Publications –", round_name),
      x = "Company Rank (Most to Least)",
      y = "Number of Publications"
    ) +
    theme_minimal()
}

pub_hydrogen <- plot_ranked_publications(IPCEI_Projects, "Hydrogen", "#00a8d5")
pub_battery <- plot_ranked_publications(IPCEI_Projects, "Battery", "#c1d11f")
pub_micro <- plot_ranked_publications(IPCEI_Projects, "Microelectronics", "#cc5500")

# Show plots
print(pub_hydrogen)
print(pub_battery)
print(pub_micro)

# Optionally combine
library(patchwork)
combined_publications <- pub_hydrogen / pub_battery / pub_micro +
  plot_annotation(title = "Ranked Number of Publications by IPCEI Round")

print(combined_publications)

##Summary statistics
summarise_publications_stats <- function(data, round_name) {
  df <- data %>%
    filter(IPCEI_Round == round_name,
           !is.na(Company_Name)) %>%
    mutate(Publications = as.numeric(Number.of.publications)) %>%
    group_by(Company_Name) %>%
    summarise(Publications = sum(Publications, na.rm = TRUE), .groups = "drop")

  # Valid and missing counts
  valid_df <- df %>% filter(!is.na(Publications))
  na_count <- data %>%
    filter(IPCEI_Round == round_name, is.na(Number.of.publications)) %>%
    distinct(Company_Name) %>%
    nrow()

  total <- nrow(valid_df)
  mean_pub <- round(mean(valid_df$Publications), 1)
  median_pub <- median(valid_df$Publications)
  min_pub <- min(valid_df$Publications)
  max_pub <- max(valid_df$Publications)

  # Print summary
  cat("\n--------------------------------------------------\n")
  cat("📚 Publications Summary for IPCEI Round:", round_name, "\n\n")
  cat("Number of companies with publication data:", total, "\n")
  cat("Number of NA values (missing):", na_count, "\n\n")
  cat("Mean publications:   ", mean_pub, "\n")
  cat("Median publications: ", median_pub, "\n")
  cat("Min publications:    ", min_pub, "\n")
  cat("Max publications:    ", max_pub, "\n")
  cat("--------------------------------------------------\n")
}

summarise_publications_stats(IPCEI_Projects, "Hydrogen")
summarise_publications_stats(IPCEI_Projects, "Battery")
summarise_publications_stats(IPCEI_Projects, "Microelectronics")



####################################################################################
######### INDUSTRY CLASSIFICATIONS #################################################

library(ggplot2)
library(dplyr)

plot_nace_pie <- function(data, round_name, base_colors) {
  df <- data %>%
    filter(IPCEI_Round == round_name, !is.na(NACE)) %>%
    mutate(NACE = as.character(NACE)) %>%
    count(NACE) %>%
    mutate(Share = n / sum(n) * 100)

  # Adjust color vector to match number of NACE codes
  num_nace <- nrow(df)
  fill_colors <- rep(base_colors, length.out = num_nace)

  ggplot(df, aes(x = "", y = Share, fill = NACE)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(
      title = paste("NACE Sector Distribution –", round_name),
      fill = "NACE Code"
    ) +
    theme_void() +
    scale_fill_manual(values = fill_colors)
}

battery_colors <- c("#c1d11f", "#99b71a", "#779f16", "#558712", "#336f0e", "#00610e")
hydrogen_colors <- c("#00a8d5", "#33afd6", "#66c3e0", "#99d8eb", "#ccecf5", "#003B45")
micro_colors <- c("#ffe1b5", "#ffd28e", "#ffc468", "#ff9c1a", "#e07300", "#cc5500")

pie_battery <- plot_nace_pie(IPCEI_Projects, "Battery", battery_colors) %>% print()
pie_hydrogen <- plot_nace_pie(IPCEI_Projects, "Hydrogen", hydrogen_colors) %>% print()
pie_micro <- plot_nace_pie(IPCEI_Projects, "Microelectronics", micro_colors) %>% print()


# Combine and display them side by side
library(patchwork)

combined_pies <- pie_battery + pie_hydrogen + pie_micro +
  plot_layout(ncol = 3) +
  plot_annotation(title = "NACE Sector Distribution by IPCEI Round")

print(combined_pies)

summarise_nace_descriptions <- function(data, round_name) {
  df <- data %>%
    filter(IPCEI_Round == round_name, !is.na(NACE), !is.na(NACE_description)) %>%
    distinct(NACE, NACE_description) %>%
    arrange(as.numeric(as.character(NACE)))

  cat("\n--------------------------------------------------\n")
  cat("📘 NACE Sector Descriptions –", round_name, "\n\n")
  for (i in 1:nrow(df)) {
    cat("•", df$NACE[i], ":", df$NACE_description[i], "\n")
  }
  cat("--------------------------------------------------\n")
}

summarise_nace_descriptions(IPCEI_Projects, "Battery")
summarise_nace_descriptions(IPCEI_Projects, "Hydrogen")
summarise_nace_descriptions(IPCEI_Projects, "Microelectronics")

