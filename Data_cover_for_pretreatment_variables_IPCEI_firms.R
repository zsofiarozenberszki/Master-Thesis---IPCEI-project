# Load data
IPCEI_Companies <- read.csv2("C:\\Users\\zsofi\\OneDrive - TUM\\Desktop\\TUM\\01_Master Thesis\\ICPEI data\\Control group\\IPCEI_Companies.csv", encoding = "latin1")

# View basic info
nrow(IPCEI_Companies)
colnames(IPCEI_Companies)

# Define columns of interest
columns_of_interest <- c(
  "Country.ISO.code",
  "Date.of.incorporation",
  "NACE.Rev..2..core.code..4.digits.",
  "Number.of.publications",
  "Total.assets.th.EUR.2018",
  "Operating.revenue..Turnover..th.EUR.2018",
  "ROA.using.Profit..Loss..before.tax.2018",
  "Total.liabilities.th.EUR.2018",
  "Research...development.expenses.th.EUR.2018"
)

# Subset only these columns
df_subset <- IPCEI_Companies[, columns_of_interest]

# Calculate NA counts and coverage
na_counts <- colSums(is.na(df_subset))
coverage <- (1 - na_counts / nrow(df_subset)) * 100

# Combine into summary dataframe
summary_IPCEI_Companies <- data.frame(
  Column = columns_of_interest,
  Total_Rows = nrow(df_subset),
  NA_Count = na_counts,
  Coverage_Percentage = round(coverage, 2)
)

# Rank by highest coverage
summary_IPCEI_Companies <- summary_IPCEI_Companies[order(-summary_IPCEI_Companies$Coverage_Percentage), ]

# Print the ranked summary
print(summary_IPCEI_Companies)

# Load required library
library(ggplot2)

# Bar plot of coverage
ggplot(summary_IPCEI_Companies, aes(x = reorder(Column, Coverage_Percentage), y = Coverage_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # flips x and y for readability
  labs(
    title = "Data Coverage for pre-treatment variables",
    x = "Column",
    y = "Coverage (%)"
  ) +
  theme_minimal(base_size = 12)
