# Calculate etc. Summary and Mean

# Read file
library(readr)
# Exel ,spreadsheets
library(readxl)

data <- read.csv("salaries.csv")

# Find mean
meanOfYearService <- data %>% group_by(rank) %>% summarise(yearAvg = mean(yrs.service, na.rm = TRUE))

