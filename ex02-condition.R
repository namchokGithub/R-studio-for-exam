# Condition 

library(readr)

# Exel ,spreadsheets
library(readxl)

# Statustucak packeages
library(haven)

# Selecting variable
library(dplyr)

# reshape data
library(tidyr)

#------------------ End library -------------------- #

data <- read.csv("salaries.csv")

# Find mena of year service
meanOfYearService <- data %>% group_by(rank) %>% summarise(yearAvg = mean(yrs.service, na.rm = TRUE))

# Find average of salary and check it's hight or low average
checkAverageOfSalary <- data %>% select(rank, salary) %>% 
                        mutate(salary_level = ifelse(salary >= mean(salary, na.rm = TRUE), "Hight average", "Low average"))

# Distinct == Not the same value
rankRow <- distinct(data, rank)
fullRankRow <- mutate(rankRow, fullRank = ifelse(rank=="AssocProf","Associate Professor", 
                                                 ifelse(rank=="AsstProf","Assistant Professor","Professor")))

# Group and summarize
answer <- data %>% filter(yrs.service >= 20 & yrs.since.phd >= 20) %>% 
          group_by(rank, sex) %>% summarize(mean_salary = mean(salary))

# Gather
gatherValue <- gather(answer, key="variable", value="value", sex:mean_salary)







