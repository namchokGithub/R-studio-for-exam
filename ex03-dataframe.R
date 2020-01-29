# Dataframe

#--------------
# Read file   |
#--------------
library(readr)

salaries <- read_csv('salaries.csv')
year <- read_tsv('10year.txt')

#--------------
# data.frame  |
#--------------

# Create list
id = 1
bestFriends = c("Mhee", "Tar", "C", "Bank")
FriendsGirlsFriends = c("Tar", "Tar", "Bank", "Mhee")
bestFriendsGig = c(5, 1, 22, 5)

# Create dataframe
data = data.frame(id, bestFriends, FriendsGirlsFriends, bestFriendsGig)

yearsold = c(50,10,6,15)

# Add culumn
data = cbind(data, yearsold)

# Change column name
colnames(data) = c("id", "b", "f", "BB", "y")






