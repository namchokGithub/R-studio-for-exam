# Tutorial for R language

#---------------------
# Library Section    |
#---------------------

# For graph
library(ggplot2)

#####################
# Univariate Graphs #
#####################

#----------------
# Bar chart     |
#----------------

# Load data from package "mosaicData"
data(Marriage, package = "mosaicData") 

# plot the distribution of race 
ggplot(Marriage, aes(x = race)) + geom_bar() 

# plot the distribution of race with modified colors and labels
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = "cornflowerblue", color="black") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") 

# More color
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = c("red", "blue", "green", "black"), color="black") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") 

# Label center
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = c("red", "blue", "green", "black"), color="black") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") 

















