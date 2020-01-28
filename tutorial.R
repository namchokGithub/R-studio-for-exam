# Tutorial for R language

#---------------------
# Library Section    |
#---------------------

# For graph
library(ggplot2)
# 
library(dplyr) 

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
# plot.title = element_text(hjust = 0.5) same as subtitle
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = c("red", "blue", "green", "black"), color="black") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") +
  theme(plot.title = element_text(hjust = 0.5))

# plot the distribution as percentages 
ggplot(Marriage, aes(x = race, y = ..count.. / sum(..count..))) + 
  geom_bar() + 
  labs(x = "Race", y = "Percent", title = "Participants by race") + 
  scale_y_continuous(labels = scales::percent)

#----------------------
# Sorting categories  |
#----------------------

# calculate number of participants in each race category 
plotdata <- Marriage %>% count(race)

# plot the bars in ascending order 
ggplot(plotdata, aes(x = reorder(race, n), y = n)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") 

# plot the bars in descending  order 
ggplot(plotdata, aes(x = reorder(race, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") 

#-----------------
# Labeling bars  |
#-----------------

# plot the bars with numeric labels 
ggplot(plotdata, aes(x = race, y = n)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), vjust=-0.5) + 
  labs(x = "Race", y = "Frequency", title = "Participants by race") .

###

plotdata <- Marriage %>% 
            count(race) %>%
            mutate(pct = n / sum(n), pctlabel = paste0(round(pct*100), "%")) 

# plot the bars as percentages, in decending order with bar labels 
ggplot(plotdata, aes(x = reorder(race, -pct), y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") + 
  geom_text(aes(label = pctlabel), vjust = -0.25) + 
  labs(x = "Race", y = "Percent", title = "Participants by race") 

 
#----------------------
# Overlapping labels  |
#----------------------

# basic bar chart with overlapping labels 
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() + 
  labs(x = "Officiate", y = "Frequency", title = "Marriages by officiate") 

# horizontal bar chart 
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() + 
  labs(x = "", y = "Frequency", title = "Marriages by officiate") + 
  coord_flip()

# bar chart with staggered labels 
lbls <- paste0(c("", "\n"), levels(Marriage$officialTitle))

ggplot(Marriage, aes(x=factor(officialTitle, labels = lbls))) + 
  geom_bar() + 
  labs(x = "", y = "Frequency", title = "Marriages by officiate") 

# bar chart with rotated labels 
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() + 
  labs(x = "", y = "Frequency", title = "Marriages by officiate") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






















