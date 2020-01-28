# Tutorial for R language

#---------------------
# Library Section    |
#---------------------

# For graph
library(ggplot2)
# 
library(dplyr) 
# Tree map
library(treemapify) 
# Scales
library(scales) 


#####################
# Univariate Graphs #
#####################

###############
# Categorical #
###############

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

#----------------
# Pie chart     |
#----------------

# create a basic ggplot2 pie chart 
plotdata <- Marriage %>% 
            count(race) %>% 
            arrange(desc(race)) %>%
            mutate(prop = round(n * 100 / sum(n), 1), lab.ypos = cumsum(prop) - 0.5 * prop) 

ggplot(plotdata, aes(x = "", y = prop, fill = race)) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme_void()

# create a pie chart with slice labels 
plotdata <- Marriage %>% 
            count(race) %>% 
            arrange(desc(race)) %>% 
            mutate(prop = round(n*100/sum(n), 1), lab.ypos = cumsum(prop) - 0.5*prop) 

plotdata$label <- paste0(plotdata$race, "\n",round(plotdata$prop), "%") 

ggplot(plotdata, aes(x = "", y = prop, fill = race)) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  geom_text(aes(y = lab.ypos, label = label), color = "black") + 
  coord_polar("y", start = 0,direction = -1) + 
  theme_void() + 
  theme(legend.position = "FALSE") + labs(title = "Participants by race")

#----------------
# Tree map      |
#----------------

# create a treemap of marriage officials 
plotdata <- Marriage %>% count(officialTitle) 

ggplot(plotdata, aes(fill = officialTitle, area = n)) + 
  geom_treemap() +
  labs(title = "Marriages by officiate")

# create a treemap with tile labels 
ggplot(plotdata, aes(fill = officialTitle, area = n, label = officialTitle)) +
  geom_treemap() + geom_treemap_text(colour = "white", place = "centre") + 
  labs(title = "Marriages by officiate") + theme(legend.position = "none") 


###############
# Qualitative #
###############

#----------------
# Histogram     |
#----------------

# plot the age distribution using a histogram 
ggplot(Marriage, aes(x = age)) + 
  geom_histogram() + 
  labs(title = "Participants by age", x = "Age") 

# plot the histogram with blue bars and white borders 
ggplot(Marriage, aes(x = age)) + 
  geom_histogram(fill = "cornflowerblue", color = "white") + 
  labs(title="Participants by age", x = "Age") 

#---------------------------
# Bins and bandwidths      |
#---------------------------

# plot the histogram with 20 bins 
ggplot(Marriage, aes(x = age)) + 
  geom_histogram(fill = "cornflowerblue", color = "white", bins = 20) + 
  labs(title="Participants by age", subtitle = "number of bins = 20", x = "Age") 

# plot the histogram with a binwidth of 5 
ggplot(Marriage, aes(x = age)) + 
  geom_histogram(fill = "cornflowerblue", color = "white", binwidth = 5) + 
  labs(title="Participants by age", subtitle = "binwidth = 5 years", x = "Age") 

# plot the histogram with percentages on the y-axis 
ggplot(Marriage, aes(x = age, y= ..count.. / sum(..count..))) + 
  geom_histogram(fill = "cornflowerblue", color = "white", binwidth = 5) + 
  labs(title="Participants by age", y = "Percent", x = "Age") + 
  scale_y_continuous(labels = percent)

#-------------------
# Density plot     |
#-------------------

# Create a kernel density plot of age 
ggplot(Marriage, aes(x = age)) + 
  geom_density() +    
  labs(title = "Participants by age")

# Create a kernel density plot of age 
ggplot(Marriage, aes(x = age)) + 
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age") 

# default bandwidth for the age variable 
bw.nrd0(Marriage$age) 
## [1] 5.181946

# Create a kernel density plot of age 
ggplot(Marriage, aes(x = age)) + 
  geom_density(fill = "deepskyblue", bw = 1) + 
  labs(title = "Participants by age", subtitle = "bandwidth = 1")

#-------------
# Dot Chart  |
#-------------

# plot the age distribution using a dotplot 
ggplot(Marriage, aes(x = age)) + 
  geom_dotplot() + 
  labs(title = "Participants by age", y = "Proportion", x = "Age")

# Plot ages as a dot plot using gold dots with black borders 
ggplot(Marriage, aes(x = age)) + 
  geom_dotplot(fill = "gold", color = "black") + 
  labs(title = "Participants by age", y = "Proportion", x = "Age") 
















