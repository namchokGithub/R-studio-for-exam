# Graph
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

salaries <- read.csv("salaries.csv")

ggplot(data = salaries, mapping = aes(x=yrs.service, y=salary, color=rank)) + 
  geom_smooth(method = "lm", size = 0.2, se = FALSE) + 
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(~discipline) + 
  labs(title = "Relationship between salary and year of service"
       , x = "Years of Services", y = "Salary (Bath)")

# Flip bar chart 
avg_salary <- salaries %>% group_by(rank) %>% summarise(avg_salary = mean(salary, na.rm = TRUE))

ggplot(avg_salary, mapping = aes(reorder(x=rank, avg_salary), y=avg_salary)) + 
  geom_bar(stat = "identity", fill = c("#A1E8E4", "#FC6472", "#ECCDB3") ,alpha = 0.8 , color = "black") + 
  coord_flip()+ 
  labs(title = "Mean salary by rank"
       , x = "Rank", y = "Mean Salary") + geom_text(aes(label = avg_salary), vjust = -0.1) 


# Find range of number of year service
years <- mutate(salaries,
                years=ifelse(yrs.service < 10, "Lower 10 year"
                             ,ifelse(yrs.service >= 10 & yrs.service <= 20, "Between 10 - 20 year", "Higher 20 year"))
                ) %>% 
          group_by(years) %>% 
          count(years)

ggplot(data = years,
       mapping = aes(x = reorder(years, n),y=n)) +
  geom_bar(stat = "identity", fill = c("#A1E8E4", "#FC6472", "#ECCDB3")) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Year service by number of year"
       , x = "Level", y = "Frequency") + geom_text(aes(label = n), vjust = -0.25)


# change the order the levels for the categorical variable "class" 
mpg$manufacturer = factor(mpg$manufacturer, levels = c("audi", "chevrolet", "dodge", "ford", "honda", "hyundai", "jeep"
                                                       , "land rover", "lincoln", "mercury", "subaru", "toyota", "volkswagen"))

# create a summary dataset 
plotdata <- mpg %>% group_by(manufacturer, year) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n), lbl = scales::percent(pct)) 

ggplot(plotdata, aes(x = factor(manufacturer, levels = c("audi", "chevrolet", "dodge", "ford", "honda", "hyundai", "jeep"
                                                         , "land rover", "lincoln", "mercury", "subaru", "toyota", "volkswagen")), 
                     y = pct, fill = factor(year, levels = c("1999", "2008"), labels = c("1999", "2008")))) +
  
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent) + 
  geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) + 
  scale_fill_brewer(palette = "Accent") + 
  labs(y = "Percent", fill = "Year", x = "Class", title = "Year sales by Manufaclurer") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


