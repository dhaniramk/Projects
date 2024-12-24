install.packages('tidyverse')
install.packages('here')
install.packages('skimr')
install.packages('janitor')
install.packages('data.table')
install.packages('lubridate')

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(data.table)
library('lubridate')

#Import Data
daily_activity <- read.csv("C:\\Users\\dhani\\OneDrive\\Desktop\\Bellabeat\\dailyActivity_merged.csv")
sleep_day <- read.csv("C:\\Users\\dhani\\OneDrive\\Desktop\\Bellabeat\\sleepDay_merged.csv")

## Explore these tables

# Let's see what we have
head(daily_activity)
colnames(daily_activity)
glimpse(daily_activity)
skim_without_charts(daily_activity)
str(daily_activity)

head(sleep_day)
colnames(sleep_day)
glimpse(sleep_day)
skim_without_charts(sleep_day)
str(sleep_day)

# Let's format the headers consistently and check that each field name is unique and consistent
daily_activity<- rename_with(daily_activity, tolower)
clean_names(daily_activity)
sleep_day <- rename_with(sleep_day, tolower)
clean_names(sleep_day)

colnames(daily_activity)
colnames(sleep_day)

combined_data <- read.csv("C:\\Users\\dhani\\OneDrive\\Desktop\\Bellabeat\\combinedData.csv")
combined_data <- rename_with(combined_data, tolower)
clean_names(combined_data)
head(combined_data)
colnames(combined_data)
glimpse(combined_data)
skim_without_charts(combined_data)
str(combined_data)

## Understanding some summary statistics

# How many unique participants are there in each dataframe? 
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

# How many observations are there in each dataframe?
nrow(daily_activity)
nrow(sleep_day)


## Plotting a few explorations 

#calories vs steps
ggplot(data=daily_activity, aes(x=totalsteps, y=calories)) + 
	geom_point() + 
	geom_smooth(method="loess") +
	ggtitle("Calories vs Total Steps") + 
	labs(x= "Total Steps", y = "Calories")

ggsave("stepsvscal.png")

#calories vs sleep
ggplot(data=combined_data, aes(x=totalminutesasleep, y=calories)) + 
	geom_point() + 
	geom_smooth(method="loess") +
	ggtitle("Calories vs Minutes Sleeping") + 
	labs(x= "Total Minutes Sleeping", y = "Calories")

ggsave("sleepvscal.png")

# total minutes of sleep and steps taken.

ggplot(data=combined_data, aes(y=totalsteps, x=totalminutesasleep)) + 
	geom_point() + 
	geom_smooth(method="loess") +
	ggtitle("Total Steps vs Minutes Sleeping") + 
	labs(y= "Total Steps ", x = "Total Minutes Sleeping") +
	ylim(0,25000)

ggsave("sleepvssteps.png")

# sleep vs minutes of activity.

combined_data <- combined_data %>% mutate(totalactiveminutes = veryactiveminutes + fairlyactiveminutes + lightlyactiveminutes + sedentaryminutes)

combined_data_long <- pivot_longer(data=combined_data, 
	cols = c("veryactiveminutes","fairlyactiveminutes", "lightlyactiveminutes", "sedentaryminutes", "totalactiveminutes"), 
	names_to = "activity", 
	values_to = "time")

ggplot(data=combined_data_long, aes(x=totalminutesasleep, y = time, colour = activity)) +
	geom_point() +
	geom_smooth(method="gam", formula = y ~s(x)) + 
	ggtitle("Sleep vs Activity") + 
	labs(x = "Total Minutes Asleep ", y = "Activity Time (Minutes)")

ggsave("sleepvsactivity.png") 

# calories vs minutes of activity.

ggplot(data=combined_data_long, aes(x=calories, y = time, colour = activity)) +
	geom_point() +
	geom_smooth(method="gam", formula = y ~s(x)) + 
	ggtitle("Calories vs Activity") + 
	labs(x = "Calories ", y = "Activity Time (Minutes)")

ggsave("caloriesvsactivity.png") 

