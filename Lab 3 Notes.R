library(ggplot2)
library(dplyr)
library(tidyverse)

av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)

#New data with five columns are replaced by two columns
av_deaths <- av %>%
  gather(key = "Time", value = "Death", starts_with("Death")) %>%
  mutate(Time = as.integer(gsub("Death", "", Time)), 
         Death = case_when(
           Death == "YES" ~ "yes",
           Death == "NO" ~ "no",
           TRUE ~ "" #Assuming missing data as empty string
         ))

#Calculate the average deaths per avenger
deaths_per_avenger <- av_deaths %>%
  filter(Death == "yes") %>%
  group_by(Name.Alias) %>% #Filter by name
  summarize(Deaths = n()) 

average_deaths <- mean(deaths_per_avenger$Deaths)
average_deaths

#Question to answer -
#Given the Avengers’ 53 years in operation and overall mortality rate, 
#fans of the comics can expect one current or former member to die every seven months or so, 
#with a permanent death occurring once every 20 months.

#Calculate the total number of deaths 
total_deaths <- sum(av$Death1 == "YES" | av$Death2 == "YES" | av$Death3 == "YES" | av$Death4 == "YES" | av$Death5 == "YES")
#total_deaths

#Calculate the total number of returns
total_returns <- sum(av$Return1 == "YES" |av$Return2 == "YES" |av$Return3 == "YES" |av$Return4 == "YES" |av$Return5 == "YES")
#total_returns

#Calculate the total number of permanent deaths
permanent_deaths <-total_deaths - total_returns
permanent_deaths

#Convert some numbers with hard coding
months_of_avengers <- 53 * 12

#How often an avenger dies calculation
average_time_of_death <- months_of_avengers/permanent_deaths
average_time_of_death


