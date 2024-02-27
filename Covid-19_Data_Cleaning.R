### Covid-19 data cleaning and manipulation ###
###          Made by Ai Iwashita            ###


# Loading packages that we will use for this analysis
library(tidyverse)
library(zoo)

# Read the csv file
covid <- read.csv("owid-covid-data.csv")

# Fix the class types 
sapply(covid, class)
covid$iso_code <- as.factor(covid$iso_code)
covid$continent <- as.factor(covid$continent)
covid$location <- as.factor(covid$location)
covid$tests_units <- as.factor(covid$tests_units)
covid$date <- as.Date(covid$date)



## Number of Covid-19 cases ##

covid_cases <- covid[,c(2,3, 4, 5, 6, 63)] 
head(covid_cases)

# As the new_cases and total_cases are only updated once a week, we will remove rows where no reports have been made
date <- seq(from = as.Date("2020-01-05"), to = as.Date("2024-02-04"), by = "day") 
weekly_report_date <- date[seq(from = 1, to = length(date), by = 7)]

covid_cases <- covid_cases[which(covid_cases$date %in% weekly_report_date),]

# We will calculate the proportion of population infected for each location
# And add to the data frame as a new column
covid_cases <- covid_cases %>%
  mutate("prop_population_infected" = new_cases / population)
head(covid_cases)



## Number of Death Attributed to Covid-19 ##

covid_death <- covid[,c(2, 3, 4, 8, 9)]  
head(covid_death)

# We will only keep rows where weekly reports have been recorded
covid_death <- covid_death[which(covid_death$date %in% weekly_report_date),] 



## Covid-19 Vaccination ##

vaccine <- covid[, c(2, 3, 4, 36, 37, 63)]
head(vaccine)

# We will identify the countries with no vaccination reported 
test2 <- vaccine %>%
  group_by(location) %>%
  summarise(total = sum(people_vaccinated, na.rm = TRUE))

no_vaccine_reported_countries <- test2$location[which(test2$total == 0)]
no_vaccine_reported_countries

# We will now remove any rows corresponding to these countries
vaccine <- vaccine[-which(vaccine$location %in% no_vaccine_reported_countries),]

# There are irregular patterns in the reporting dates
# We will replace the NA values with last non-missing values 

# We first need to replace the NA in earliest date for each location with 0
min_date <- vaccine %>%
  group_by(location) %>%
  summarise(min_date = min(date))

n <- length(min_date$location)

for (i in 1:n){
  loc <- min_date$location[i]
  date <- min_date$min_date[i]
  index <- which(vaccine$location == loc & vaccine$date == date)
  vaccine$people_vaccinated[index] <- 0
  vaccine$people_fully_vaccinated[index] <- 0
}

vaccine$people_vaccinated <- na.locf(vaccine$people_vaccinated)
vaccine$people_fully_vaccinated <- na.locf(vaccine$people_fully_vaccinated)

# We will calculate the proportion of population vaccinated for each location
# And add to the data frame as a new column

vaccine <- vaccine %>%
  mutate(prop_vaccinated = people_vaccinated / population) %>%
  mutate(prop_fully_vaccinated = people_fully_vaccinated / population)



### Writing csv ###

write.csv(covid_cases, file = "C:/Users/aimee/OneDrive/デスクトップ/Covid_analysis/covid_cases.csv")
write.csv(covid_death, file = "C:/Users/aimee/OneDrive/デスクトップ/Covid_analysis/covid_death.csv")
write.csv(vaccine, file = "C:/Users/aimee/OneDrive/デスクトップ/Covid_analysis/covid_vaccine.csv")







