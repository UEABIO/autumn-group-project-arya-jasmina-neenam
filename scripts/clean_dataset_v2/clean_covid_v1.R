#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(grid)

#___________________________----
# IMPORT DATA ----
covid <-read_csv("data/covid_example_data (1).csv")
head(covid)
#__________________________----
#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # check the new variable names


#glimpse(covid) # displays column names in console

colnames(covid) # check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

summary(covid) # summaries data