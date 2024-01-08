#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)


# IMPORT DATA ----
covid <- read_csv("covid_example_data (1).csv")
head(covid)
ca <- read_csv("owid-covid-data.csv") #ca is short for covid_area
head(ca)



#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # quickly check the new variable names

#covid <- rename(covid,
#                   "AFR"="OWID_AFR") #look at this later

covid <- rename(covid,
                "date_of_entry_for_case"="reprt_creationdt_false",
                "date_of_birth"="case_dob_false",
                "symptomatic_status"="contact_id" ,
                "date_of_first_symptoms"="sym_startdt_false",
                "symptoms_now_resolved"="sym_resolveddt_false",
                "date_of_hospital_admission"="hosp_admidt_false",
                "date_of_hospital_discharge"="hosp_dischdt_false",
                "date_of_death"= "died_dt_false",
                "date of positive PCR test" = "pos_sampledt_false",
                "case_ethnicity" = "case_eth")


#glimpse(covid) # displays column names in console

colnames(covid)
# check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

summary(covid) # summaries data

#filter location data
ca$no_of_chars<-nchar(gsub("[^A-Z]","",ca$iso_code))
fca <- filter(ca,ca$no_of_chars < 4)

filter(ca,ca$new_cases > 3)


#___________________________----
# WORLD DEATH RATES by country----

# create data for world coordinates using  
# map_data() function 
world_coordinates <- map_data("world"
) 

# create world map using ggplot() function 
ggplot() + 
  
  # geom_map() function takes world coordinates  
  # as input to plot world map 
  geom_map( 
    data = world_coordinates, map = world_coordinates, 
    aes(long, lat, map_id = region) 
  )

countries <- c('Afghanistan','Albania','Algeria','Andorra','Angola','Anguilla','Antigua and Barbuda','Argentina')