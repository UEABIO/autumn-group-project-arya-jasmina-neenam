
# PACKAGES ----
library(tidyverse)
library(janitor) 
library(lubridate) 
library(scales)
library(patchwork)
library(dplyr)

#__________________________----
# IMPORT DATA ----
covid <- read_csv ("data/owid-covid-data.csv")
head(covid)

#__________________________----
# CHECK DATA----
# check the data
colnames(covid)
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

covid <- janitor::clean_names(covid) 
colnames(covid) # quickly check the new variable names
glimpse(covid) # displays column names in console
colnames(covid)

# check for duplicates---
duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                
covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()
summary(covid) # summaries data
filter(.data = covid, continent %in% c("north america", "south america"))

