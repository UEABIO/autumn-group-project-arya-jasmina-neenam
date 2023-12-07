#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)


# IMPORT DATA ----
covid <- read_csv("data/owid-covid-data.csv")
head(covid)

#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # quickly check the new variable names

covid <- rename(covid,
                   "AFR"="OWID_AFR")

covid <- covid %>% 
  mutate(species = case_when("reprt_creationdt_FALSE" ~ "date_of_entry_for_case",
                             "case_dob_FALSE" ~ "date_of_birth",
                             "Contact_id" ~ "symptomatic_status",
                             "sym_startdt_FALSE" ~ "date_of_first_symptoms",
                             "sym_resolveddt_FALSE" ~ "symptoms_now_resolved",
                             "hosp_admidt_FALSE" ~ "date_of_hospital_admission",
                             "hosp_dischdt_FALSE" ~ "	date_of_hospital_discharge",
                             "hdied_dt_FALSE" ~ "	date_of_death",
                             "pos_sampledt_FALSE" ~ "	date of positive PCR test",
                             "case_eth" ~ "case_ethnicity"))
                  
glimpse(covid)# displays column names in console

# check for duplicate rows in the data
covid %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all the TRUE statements                                                                                                                                

summary(covid) # summaries data

