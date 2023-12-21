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
covid <- read_csv("covid_example_data (1)(1).csv")
head(covid)


#__________________________----
#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # check the new variable names

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
                "case_ethnicity" = "case_eth") #re-names columns to easily understandable text

glimpse(covid) # displays column names in console

colnames(covid) # check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements   

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

covid %>% 
  summarise(
    mean_died_covid = mean(died_covid, na.rm=TRUE),
    mean_case_age = mean(case_age, na.rm=TRUE))

covid <- covid[!(is.na(covid$case_age)),] # omits NA from case_age data

covid

covid <- covid[!(is.na(covid$died_covid)),] #omits NA from died_covid data
covid <- covid[!(is.na(covid$case_gender)),]#omits NA from case_gender data
#___________________________----
#comparative graph of age and date of death to show death rate



covid %>%
  ggplot(aes(x = died_covid, y = case_age)) +
  geom_boxplot(aes(fill = died_covid),
               alpha = 0.2,
               width = 0.5,
               outlier.shape = NA)+
  geom_jitter(aes(colour = died_covid),
              width=0.2)+
  theme(legend.position = "none")
ggtitle("ages of patient death from COVID")

covid %>%
  ggplot(aes(x = case_gender, y = case_age)) +
  geom_boxplot(aes(fill = case_gender),
               alpha = 0.2,
               width = 0.5,
               outlier.shape = NA)+
  geom_jitter(aes(colour = case_gender),
              width=0.2)+
  theme(legend.position = "none")
ggtitle("genders of infected patients")

