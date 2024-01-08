library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(grid)


#___________________________----
# IMPORT DATA ----
covid <- read_csv("data/covid_example_data (1)(1).csv")
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

covid$new_date_of_first_symptoms <- mdy(covid$date_of_first_symptoms)

covid$new_date_of_entry_for_case <- mdy(covid$date_of_entry_for_case)
covid$new_date_of_birth <- mdy(covid$date_of_birth)
covid$new_date_of_hospital_discharge <- mdy(covid$date_of_hospital_discharge)


new_covid <- select(.data = covid, 
                       new_date_of_entry_for_case, case_age, case_race, sym_fever)

symptoms_and_ages <- select(.data = covid, 
                    case_age, case_race, sym_fever, sym_myalgia, sym_losstastesmell)

symptpms_and_ages_omit  <- na.omit(symptoms_and_ages)

ggplot(data = symptpms_and_ages_omit, aes(x = sym_fever, y = case_age)) +
  geom_boxplot(
               alpha = 0.7, 
               width = 0.5,# change width of boxplot
               outlier.shape = NA)



complete.cases(new_covid)
which(complete.cases(new_covid))

new_covid_omit <- na.omit(new_covid)

head(new_covid_omit)


ggplot(data = new_covid_omit, aes(x = case_race, y = case_age)) +
  geom_boxplot(aes(fill = case_race),
               alpha = 0.7, 
               width = 0.5,# change width of boxplot
               outlier.shape = NA)
              
ggplot(data = new_covid_omit, aes(x = case_race, y = new_date_of_entry_for_case)) +
  geom_boxplot(aes(fill = case_race),
               alpha = 0.7, 
               width = 0.5,# change width of boxplot
               outlier.shape = NA)

  


  ggplot(data = new_covid_omit, aes(x = case_race,
             y = new_date_of_entry_for_case,
             fill = case_race,
             colour = case_race))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)
  
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "",
    y = "Body mass (g)",
    title = "Body mass of brush-tailed penguins",
    subtitle = "Box and violin plot of body mass by species")




# note fill is "inside" colour and colour is "edges" - try it for yourself
alpha = 0.2, # fainter boxes so the points "pop"
width = 0.5, # change width of boxplot
outlier.shape=NA)+
  geom_jitter(aes(colour = species),
              width=0.2)+
  theme(legend.position = "none")
