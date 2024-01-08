#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(grid)
library(patchwork)

#___________________________----
# IMPORT DATA ----
covid <- read_csv("covid_example_data (1)(1).csv")
head(covid)


#__________________________----
#CLEAN DATA----
covid <- janitor::clean_names(covid) # cleans the column names

colnames(covid) # checks the new variable names

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

covid <- covid[!(is.na(covid$case_age)),] # omits NA from case_age data

covid # shows snap shot of data in console

covid <- covid[!(is.na(covid$died_covid)),] #omits NA from died_covid data
covid <- covid[!(is.na(covid$case_gender)),]#omits NA from case_gender data
#___________________________----
#COMPARATIVE GRAPH OF AGE AND GENDER OF PATIENTS STUDIED----


pal <- c("tomato2","palegreen3", "lightgrey")

p1 <- covid %>% 
  ggplot(aes(x = case_gender, # gender displayed on X-axis
             y = case_age, # age displayed on Y-Axis
             fill = case_gender,
             colour = case_gender))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+ # adds scale
  scale_fill_manual(values = pal)+ 
  scale_colour_manual(values = pal)+
  theme_light()+ # adds in 'light' theme for cohesion between plots
  theme(legend.position = "none")+
  labs(
    x = "pateint gender", #x-axis title
    y = "patient age", #y-axis title
    title = "Age verses gender of patients studied", # graph title
    subtitle = "Box and violin plot of age by gender")# graph sub-title


#___________________________----
#COMPARATIVE GRAPH OF AGE AND DATETO SHOW DEATH RATE----

pal <- c("orange", "lightgrey", "cornflowerblue")

p2 <- covid %>% 
  ggplot(aes(x = died_covid,# weather they died or not displayed on x-axis
             y = case_age, # age of patient recorded
             fill = died_covid,
             colour = died_covid))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+ # adds scale
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  theme_light()+ # adds in 'light' theme for cohesion between plots
  theme(legend.position = "none")+
  labs(
    x = "death of patient",
    y = "patient age",
    title = "Age of patient death from covid",
    subtitle = "Box and violin plot of age by death rate") 

#__________________________----
#COMBINING PLOTS INTO ONE FIGURE----

(p1+p2)+
plot_layout(guides = "collect")