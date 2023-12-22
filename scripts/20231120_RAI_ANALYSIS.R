#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(grid)
library(ggplot2)


#___________________________----
# IMPORT DATA ----
covid <-read_csv("data/covid_example_data (1).csv")
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

summary(covid) # summaries data

##
covid %>% count(symptomatic_status)
covid %>% count(symptomatic_status, sort = TRUE)
covid %>% count(symptomatic_status, case_ethnicity, sort = TRUE)
# create data
data <- data.frame(
  Ethnicity = c("NON-HISPANIC", "HISPANIC/LATINO"),
  Asymptomatic = c(6032, 848),
  Symptomatic = c(33357,5056),
  Unknown = c(22423,60)
)

data_long <- tidyr::gather(data, key = "Symptomatic Status", value = "Number of Observations", -Ethnicity)

# Create the bar graph using ggplot2
ggplot(data_long, aes(x = Ethnicity, y = Observation , fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
theme(legend.position="none")+
  scale_fill_manual(values = c("Asymptomatic" = "cornflowerblue", "Symptomatic" = "palegreen", "Unknown" = "salmon")) +
  theme_minimal()

ggsave ="Ethnicity_vs_Symptomatic_Status"

