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

        
colors = c("#009E73", "#CC79A7", "#56B4E9")
ethnicity <- c("Hipsanic/Latino", "Unknown", "N/A","Other")
symptomatic_status <- c("Asymptomatic", "Symptomatic", "Unknown")
        
# Matrix for the values
Values <- matrix(c( 5056, 2301,148,33357, 848, 493,568, 6032, 60, 493,62,865),
                         nrow = 3, ncol = 4, byrow = TRUE)
         # Create the bar chart
barplot(Values,horiz = TRUE, main = "The distribution of symptomatic status across ethnicity", names.arg = ethnicity,
xlab = "Ethncity", ylab = "Number of Observations",
col = colors, beside = TRUE)
        
# Add the legend to the chart
legend("topright", symptomatic_status, cex = 0.5, fill = colors)
        
        