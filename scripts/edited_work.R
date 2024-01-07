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


symptomatic_and_case_race <- select(.data = covid, 
                           case_race, symptomatic_status) # Creating a data set with all the columns I am interested in

symptomatic_and_case_race_omit  <- na.omit(symptomatic_and_case_race) # getting rid of missing variables in the data set 


symptomatic_and_case_race_omit_no_unknown   <- filter(symptomatic_and_case_race_omit , 
                                              case_race != "UNKNOWN")

##
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status)
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status, sort = TRUE)
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status, case_race, sort = TRUE)

colors = c("#009E73", "#CC79A7", "#56B4E9")
ethnicity <- c("Black", "White", "Asian", "American indian/Alaska native", "Native Hawaiian/Pacific islander", "Other")
symptomatic_status <- c("Asymptomatic", "Symptomatic", "Unknown")



# Matrix for the values
Values <- matrix(c( 3721, 2693, 281, 5, 3, 474, 18143, 17215, 1737, 50, 41, 2802, 545, 355, 33, 5, 3, 53),

                                  nrow = 3, ncol = 6, byrow = TRUE)


#
# Create the bar chart
barplot(Values,horiz = TRUE, main = "The distribution of symptomatic status across race", names.arg = ethnicity,
        xlab = "Race", ylab = "Number of Observations",
        col = colors, beside = TRUE)
plot(xlab, ylab, xlim = c(0, 20000))

# Add the legend to the chart
legend("topright", symptomatic_status, cex = 0.5, fill = colors)




