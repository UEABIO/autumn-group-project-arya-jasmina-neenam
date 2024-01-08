#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(ggplot2)


#__________________________----
# ----IMPORT DATA ----
covid <- read_csv("covid_example_data (1)(1).csv")
head(covid)
#__________________________----
# ----CLEAN DATA----
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
                "case_ethnicity" = "case_eth") 
glimpse(covid) 
colnames(covid) 
duplicated(covid)
sum()                                                                                                                        
covid %>% 
  is.na() %>% 
  sum()

summary(covid) 

#__________________________----
# ----SELECT DATA SETS REQUIRED----
symptomatic_and_case_race <- select(.data = covid, 
                                    case_race, symptomatic_status) 
#__________________________----
# ----REMOVE MISSING VARIABLES----
symptomatic_and_case_race_omit  <- na.omit(symptomatic_and_case_race) 

#__________________________----
# ----REMOVE UNKNOWN VALUES----
symptomatic_and_case_race_omit_no_unknown   <- filter(symptomatic_and_case_race_omit , 
                                                      case_race != "UNKNOWN")

#__________________________----
# ----CALCULATE OBSERVATION COUNT ACROSS SYMPTOMATIC STATUS----
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status)
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status, sort = TRUE)
symptomatic_and_case_race_omit_no_unknown %>% count(symptomatic_status, case_race, sort = TRUE)

#__________________________----
# ----ASSIGN THE COLOURS FOR THE KEY, LABEL THE KEY AND RACES----
colors = c("#009E73", "#CC79A7", "#56B4E9")

#__________________________----
# ----LABEL THE KEY AND RACES----
race <- c("Black", "White", "Asian", "Native American/Alaska native", "Pacific islander", "Other")
symptomatic_status <- c("Asymptomatic", "Symptomatic", "Unknown")

#__________________________----
# ----CREATE MATRIX----
Values <- matrix(c( 3721, 2693, 281, 5, 3, 474, 18143, 17215, 1737, 50, 41, 2802, 545, 355, 33, 5, 3, 53),
                 nrow = 3, ncol = 6, byrow = TRUE)

#__________________________----
# ----CREATE BAR CHART----
barplot(Values, main = "The distribution of symptomatic status across race", names.arg = ethnicity,
        xlab = "Race", ylab = "Number of Observations",
        #set limits and arrange font sizes
        ylim=c(0,20000),
        cex.name=0.50,
        col = colors, beside = TRUE)

#__________________________----
# ----ADD LEGEND FOR THE KEY----
legend("topright", symptomatic_status, cex = 1, fill = colors)

