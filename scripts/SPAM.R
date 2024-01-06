library(lubridate)

covid <- read_csv("data/covid_example_data (1)(1).csv")
head(covid)

covid <- rename(covid,
                "date_of_entry_for_case"="reprt_creationdt_FALSE",
                "date_of_birth"="case_dob_FALSE",
                "symptomatic_status"="contact_id" ,
                "date_of_first_symptoms"="sym_startdt_FALSE",
                "symptoms_now_resolved"="sym_resolveddt_FALSE",
                "date_of_hospital_admission"="hosp_admidt_FALSE",
                "date_of_hospital_discharge"="hosp_dischdt_FALSE",
                "date_of_death"= "died_dt_FALSE",
                "date of positive PCR test" = "pos_sampledt_FALSE",
                "case_ethnicity" = "case_eth") #re-names columns to easily understandable text
colnames(covid)

covid$new_date_of_first_symptoms <- mdy(covid$sym_startdt_FALSE)

covid$new_date_of_entry_for_case <- mdy(covid$date_of_entry_for_case)
covid$new_date_of_birth <- mdy(covid$date_of_birth)
covid$new_date_of_hospital_discharge <- mdy(covid$date_of_hospital_discharge)

head(covid)
colnames(covid)

covid %>% 
  ggplot(aes(x= new_date_of_first_symptoms)+
  geom_histogram()
            
  