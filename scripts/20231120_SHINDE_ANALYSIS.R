
# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly

#__________________________----
# IMPORT DATA ----
covid <- read_csv ("data/owid-covid-data.csv")
head(covid) # check the data has loaded, prints first 10 rows of dataframe


#__________________________----
# CHECK DATA----
# check the data
colnames(covid)


covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # quickly check the new variable names

#covid <- rename(covid,
#                   "AFR"="OWID_AFR") #look at this later

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


#glimpse(covid) # displays column names in console

colnames(covid)
# check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

summary(covid) # summaries data

filter(.data = covid, continent %in% c("north america", "south america"))

# making graph

covid %>% 
  ggplot(aes(x=people_fully_vaccinated_per_hundred, 
             y = new_deaths_per_million))+
  geom_point(aes(colour=continent))+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=continent)) # note layers inherit information from the top ggplot() function but not previous layers - if we want separate lines per species we need to either specify this again *or* move the color aesthetic to the top layer. 


covid %>% 
  ggplot(aes(x=population_density, 
             y = new_deaths_per_million))+
  geom_point(aes(colour=continent))+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=continent)) # note layers inherit information from the top ggplot() function but not previous layers - if we want separate lines per species we need to either specify this again *or* move the color aesthetic to the top layer. 


covid %>% 
  ggplot(aes(x= continent, 
             y = total_deaths_per_million))+
  geom_col(aes(colour=continent))+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=continent))+ # note layers inherit information from the top ggplot() function but not previous layers - if we want separate lines per species we need to either specify this again *or* move the color aesthetic to the top layer. labs(x = "Flipper length (mm)",
  labs(x = "Continents", 
       y = "Total deaths (per million)")


ggsave('filename.png', width= , height=??)



