library(lubridate)

covid <- read_csv("data/covid_example_data (1).csv")
head(covid)

covid$new_date_of_first_symptoms <- mdy(covid$date_of_first_symptoms)

covid$new_date_of_entry_for_case <- mdy(covid$date_of_entry_for_case)
covid$new_date_of_birth <- mdy(covid$date_of_birth)
covid$new_date_of_hospital_discharge <- mdy(covid$date_of_hospital_discharge)

head(covid)
colnames(covid)


covid %>% 
  ggplot(aes(x= sym_fever, 
             y = case_age))+
  geom_boxplot(aes(colour=case_race))


options(scipen = 999)

covid %>% 
  ggplot(aes(y = case_age, 
             x = new_date_of_entry_for_case))+
  geom_point(aes(colour=case_race))+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=case_race))+ 
  coord_flip()

covid %>% 
  ggplot(aes(y = case_age, 
             x = new_date_of_first_symptoms))+
  geom_line()
  


  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=case_race))+ 
  coord_flip()

covid %>% 
  ggplot(aes(y= case_age, 
             x = new_date_of_first_symptoms))+
  geom_line(aes(colour=case_race))


  #geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=case_race))+
  scale_x_date(date_labels = "%w"
               ,date_breaks = "1 month")
  coord_flip()

  covid %>% 
    ggplot(aes(x= case_race, 
               y = case_age)) +
    geom_col()
  
    geom_smooth(method="lm",    #add another layer of data representation.
                se=FALSE,
                aes(colour=continent))+ 
    ylim(0,800000)+
    coord_flip()+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Continents", 
         y = "Total vaccinations (per hundred)",
         title= "Total recorded vaccinations in each continent")



  labs(x = "Continents", 
       y = "Total vaccinations (per hundred)",
       title= "Total recorded vaccinations in each continent")

p2 <- covid %>% 
  ggplot(aes(x= continent, 
             y = total_deaths_per_million))+
  geom_col(aes(colour=continent))+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=FALSE,
              aes(colour=continent))+ 
  coord_flip()+
  scale_y_continuous(labels = scales::labels)+
  labs(x = "Continents", 
       y = "Total deaths (per million)",
       title= "Total recorded deaths in each continent")

(p1+p2)+
  plot_layout(guides = "collect")





covid %>% 
  ggplot(aes(x= new_date_of_entry_for_case, 
             y = case_age))+
  geom_boxplot(aes(colour=case_race))

