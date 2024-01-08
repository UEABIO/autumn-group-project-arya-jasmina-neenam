library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)

#___________________________----
# IMPORT DATA ----
covid <- read_csv("data/covid_example_data (1)(1).csv")
head(covid)


#__________________________----
#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

symptoms_and_ages <- select(.data = covid, 
                            case_age, case_race, sym_fever, sym_myalgia, sym_losstastesmell, sym_sorethroat, sym_cough, sym_headache) # Creating a data set with all the columns i am interested in

symptpms_and_ages_omit  <- na.omit(symptoms_and_ages) # getting rid of missing variables in the data set 

symptpms_and_ages_omit_no_unknowns  <- filter(symptpms_and_ages_omit, 
                                              sym_fever != "Unk", 
                                              sym_myalgia != "Unk",
                                              sym_losstastesmell != "Unk",
                                              sym_sorethroat != "Unk",
                                              sym_cough != "Unk",
                                              sym_headache != "Unk") # getting rid of the rows containing unknown variables in the data set 



#COMPARATIVE GRAPHS OF AGE AND PREVELANCE OF CERTAIN SYMPTOMS----


#Graph for age and the presence of fever
pal <- c("tomato2","cornflowerblue")

p1 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_fever,
             y = case_age,
             fill = sym_fever,
             colour = sym_fever))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+ 
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of fever",
        y = "Patients age",
        title = "Age of patients with symptoms of fever",
        subtitle = "Box and violin plot of the presence of fever by patients age")



#Graph for age and the presence of myalgia
p2 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_myalgia,
             y = case_age,
             fill = sym_myalgia,
             colour = sym_myalgia))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of myalgia",
        y = "Patients age",
        title = "Age of patients with symptoms of myalgia",
        subtitle = "Box and violin plot of the presence of myalgia by patients age")



#Graph for age and the presence of the loss of taste and smell
p3 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_losstastesmell,
             y = case_age,
             fill = sym_losstastesmell,
             colour = sym_losstastesmell))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of the loss of taste and smell",
        y = "Patients age",
        title = "Age of patients with symptoms of loss of taste and smell",
        subtitle = "Box and violin plot of the presence of the loss of taste and smell by patients age")


#Graph for age and the presence of a sore throat
p4 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_sorethroat,
             y = case_age,
             fill = sym_sorethroat,
             colour = sym_sorethroat))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of a sore throat",
        y = "Patients age",
        title = "Age of patients with symptoms of sore throat",
        subtitle = "Box and violin plot of the presence of a sore throat by patients age")



#Graph for age and the presence of cough
p5 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_cough,
             y = case_age,
             fill = sym_cough,
             colour = sym_cough))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of coughing",
        y = "Patients age",
        title = "Age of patients with symptoms of coughing",
        subtitle = "Box and violin plot of the presence of coughing by patients age")



#Graph for age and the presence of headache
p6 <- symptpms_and_ages_omit_no_unknowns %>% 
  ggplot(aes(x = sym_headache,
             y = case_age,
             fill = sym_headache,
             colour = sym_headache))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  coord_flip()+  
  theme_classic()+
  theme(legend.position = "none")+
  labs( x = "The presence of headache",
        y = "Patients age",
        title = "Age of patients with headache symptoms",
        subtitle = "Box and violin plot of the presence of headache by patients age")



# merging all the plots together
(p1+p2+p3+p4+p5+p6)+
  plot_layout(guides = "collect")

