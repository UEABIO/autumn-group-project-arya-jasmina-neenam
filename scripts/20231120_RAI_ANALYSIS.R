#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(grid)

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



# Load ggplot2
library(ggplot2)

# Create data
data <- data.frame(
  name=c("Non-Hispanic","Hispanic/Latin","Not Specific","N/A") ,  
  value=c(3,12,5,18)
)

# Barplot
ggplot(data, aes(x=Ethnicity, y= Number of observations)) + 
  geom_bar(stat = "identity")





library(ggplot2)

# Sample data (replace this with your actual data)
data <- data.frame(
  Ethnicity = c("Ethnicity1", "Ethnicity2", "Ethnicity3"),
  Asymptomatic = c(10, 15, 8),
  Symptomatic = c(5, 12, 6),
  Unknown = c(8, 10, 4)
)

# Reshape data for plotting
data_long <- tidyr::gather(data, key = "Status", value = "Count", -Ethnicity)

# Create the bar graph using ggplot2
ggplot(data_long, aes(x = Ethnicity, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ethnicity Distribution by Symptomatic Status",
       x = "Ethnicity", y = "Number of Observations") +
  scale_fill_manual(values = c("Asymptomatic" = "blue", "Symptomatic" = "red", "Unknown" = "gray")) +
  theme_minimal()


















covid %>% 
  ggplot(aes(y = case_ethnicity))+
 
theme(panel.border = element_rect(color="black"), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),  
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 20), 
      legend.text = element_text(size = 15), 
      legend.title = element_text(size = 20)) 

ethnicity_vs_symptomatic_status <- ggplot(data = covid,aes(x = symptomatic_status , y =  )) + 
  geom_point(aes(colour = species)) +
  geom_smooth(method="lm", colour = "cornflowerblue", fill = "lightblue", size = 1, ) +
  labs(x = "Length (mm)", y = "Parasite Count") +
  scale_color_manual(name = "Species", labels = c("C. araguaiaensis", "C. maculifer"), values = c("deepskyblue3", "coral")) +
  theme_bw() +
  theme(panel.border = element_rect(color="black"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),  
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20)) 
ggplot(data = covid)
