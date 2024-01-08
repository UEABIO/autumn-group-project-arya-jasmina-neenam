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
covid <- read_csv("covid_example_data (1).csv")
head(covid)
ca <- read_csv("owid-covid-data.csv") #ca is short for covid_area
head(ca)


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


#glimpse(covid) # displays column names in console

colnames(covid) # check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

summary(covid) # summaries data

#filter location data which removes nulls and continents
ca$no_of_chars<-nchar(gsub("[^A-Z]","",ca$iso_code))
fca <- filter(ca,ca$no_of_chars < 4) #continents
fca <- filter(ca,ca$no_of_chars > 1) #nulls

filter(ca,ca$new_cases > 3)


#___________________________----
# COVID CASES PER COUNTRY PER MILLION ----

world_coordinates <- map_data("world"
) # makes world map cooridnates

ggplot() + 
  
  geom_map( 
    data = world_coordinates, map = world_coordinates, #plots map using generated co-ordiantes
    aes(long, lat, map_id = region) # asethetic function used to give the map axis titles etc.
  )

fca <- rename(fca, "region" = "location")

fca <- fca %>%
  group_by(region) %>%
  summarise(total_cases_per_million = max(total_cases_per_million,0, na.rm =T))

fca <- filter(fca, fca$total_cases_per_million > 0)

fca <- fca %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Congo" = "Republic of Congo",
                         "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                         "Cote d'Ivoire" = "Ivory Coast",
                         "Czechia" = "Czech Republic"))

hello_world <- left_join(world_coordinates, fca, by = "region")
head(hello_world)

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5), # create plain theme
)
covid_heatmap <- ggplot(data = hello_world,mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = total_cases_per_million)) +
  scale_fill_distiller(palette = "BuPu", direction = 1) + #adds blue-purple coolour gradient
  ggtitle("Covid Cases per million") + plain # gives plot title

#adding in N/A annotation----
na_annotation <- data.frame(x = 160, y = -60, label = "N/A", stringsAsFactors = FALSE)
covid_heatmap <- covid_heatmap +
  annotation_custom(
    grob = grid::textGrob(label = na_annotation$label, just = "left", gp = gpar(fontsize = 6)),
    xmin = max(na_annotation$x) + 7.5, xmax = max(na_annotation$x) + 10,
    ymin = min(na_annotation$y), ymax = max(na_annotation$y + 5)
  ) +
  annotation_custom(grob = rectGrob(gp = gpar(fill = "darkgrey")), #changes colour of annotation box
                    xmin = max(na_annotation$x), xmax = max(na_annotation$x) + 5, 
                    ymin = min(na_annotation$y), ymax = max(na_annotation$y) + 5)# creates N/A annotation for grey areas on plot
p1 <- covid_heatmap

#___________________________----
# DEATH RATES PER COUNTRY PER MILLION ----

world_coordinates <- map_data("world"
) # makes world map cooridnates

ggplot() + 
  
  geom_map( 
    data = world_coordinates, map = world_coordinates, #plots map using generated co-ordiantes
    aes(long, lat, map_id = region) # asethetic function used to give the map axis titles etc.
  )
ca$no_of_chars<-nchar(gsub("[^A-Z]","",ca$iso_code))
fca <- filter(ca,ca$no_of_chars < 4) #continents
fca <- filter(ca,ca$no_of_chars > 1) #nulls

filter(ca,ca$new_cases > 3)
fca <- rename(fca, "region" = "location")

fca <- fca %>%
  group_by(region) %>%
  summarise(total_deaths_per_million = max(total_deaths_per_million,0, na.rm =T))

fca <- filter(fca, fca$total_deaths_per_million > 0)

fca <- fca %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Congo" = "Republic of Congo",
                         "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                         "Cote d'Ivoire" = "Ivory Coast",
                         "Czechia" = "Czech Republic")) #adds in missing regions

hello_world <- left_join(world_coordinates, fca , by = "region")
head(hello_world)

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

covid_deathmap <- ggplot(data = hello_world,mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + #
  geom_polygon(aes(fill = total_deaths_per_million)) +
  scale_fill_distiller(palette = "BuPu", direction = 1) + #created purple to blue colour gradient colour scheme
  ggtitle("Covid Deaths per million") + #gives plot a title
  plain
na_annotation <- data.frame(x = 160, y = -60, label = "N/A", stringsAsFactors = FALSE)
covid_deathmap <- covid_deathmap +
  annotation_custom(
    grob = grid::textGrob(label = na_annotation$label, just = "left", gp = gpar(fontsize = 6)),
    xmin = max(na_annotation$x) + 7.5, xmax = max(na_annotation$x) + 10,
    ymin = min(na_annotation$y), ymax = max(na_annotation$y + 5)
  ) +
  annotation_custom(grob = rectGrob(gp = gpar(fill = "darkgrey")),#changes colour of annotation box
                    xmin = max(na_annotation$x), xmax = max(na_annotation$x) + 5,
                    ymin = min(na_annotation$y), ymax = max(na_annotation$y) + 5)# creates N/A annotation for grey areas on plot

p2 <- covid_deathmap

#geom_map( 
#  data = hello_world, map = world_coordinates, 
#  aes(long, lat, map_id = region) )

#fca <- fca %>%
#  group_by(region) %>%
#  summarise(total_cases = max(total_deaths,0, na.rm =T))

(p1/p2)+
  plot_layout(guides = "collect")