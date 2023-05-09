#Set work directory
setwd("C:/Users/HP/OneDrive/Desktop/MA304-Data Visualisation/Completed")


#Read the csv file
proj_dallas <- read.csv("37-00049_UOF-P_2016_prepped.csv")


#Install requried libraries
library(dplyr)
library(leaflet)
library(viridis)
library(parsedate)
library(tidyr)
library(stringr)
library(ggplot2)
library(kableExtra)
library(tidygeocoder)
library(plotly)


#Clean the data by: 
#Correcting the date format 
proj_dallas <- proj_dallas %>% mutate(INCIDENT_DATE = parse_date(INCIDENT_DATE),
                                      OFFICER_HIRE_DATE = parse_date(OFFICER_HIRE_DATE))


#Separating rows having multiple values
proj_dallas <- proj_dallas %>% separate_rows(UOF_NUMBER, FORCE_EFFECTIVE, sep = ",")


#Trimming the space from left and right of the values
proj_dallas <- proj_dallas %>% mutate(across(where(is.character),str_trim))


#Convert char to factor
proj_dallas <- proj_dallas %>%
  mutate_at(c('OFFICER_GENDER','OFFICER_RACE','OFFICER_INJURY','OFFICER_INJURY_TYPE',
              'OFFICER_HOSPITALIZATION','SUBJECT_RACE','SUBJECT_GENDER','SUBJECT_INJURY',
              'SUBJECT_INJURY_TYPE','SUBJECT_WAS_ARRESTED','SUBJECT_DESCRIPTION','SUBJECT_OFFENSE',
              'DIVISION','INCIDENT_REASON','REASON_FOR_FORCE','FORCE_EFFECTIVE'),as.factor)


#Reducing the columns for 'type_of_forced' used by rows
proj_dallas <- proj_dallas %>% pivot_longer(c("TYPE_OF_FORCE_USED1","TYPE_OF_FORCE_USED2","TYPE_OF_FORCE_USED3",
                                              "TYPE_OF_FORCE_USED4","TYPE_OF_FORCE_USED5","TYPE_OF_FORCE_USED6",
                                              "TYPE_OF_FORCE_USED7","TYPE_OF_FORCE_USED8","TYPE_OF_FORCE_USED9",
                                              "TYPE_OF_FORCE_USED10"), names_to = "TYPE_OF_FORCE",
                                            values_to = "ACTIONS_TAKEN")


#Filter Out empty rows
proj_dallas <- proj_dallas %>%
  filter(ACTIONS_TAKEN != "")


#Remove unwanted column
proj_dallas <- proj_dallas[,-c(25:27,30,31,36), drop = FALSE]

#Displaying Arrest of subjects in Dallas
location_data <- proj_dallas %>%
  filter(!is.na(LOCATION_LATITUDE), SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(SUBJECT_ID, LOCATION_LATITUDE, LOCATION_LONGITUDE, LOCATION_DISTRICT) %>%
  summarise(count = n()) %>%
  filter(SUBJECT_ID != 0, LOCATION_DISTRICT != "NULL")

my_loc_palette <- colorFactor(palette = viridis(7),
                              domain = location_data$LOCATION_DISTRICT,
                              na.color = "transparent")

leaflet(location_data) %>%
  setView(lng = -96.808891, lat = 32.779167, zoom = 10) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircleMarkers(
    lng = ~LOCATION_LONGITUDE,
    lat = ~LOCATION_LATITUDE,
    fillColor = ~my_loc_palette(location_data$LOCATION_DISTRICT),
    fillOpacity = 0.8,
    color="white",
    radius=8,
    stroke=F,
    label=~paste("Arrested: ", count),
    popup=~paste("Arrested: ", count, "\nLOCATION_DISTRICT: ", LOCATION_DISTRICT)
  ) %>%
  addLegend(pal=my_loc_palette, values = ~LOCATION_DISTRICT, title = "Regions", 
            position = "bottomleft",
            opacity = 1)

  
#Count of highest Arrest by Divison
proj_dallas %>% select(SUBJECT_WAS_ARRESTED, DIVISION) %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(DIVISION) %>% count() %>%
  ggplot(aes(x = DIVISION, y = n)) +
  geom_histogram(stat = "identity", fill = "violet")

#which month has highest robbery
proj_dallas %>% select(INCIDENT_DATE, SUBJECT_WAS_ARRESTED) %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  mutate(month = format(INCIDENT_DATE, "%m")) %>%
  group_by(month) %>%
  summarise(count_arrest = n()) %>%
  ggplot(aes(x = month, y=count_arrest)) +
  geom_col(fill = "orange")

#count of officers injuried
officers_injuried <- proj_dallas %>% select(OFFICER_GENDER, OFFICER_HOSPITALIZATION) %>%
  group_by(OFFICER_GENDER, OFFICER_HOSPITALIZATION) %>% count() %>%
  pivot_wider(names_from = OFFICER_HOSPITALIZATION, values_from = n)
officers_injuried

#minor and major injuries
proj_dallas %>% select(OFFICER_GENDER, OFFICER_INJURY_TYPE,OFFICER_HOSPITALIZATION) %>%
  filter(OFFICER_INJURY_TYPE != "No injuries noted or visible" &
           OFFICER_HOSPITALIZATION == "Yes") %>%
  group_by(OFFICER_GENDER) %>% count() %>%
  ggplot(aes(x = "", y = n, fill = OFFICER_GENDER)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste(round((n/sum(n))*100, 2), "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0)+
  theme_void()

#how many and which year who has joined more
proj_dallas %>%
  mutate(year = format(OFFICER_HIRE_DATE, "%Y")) %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(x = year, y = n, group = "red")) +
  geom_line(aes(color = "red")) +
  geom_point(aes(color = "red")) +
  theme_bw() +
  theme(legend.position = "none")

#people of different race joining force
proj_dallas %>% select(OFFICER_ID, OFFICER_RACE, OFFICER_HIRE_DATE) %>%
  mutate(year = format(OFFICER_HIRE_DATE, "%Y")) %>%
  group_by(year, OFFICER_RACE) %>%
  count() %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "turquoise4") +
  coord_flip() +
  facet_wrap(vars(OFFICER_RACE), scales = "free") +
  theme_classic()

#officers years spent on force
proj_dallas %>% select(OFFICER_ID,OFFICER_YEARS_ON_FORCE) %>%
  group_by(OFFICER_YEARS_ON_FORCE) %>% count() %>%
  ggplot(aes(x = OFFICER_YEARS_ON_FORCE, y=n)) +
  geom_density(stat = "identity", fill = "green")

#boxplot
proj_dallas %>% group_by(OFFICER_RACE, OFFICER_YEARS_ON_FORCE) %>%
  ggplot(aes(x = OFFICER_RACE, y = OFFICER_YEARS_ON_FORCE, fill = OFFICER_RACE)) +
  geom_boxplot(show.legend = FALSE)

#correlation
df1 <- proj_dallas %>%
  group_by(OFFICER_ID, TYPE_OF_FORCE) %>%
  summarise(count = n()) %>%
  select(c("OFFICER_ID", "count")) %>%
  filter(OFFICER_ID != 0)

df1 <- df1[!duplicated(df1), ]

df1 <- df1 %>%
  group_by(OFFICER_ID) %>%
  summarise(total = sum(count)) %>%
  arrange(OFFICER_ID) %>%
  select(total)

corr_force_officer <- cbind(df1,
                            proj_dallas %>%
                              select(c(OFFICER_ID, OFFICER_YEARS_ON_FORCE)) %>%
                              group_by(OFFICER_ID) %>%
                              slice(1) %>%
                              ungroup %>%
                              filter(OFFICER_ID != 0)
)

cor_graph <- corr_force_officer %>% ggplot(aes(x = total, y = OFFICER_YEARS_ON_FORCE)) +
  geom_point(color = "pink") +
  geom_smooth()

ggplotly(cor_graph)
