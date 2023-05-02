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

