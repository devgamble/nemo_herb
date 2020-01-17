#
# Data Cleaning for the (almost) raw CCH2 occurrence dataset
#

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)

###
# Tasks
###

# NOTE: The csv I've read in here was trimmed a bit (irrelevant columns removed) but this workflow should work just as well with a csv downloaded straight from CCH2.

# Clean & Standardize Dates

nemo_cch2 <- read_csv(here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_Dec2019.csv")) %>% 
  filter(!day == 0) %>% #remove obs where day = 0 or is blank
  mutate(date_new = make_date(year = year, month = month, day = day)) %>%  #Combine YYYY MM DD
  select(- c(coll_number, institutionCode, recordId, references)) %>% 
  mutate(sub_sp = case_when(taxonID == 205100 | taxonID == 205103 ~ "menziesii",
                            taxonID == 205101 ~ "atomaria",
                            taxonID == 205102 | taxonID == 210481 |taxonID == 218999 | scientificName %in% c("Nemophila menziesii var. integrefolia", "Nemophila menziesii subsp. australis") ~ "integrifolia")) %>% 
  filter(!scientificName %in% c("Nemophila rotata", "Nemophila venosa")) #not menzies

#1475 obs

#More cleaning


nemo_2 <- nemo_cch2 %>% 
  mutate(DOY = yday(date_new)) %>% #accounts for leap years!
  #mutate(testdoy = DOY - startDayOfYear) #Suggests included startDayOfYear is perfectly accurate!
  select(-c(scientificName, taxonID, verbatimEventDate)) #%>% filter(!is.na(decimalLatitude)) 
  #IF remove empty lat/long: loss of 359 obs
# Retain blank lat/long to estimate coords at later time #Option to subset (remove) later


#Fixing Elevation - Option to fix by hand:
# Several (~10) verbatim dates with [ft] where minelev_m was empty were converted manually
# `minimumElevation` used for ranges and when only one measure of elevation present

nemo_2 <- nemo_2 %>% 
  rename(elev_m = minimumElevationInMeters) %>% 
  select(-verbatimElevation) %>% 
  filter(!DOY == 1) #Remove obs where MM and DD = 01 (none of these are accurate)
  

#Check for duplicates

#which(duplicated(nemo_2[,c(12,17)]))

#Can't seem to find any. Below dplyr::distinct doesn't seem correct. Will wait til after combining CCH1 and CCH2

#nemo_2 <- nemo_2 %>% distinct(date_new, decimalLatitude, decimalLongitude, .keep_all = TRUE) # Removes ~ 100 observations


#
# Final cleaning, organizing, and export

nemo_cch2_clean <- nemo_2 %>% 
  select(-c(eventDate, startDayOfYear)) %>% 
  rename(repro = reproductiveCondition, lat = decimalLatitude, long = decimalLongitude, error_dist_m = coordinateUncertaintyInMeters) %>% #Get col names to match CCH1
  select(specimen_number, id, date_new, year, month, day, DOY, lat, long, elev_m, error_dist_m, sub_sp, everything()) #Rearrange df with desired columns in order

write_csv(nemo_cch2_clean, here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned.csv"))

