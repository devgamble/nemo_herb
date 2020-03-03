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

# NOTE 2: I chose to create an updated cleaned csv in this script so I don't have to pull from the original occurrence data again. Here I remove duplicates and add back two columns. the first csv `nemo_cch1_cleaned.csv` should functionally be the same

# Clean & Standardize Dates

nemo_cch2 <- read_csv(here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_Dec2019.csv")) %>% 
  filter(!day == 0) %>% #remove obs where day = 0 or is blank
  mutate(date_new = make_date(year = year, month = month, day = day)) %>%  #Combine YYYY MM DD
  select(- c(institutionCode, recordId)) %>% 
  mutate(sub_sp = case_when(taxonID == 205100 | taxonID == 205103 ~ "menziesii",
                            taxonID == 205101 ~ "atomaria",
                            taxonID == 205102 | taxonID == 210481 |taxonID == 218999 | scientificName %in% c("Nemophila menziesii var. integrefolia", "Nemophila menziesii var. incana", "Nemophila menziesii subsp. australis") ~ "integrifolia")) %>% 
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
  rename(elev_m = minimumElevationInMeters,
         datum = geodeticDatum) %>% 
  select(-verbatimElevation) %>% 
  filter(!DOY == 1) #Remove obs where MM and DD = 01 (none of these are accurate)
  
#Check for missing specimen_number:
which(is.na(nemo_2$specimen_number))

#Assign secondary catalog number (`coll_number`) to specimen_number when it is missing
nemo_2 <- nemo_2 %>% 
  mutate(specimen_number = ifelse(is.na(specimen_number),paste("COLL-", coll_number, sep = ""), specimen_number)) #Specify change with "COLL-" at beginning of new specimen number (only two cases)
#Updated: Cannot use `*` because of error in naming images




#
# Final cleaning, organizing, and export

nemo_cch2_clean <- nemo_2 %>% 
  select(-c(coll_number, eventDate, startDayOfYear, references, datum)) %>% 
  rename(repro = reproductiveCondition, lat = decimalLatitude, long = decimalLongitude, error_dist_m = coordinateUncertaintyInMeters) %>% #Get col names to match CCH1
  select(specimen_number, id, date_new, year, month, day, DOY, lat, long, elev_m, error_dist_m, sub_sp, everything()) #Rearrange df with desired columns in order

write_csv(nemo_cch2_clean, here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned.csv"))




#
####
# UPDATED CLEANED CCH2 DATA
####
#



#Adding back the geodetic datum and URLs

nemo_2.5 <- nemo_2 %>% 
  select(-c(coll_number, eventDate, startDayOfYear)) %>% 
  rename(repro = reproductiveCondition, lat = decimalLatitude, long = decimalLongitude, error_dist_m = coordinateUncertaintyInMeters) %>% 
  select(specimen_number, id, date_new, year, month, day, DOY, lat, long, elev_m, error_dist_m, sub_sp, repro, county, locality, habitat, datum, everything()) %>% 
  mutate(datum = replace(datum, which(datum %in% c("WGS84, Google Earth", "WGS 84")), "WGS84"))

#1452 Obs

#Removing Duplicate Records (Same Lat/Long, Date, [and locality when lat/long = NA])
library(janitor)

length(which(duplicated(nemo_2.5[,c(3,8,9)]))) #roughly 129 duplicates

nemo_drm <- nemo_2.5 %>% 
  distinct(date_new, lat, long, sub_sp, locality, county, .keep_all = TRUE) #Retains unique rows for combination of these variables
#specified sub_sp & locality to hand-pick out duplicates for missing lat/long
#1409 oobs - Only 43 observations removed

#Hand-check duplicates
nemo_2_dupes <- get_dupes(nemo_drm, date_new, lat, long, county) %>% 
  select(specimen_number, everything()) #ID w/ specimen number first

numbers_to_rm <- c("UC24241", "UC24224", "UC63798", "JEPS1894", "JEPS1891", "JEPS1915", "UC63802", "UC24242", "UC131446", "UC126906", "CAS-DS-135657", "UC163963", "JEPS3116", "UC489037", "JEPS1884", "UC572488", "JEPS21152", "UC1069172", "JEPS21153", "UC1069177", "UC596368", "JEPS1910", "UC606907", "UC606909", "SBBG164523", "JEPS1489", "UC606911", "UC606914", "UC666593", "SD00010669", "SD00010496", "SDSU06118", "UCSB030048", "JEPS8961", "SD00010483", "OBI106054", "SDSU06094", "SDSU06112", "UCR0077183", "SBBG164524", "OBI106029", "HSC201536", "OBI106027", "OBI106040", "JEPS82018", "JEPS82020", "UCR0077136", "SDSU15873", "UCR0077135", "IRVC100598", "IRVC100601", "UCR0077132", "OBI106018", "UCR0077114", "UCR0077113", "UCR0077203", "SBBG164511", "UCR0077198", "UCR0077047", "UCR0077195") #Stop line 60

# SDSU06118 removec but sub_sp was different from duplicate (menziesii rm)
#	OBI106029 and SBBG164524 "in organic garden - removed

nemo_3 <- nemo_drm %>% 
  filter(!(specimen_number %in% numbers_to_rm)) #bingo. 1676 obs after dupe removal
#60 more removed for 1349 obs == total of 103 duplicates removed!!

#double check
#dupes_check2 <- get_dupes(nemo_3, date_new, lat, long, county) %>% 
 # select(specimen_number, everything()) #ID w/ specimen number first
# All Good!

write_csv(nemo_3, here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned_V2.csv"))


