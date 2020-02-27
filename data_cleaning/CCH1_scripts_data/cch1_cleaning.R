#
# Data Cleaning for the CCH1 occurrence dataset - initially compiled & partially standardized by Ivana Gomez [Dec 2019 - Feb 2020] - See other scripts in CCH1 folder: data_cleaning_724 and _1025
#

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)


# NOTE: For future cleaning, refer to cch2_cleaning.R script - assuming the future CCH (1 & 2 combined?) is in a friendlier, more standardized format.
#No habitat column, maybe it's missing or was lumped in with locality?

#Thus far, Ivana has standardized dates and calculated the DOY (accounting for leap year), standardized the error distance (coordinate uncertainty) to meters, and removed incomplete/erroneous records.

###
# Tasks
###
# Standardized elevation to meters
# Remove unecessary columns
# Include single date column - use lubridate to standardize [verbatim is most recent - IG]
# Add columns for subspecies, day, month, year
# standardize columns to format that matches cch2 data



# Clean & Standardize Dates

nemo_cch1 <- read_csv(here::here("data_cleaning", "CCH1_scripts_data", "Nemo_CCH1_02182020.csv")) %>% 
  select(- c(X1, collector, coll_number_prefix, coll_number_suffix, collection_date, late_collection_date, source, citation, error_distance, units)) %>%  #Removing these columns
  mutate(sub_sp = case_when(`taxon name` %in% c("Nemophila menziesii subsp. menziesii", "Nemophila menziesii var. menziesii") ~ "menziesii",
                            `taxon name` %in% c("Nemophila menziesii subsp. atomaria", "Nemophila menziesii var. atomaria") ~ "atomaria",
                            `taxon name` %in% c("Nemophila menziesii var. integrifolia", "Nemophila menziesii subsp. integrifolia", "Nemophila menziesii subsp. australis", "Nemophila menziesii var. annulata", "Nemophila menziesii var. intermedia", "Nemophila menziesii var. minima") ~ "integrifolia")) %>% 
  filter(!`taxon name` %in% c("Nemophila menziesii var. rotata")) #not menzies


#Get all dates in same format
#59 obs in format yyy-mm-dd
nemo_cch1 <- nemo_cch1 %>% 
  mutate(date_new = case_when(str_detect(verbatim_date, "-") ~ as_date(ymd(verbatim_date)),
         str_detect(verbatim_date, "/") ~ as_date(mdy(verbatim_date))))

#Get warning "failed to parse" but it looks like the conversion was successful? No NA values :)
#cleaned CCH2 data has `date_new` in YYYY-MM-DD so this is good!



#Fix Elevation 
#1161 records with elevation in m or ft.
#Most have spaces, many don't


nemo_cch1_1 <- nemo_cch1 %>% 
  mutate(el_units = case_when(
    str_detect(elevation, "m") ~ "m",
    str_detect(elevation, "f") ~ "f"))
    
#standardize to m 
nemo_cch1_2 <- nemo_cch1_1 %>% 
  mutate(elev_num = as.numeric(gsub("\\D", "", elevation))) %>% 
  mutate(elev_m = case_when(el_units == "m" ~ elev_num,
                            el_units == "f" ~ (0.3048*elev_num))) #conversion factor

#Success!


#Removing columns we've transformed and don't need
#Renaming columns to match with cch2 data
#Adding `source` column to specify cch1

nemo_1 <- nemo_cch1_2 %>% #get rid of coll_number, only 279 entries
  select(-c(`taxon name`, verbatim_date, coll_number, elevation, el_units, elev_num)) %>%  
  mutate(source = "cch1") %>% 
  rename(lat = latitude, long = longitude, DOY = doy, error_dist_m = new_error_distance_m) %>% 
  mutate(datum = na_if(datum, "not recorded")) %>% 
  mutate(datum = replace(datum, which(datum %in% c("Geolocate", "Geolocate (copied from A343463)", "Geolocate (copied from DS135657)", "WGS 1984")), "WGS84")) %>% 
  mutate(datum = replace(datum, which(datum %in% c("NULL", "null")), NA))
  
                    


#Parse out date into Y, M, D columns
#Reorder columns

nemo_1a <- nemo_1 %>% 
  mutate(year = year(date_new),
         month = month(date_new),
         day = day(date_new)) %>% 
  select(specimen_number, date_new, year, month, day, DOY, lat, long, elev_m, error_dist_m, sub_sp, county, locality, source, datum) 
  #mutate(DOY2 = yday(date_new))  #double checks original DOY # and it's good



#CCH2 has id, habitat, repro while cch1 does not
# 1823 observations



#Check for duplicates

length(which(duplicated(nemo_1a[,c(2,7,8)]))) #180 duplicates? potentially more (triplicates)
#nemo_dupes1 <-  slice(nemo_1a, which(duplicated(nemo_1a[,c(2,7,8)])))
nemo_dupes2 <- get_dupes(nemo_1a, date_new, lat, long) %>% 
  select(specimen_number, everything()) # Shows all dupes, better?



#Trying the `distinct` function
#nemo_test1 <- nemo_1a %>% 
 # distinct(date_new, lat, long, county, .keep_all = TRUE) #Removed 180 duplicates


#Removing duplicated specimens - same exact date & lat/long
#removing record with less info OR from non-CA herbarium OR at random
#Include missing lat/long, which could differ for specimens collected on same date

nemo_drm <- nemo_1a %>% 
  distinct(date_new, lat, long, sub_sp, locality, county, .keep_all = TRUE) #specified sub_sp & locality to hand-pick out duplicates. #Duplicates with blank/identical locality/ssp removed

nemo_dupes3 <- get_dupes(nemo_drm, date_new, lat, long, county) %>% 
  select(specimen_number, everything()) #All dupes after removing those without unique sub_sp or locality

numbers_to_rm <- c("UC24241", "UC24224", "UC24223", "UC24206", "UC24242", "POM3740", "POM65266", "UC126906", "UC126908", "SEINET3156489", "SEINET3156488", "SEINET5521008", "POM3469", "NY337174", "SEINET3156482", "CAS-BOT-BC7216", "UC75480", "RSA0003798", "SEINET10743479", "SEINET5521030", "SEINET5521025", "UC111549", "POM14756", "POM3743", "SD58283", "JEPS3116", "UCD120222", "UC310947", "RSA75871", "RSA75879", "RSA10160", "UCD120078", "JEPS1884", "RSA6997", "UC572488", "UC572487", "UC880908", "UC1069175", "UC1069177", "UC718104", "OBI13911", "POM248971", "JEPS1910", "JEPS1494", "UC606907", "JEPS1490", "UC606909", "CHSC76195", "JEPS1489", "UC606911", "POM249024", "UC880907", "SD75491", "UC668349", "SD28484", "UC666593", "UC697772", "UCD120228", "RSA344390", "SD124417", "UC1541101", "RSA51317", "OBI13916", "PGM5924", "PGM5925", "SD125572", "OBI45546", "SDSU6094", "SDSU6112", "HSC13589", "UCD120081", "UCR95839", "SD88406", "OBI39923", "UC1426018", "HSC35029", "HSC65622", "UCD23403", "UCD23407", "UCD23408", "OBI47389", "RSA583625", "RSA754914", "RSA721608", "UCR132817", "RSA699605", "UC1927593", "UC1927520", "UCR128013", "RSA681345", "UC1979852", "UCR204322", "UC1980035", "SD239783", "RSA771734", "RSA0093734", "RSA0110795", "RSA0098372", "SD247444", "UCR249971", "SD247659") #duplicates and triplicates w/ less info/randomly chosen to remove #101 additional duplicates to remove.
#RSA721608 was a duplicate and removed, but listed as different subspecies (integrifolia)


nemo_1b <- nemo_drm %>% 
  filter(!(specimen_number %in% numbers_to_rm)) #bingo. 1676 obs after dupe removal


#
# Final cleaning, organizing, and export

nemo_cch1_clean <- nemo_1b

write_csv(nemo_cch1_clean, here::here("data_cleaning", "CCH1_scripts_data", "nemo_cch1_cleaned.csv"))

