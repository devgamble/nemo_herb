#####
# Combining cch1 & cch2 herbarium data
#####

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)
library(janitor)



#Objective: Combine cch1 and cch2 data and remove duplicates from *cch1 only*!

# Load in the cleaned data - output from cch1_cleaning.R
nemo_cch1_cleaned <- read_csv(here::here("data_cleaning", "CCH1_scripts_data", "nemo_cch1_cleaned.csv"))
# Looks good #1676 obs, 15 col


##
### Filter out duplicates shared with CCH2!
##
#Combine to first identify duplicates, then remove duplicates *from cch1 only*


#Load Cleaned CCH2 data #1349 obs, 19 col
nemo_cch2 <- read_csv(here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned_V2.csv"))

#Combine cch1 and cch2
nemo_comb <- bind_rows(nemo_cch1_cleaned, nemo_cch2) #3025 obs total


#Pull out dupes from cch1 only
length(which(duplicated(nemo_comb[,c(2,7,8,13)]))) #shows 332 dupes for date, lat/long, locality


#First remove cch1 record dupes where locality is an exact match!!
nemo_comb_dupes1 <- get_dupes(nemo_comb, date_new, lat, long, county, locality) %>% 
  select(specimen_number, source, everything()) %>% 
  filter(source == "cch1") #show duplicates shared by both df's present only in cch1 data
#List of specimen_numbers to remove based on this criteria
cch1_specrm_1 <- c(nemo_comb_dupes1$specimen_number) #311 dupes

#Update combined df
nemo_comb2 <- nemo_comb %>% 
  filter(!(specimen_number %in% cch1_specrm_1 & source == "cch1")) 
#removes numbers from this list that are sourced from cch1


#Check for matching specimen numbers after removing identical localities
nemo_comb_dupes2 <- get_dupes(nemo_comb2, specimen_number, date_new, lat, long, county) %>% 
  select(specimen_number, source, everything()) %>% 
  filter(source == "cch1")
cch1_specrm_2 <- c(nemo_comb_dupes2$specimen_number) #114 dupes

#Update combined df (2)
nemo_comb3 <- nemo_comb2 %>% 
  filter(!(specimen_number %in% cch1_specrm_2 & source == "cch1")) #remove identical numbers from cch1
#Down to 2600 obs now


#Now rm records where available lat/long & dates match
nemo_comb_dupes3 <- get_dupes(nemo_comb3, date_new, lat, long, county) %>% 
  select(specimen_number, source, everything()) %>% 
  filter(!(is.na(lat))) %>% #For lat/long that are not NA - safely assume these are identical localities
  filter(source == "cch1")
cch1_specrm_3 <- c(nemo_comb_dupes3$specimen_number) #351 dupes

#Update combined df (3) 
nemo_comb4 <- nemo_comb3 %>% 
  filter(!(specimen_number %in% cch1_specrm_3 & source == "cch1")) #rm cch1 dupes w lat/long
#Down to 2249 obs now


#108 potential matches missing lat/long - Scan through & check by hand
nemo_comb_dupes4 <- get_dupes(nemo_comb4, date_new, lat, long, county) %>% 
  select(specimen_number, source, everything()) #cannot rm cch1 before checking if they are distinct
#Selected cch1 records to rm when localities were near identicial - assumed to be dupes
cch1_specrm_4 <- c("POM65386", "POM3465", "SEINET5521032", "POM97169", "RSA75870", "SD38531", "RSA8325", "SD40793", "SD00010501", "RSA20888", "RSA75886", "RSA75887", "POM249048", "RSA75888", "UCD120082", "UCD35080", "SD44541", "SD44504", "UCSB030046", "SDSU6118", "SD52427", "RSA396242", "SD51051", "UCSB5325", "UCD126443", "HSC13600", "HSC13604", "RSA244978", "HSC87763", "SDSU6130", "POM347130", "HSC39198", "SDSU6106", "HSC26644", "RSA298351", "HSC36876", "RSA418543", "OBI20178", "UCSB42030", "HSC59333", "SD111947", "SEINET5521001", "SEINET10497055", "RSA659263", "HSC85866", "UCSB63597", "UCSB69908", "OBI58215", "UCSB72228") #49 more cch1 dupes to rm

#SD00010501 dupe within-cch2 dupe (unique but same date & location as UC880918
#UCSB030046 dupe w/in cch2 (unique but same date & location as UC1541101)
#POM347130 dupe w/in cch1 removed (same data & location as POM318298)
#OBI20178 dupe w/in cch1 removed (same as SBBG54841)

#Update combined df (4)
nemo_comb5 <- nemo_comb4 %>% 
  filter(!(specimen_number %in% cch1_specrm_4))  #2200 obs!
  

#All duplicates should be removed (for source = cch1)!
#Last check for duplicates
dupes_f <- get_dupes(nemo_comb5, date_new, lat, long, county) %>% 
  select(specimen_number, source, everything()) #None of these appear to be dupes!! Let 'em be

nemo_comb_f <- nemo_comb5 %>% 
  select(specimen_number, date_new, year, month, day, DOY, lat, long, elev_m, error_dist_m, sub_sp, repro, county, locality, habitat, datum, id, everything()) #Organizing columns



#Save combined csv
write_csv(nemo_comb_f, here::here("data_cleaning", "cch1&2_combined.csv"))


#823 dupes w overlap in cch2 removed from cch1! (1676 - 853)

#Save CCH1 cleaned & Unique (cch2-dupe-removed) csv 
nemo_cch1_drm_cleaned <- nemo_comb_f %>% 
  filter(source == "cch1") %>% 
  select(-c(repro, habitat, id, references)) #remove cch2-only variables
#853 unique records from cch1
#Implies 1347 unique records from cch2 (2 dupes missed in first cleaning - SD00010501, UCSB030046)


write_csv(nemo_cch1_drm_cleaned, here::here("data_cleaning", "CCH1_scripts_data", "cch1_cleaned_unique.csv"))




