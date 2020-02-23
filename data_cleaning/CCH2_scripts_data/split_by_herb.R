#
# Splitting the cleaned dataset (CCH2) into csv subsets for each herbarium
#

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)



# Load in the cleaned data - output from cch2_cleaning.R

nemo_cch2_cleaned <- read_csv(here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned.csv"))
# Looks good


cch2_AHUC_to_CSUS <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "^AHUC|BSCA|CAS|COLL|CSUS")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_AHUC_to_CSUS, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_A-C.csv"), na = "")


cch2_DAV_to_LOB <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "DAV|HSC|IRVC|JEPS|LOB")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_DAV_to_LOB, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_D-L.csv"), na = "")


cch2_OBI_to_SDSU <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "OBI|RSA|SBBG|SD00|SDSU")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_OBI_to_SDSU, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_O-SD.csv"), na = "")


cch2_SFV_to_UC9 <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "SFV|^UC1|^UC2|^UC3|^UC4|^UC5|^UC6|^UC7|^UC8|^UC9")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_SFV_to_UC9, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_SF-UC.csv"), na = "")


cch2_UCR <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "UCR0")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_UCR, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_UCR.csv"), na = "")


cch2_UCSB <- nemo_cch2_cleaned %>% 
  filter(str_detect(specimen_number, "UCSB")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, source) %>% 
  arrange(lat)
write_csv(cch2_UCSB, here("data_cleaning", "CCH2_scripts_data", "cch2_cleaned_splits", "nemo_cch2_UCSB.csv"), na = "")

