###############
## Splitting up georeferenced records by herbarium for PI scoring (CCH2)
###############
#

#Using the georeferenced data sheets with lat/long=NA observations removed, split up the data by herbarium for Phenological Index scoring

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)

#Don't need to recombine herbarium splits since the same splits will be used for georeferencing
#Be sure records tagged for removal are deleted before creating new csv for PI scoring

#Record Starting and ending number of records (accounts for removal!

##Filter out observations that will not be included

nemo_cch2_AC_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_A-C_V2_test.csv"))

nemo_cch2_AC_p <- nemo_cch2_AC_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA,
         scored_by = NA,
         notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  select(c(-remove_obs))

write_csv(nemo_cch2_AC_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_AC_p.csv"), na = "")

#example: 31 observations --> 27


cch2_DAV_to_LOB_V2 <- nemo_cch2_cleaned2 %>% 
  filter(str_detect(specimen_number, "DAV|HSC|IRVC|JEPS|LOB")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, references) %>% 
  arrange(lat)
write_csv(cch2_DAV_to_LOB_V2, here("data_cleaning", "CCH2_scripts_data", "cleaned_splits_V2", "nemo_cch2_D-L_V2.csv"), na = "")


cch2_OBI_to_SDSU_V2 <- nemo_cch2_cleaned2 %>% 
  filter(str_detect(specimen_number, "OBI|RSA|SBBG|SD00|SDSU")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, references) %>% 
  arrange(lat)
write_csv(cch2_OBI_to_SDSU_V2, here("data_cleaning", "CCH2_scripts_data", "cleaned_splits_V2", "nemo_cch2_O-SD_V2.csv"), na = "")


cch2_SFV_to_UC9_V2 <- nemo_cch2_cleaned2 %>% 
  filter(str_detect(specimen_number, "SFV|^UC1|^UC2|^UC3|^UC4|^UC5|^UC6|^UC7|^UC8|^UC9")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, references) %>% 
  arrange(lat)
write_csv(cch2_SFV_to_UC9_V2, here("data_cleaning", "CCH2_scripts_data", "cleaned_splits_V2", "nemo_cch2_SF-UC_V2.csv"), na = "")


cch2_UCR_V2 <- nemo_cch2_cleaned2 %>% 
  filter(str_detect(specimen_number, "UCR0")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, references) %>% 
  arrange(lat)
write_csv(cch2_UCR_V2, here("data_cleaning", "CCH2_scripts_data", "cleaned_splits_V2", "nemo_cch2_UCR_V2.csv"), na = "")


cch2_UCSB_V2 <- nemo_cch2_cleaned2 %>% 
  filter(str_detect(specimen_number, "UCSB")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, references) %>% 
  arrange(lat)
write_csv(cch2_UCSB_V2, here("data_cleaning", "CCH2_scripts_data", "cleaned_splits_V2", "nemo_cch2_UCSB_V2.csv"), na = "")



