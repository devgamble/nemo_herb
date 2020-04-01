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

#
###UPDATE: New splits specified AT THE BOTTOM of this script. Many cch2 duplicates removed from `combine_cch1_cch2_V2.R`
#

#Record Starting and ending number of records (accounts for removal)!

##Filter out observations that will not be included

#Block A-C

nemo_cch2_AC_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_A-C_V2.csv"))

nemo_cch2_AC_p <- nemo_cch2_AC_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27")) %>%  #Adds WGS84 datum for all records where GEOLocate was used
  select(c(-remove_obs))

write_csv(nemo_cch2_AC_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_AC_p.csv"), na = "")

#Saves updated csv (obs removed) to 'splits_for_PI' folder

#Block D-L

nemo_cch2_DL_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_D-L_V2.csv"))

nemo_cch2_DL_p <- nemo_cch2_DL_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27")) %>%  
  select(c(-remove_obs))

write_csv(nemo_cch2_DL_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_DL_p.csv"), na = "")


#Block O-SD

nemo_cch2_OS_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_O-SD_V2.csv"))

nemo_cch2_OS_p <- nemo_cch2_OS_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27")) %>%  
  select(c(-remove_obs))

write_csv(nemo_cch2_OS_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_OS_p.csv"), na = "")


#Block SF-UC

nemo_cch2_SU_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_SF-UC_V2.csv"))

nemo_cch2_SU_p <- nemo_cch2_SU_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27")) %>%  
  select(c(-remove_obs))

write_csv(nemo_cch2_SU_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_SU_p.csv"), na = "")

#Block UCR

nemo_cch2_UCR_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_UCR_V2.csv"))

nemo_cch2_UCR_p <- nemo_cch2_UCR_g %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27"))

write_csv(nemo_cch2_UCR_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCR_p.csv"), na = "")


#Block UCSB

nemo_cch2_UCSB_g <- 
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "georef_splits", "nemo_cch2_UCSB_V2.csv"))

nemo_cch2_UCSB_p <- nemo_cch2_UCSB_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  mutate(scored_by = NA,
         notes = NA,
         plant = NA,
         Total = NA,
         buds_1 = NA,
         flowers_2 = NA,
         spent_3 = NA,
         immature_4 = NA,
         mature_5 = NA) %>% 
  select(specimen_number:DOY, sub_sp, repro, county:habitat, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, references, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" ~ "NAD83",
                           datum == "NAD27" ~ "NAD27")) %>%  
  select(c(-remove_obs))

write_csv(nemo_cch2_UCSB_p, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCSB_p.csv"), na = "")

########################
#This chunk added after running `combine_cch1_cch2_V2.R`
##
##########
# NEW SPLITS for PI Scoring
##########
##
#



nemo_all_1_cch2 <- read_csv(here::here("data_cleaning", "nemo_all_1.csv")) %>% 
  filter(source == "cch2")
#1238 cch2 obs, 530 cch1 obs


#AC
cch2_AC_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "^AHUC|BSCA|CAS|COLL|CSUS"))
write_csv(cch2_AC_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_AC_pi.csv"), na = "")
#23 obs


#DL
cch2_DL_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "DAV|HSC|IRVC|JEPS|LOB"))
write_csv(cch2_DL_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_DL_pi.csv"), na = "")
#236 obs


#OS
cch2_OS_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "OBI|RSA|SBBG|SD00|SDSU"))
write_csv(cch2_OS_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_OS_pi.csv"), na = "")
#327 obs


#SU
cch2_SU_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "SFV|^UC1|^UC2|^UC3|^UC4|^UC5|^UC6|^UC7|^UC8|^UC9"))
write_csv(cch2_SU_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_SU_pi.csv"), na = "")
#376 obs


#UCR
cch2_UCR_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "UCR0"))
write_csv(cch2_UCR_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCR_pi.csv"), na = "")
#199 obs


#UCSB
cch2_UCSB_pi <- nemo_all_1_cch2 %>% 
  filter(str_detect(specimen_number, "UCSB"))
write_csv(cch2_UCSB_pi, here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCSB_pi.csv"), na = "")
#77 obs


