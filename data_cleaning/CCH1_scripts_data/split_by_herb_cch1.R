#
# Splitting the CCH1 cleaned dataset by Herbarium
#

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)
library(janitor)



# Load in the cleaned data & unique data - dupes shared w cch2 removed 



nemo_cch1_cleaned <- read_csv(here::here("data_cleaning", "CCH1_scripts_data", "cch1_cleaned_unique.csv"))
# Looks good #883 obs, 15 col



#Split cch1 into subsets by herbarium

cch1_CAS_to_POM <- nemo_cch1_cleaned %>% 
  filter(str_detect(specimen_number, "CAS-BOT|CDA|CHSC|CLARK|CSUSB|GH|GMDRC|HREC|HSC|IRVC|JEPS|JROH|^LA|NY|OBI|PASA|PGM|POM")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source) %>% 
  arrange(lat)
write_csv(cch1_CAS_to_POM, here("data_cleaning", "CCH1_scripts_data", "cch1_cleaned_splits", "nemo_cch1_C-P.csv"), na = "")


cch1_RSA <- nemo_cch1_cleaned %>% 
  filter(str_detect(specimen_number, "RSA")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source) %>% 
  arrange(lat)
write_csv(cch1_RSA, here("data_cleaning", "CCH1_scripts_data", "cch1_cleaned_splits", "nemo_cch1_R.csv"), na = "")

cch1_SBBG_to_Y <- nemo_cch1_cleaned %>% 
  filter(str_detect(specimen_number, "SBBG|SD|SEINET|SFV|SJSU|^UC|VV|YM")) %>% 
  mutate(georef_by = NA,
         georef_notes = NA) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source) %>% 
  arrange(lat)
write_csv(cch1_SBBG_to_Y, here("data_cleaning", "CCH1_scripts_data", "cch1_cleaned_splits", "nemo_cch1_S-Y.csv"), na = "")





