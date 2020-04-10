###########
## Download Specimen Images from **V2**
###########

# Devin Gamble, March 12 2020
#Updated March 25, 2020

#Load Packages
library(tidyverse)
library(here)
library(beepr)


#UPDATED script to grab images for Georeferenced Splits!    #was V2 cleaned CCH2 data


#Images dataframe with identifier and urls of high-res images (downloaded from cch2)
images_df <- read_csv(here("data_cleaning", "CCH2_scripts_data", "images_cch2_Dec19.csv"))


###
# A-C Block
###


#Add specimen_number from full dataset to images_df
id_spec_AC <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_AC_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_AC <- merge(id_spec_AC, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good



#Testing with one image:
download.file(images_df_AC[1,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_AC_p", paste(images_df_AC[1,2], ".jpg", sep = "")), method = "auto", mode = "wb")

#Here I've set the filenames to be from the `specimen_number` column. Change column to [,2] for for `specimen number`
#Downloading images with this code will replace images with the same filename


#Save all images:
for(i in 1:length(images_df_AC[,3])){
  download.file(images_df_AC[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_AC_p", paste(images_df_AC[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}


###
# D-L Block
###

id_spec_DL <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_DL_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_DL <- merge(id_spec_DL, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good


for(i in 1:length(images_df_DL[,3])){
  download.file(images_df_DL[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_DL_p", paste(images_df_DL[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}


###
# O-S Block
###
id_spec_OS <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_OS_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_OS <- merge(id_spec_OS, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good


for(i in 1:length(images_df_OS[,3])){
  download.file(images_df_OS[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_OS_p", paste(images_df_OS[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}



###
# S-U Block
###

id_spec_SU <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_SU_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_SU <- merge(id_spec_SU, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good


for(i in 1:length(images_df_SU[,3])){
  download.file(images_df_SU[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_SU_p", paste(images_df_SU[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}



###
# UCR Block
###

id_spec_UCR <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCR_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_UCR <- merge(id_spec_UCR, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good


for(i in 1:length(images_df_UCR[,3])){
  download.file(images_df_UCR[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_UCR_p", paste(images_df_UCR[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}


###
# UCSB Block
###

id_spec_UCSB <- read_csv(here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCSB_pi.csv")) %>% 
  select(id, specimen_number) %>% 
  arrange(id)

#Merge to images_df so both specimen identifiers are listed
images_df_UCSB <- merge(id_spec_UCSB, images_df, by = "id", all.x = FALSE) %>% arrange(id) #merges by `id` 
# 31 Observations - Good


for(i in 1:length(images_df_UCSB[,3])){
  download.file(images_df_UCSB[i,3], destfile = here("data_cleaning", "cch2_images_n", "nemo_cch2_UCSB_p", paste(images_df_UCSB[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}






