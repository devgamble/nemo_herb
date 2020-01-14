###########
#
## Download Specimen Images from 'images' csv file from CCH2
#
###########

# Devin Gamble, Dec 20 2019

#Load Packages
library(tidyverse)
library(here)
library(beepr)


images_df <- read_csv(here("data_cleaning", "CCH2_scripts_data", "images_cch2_Dec19.csv"))


#Add specimen_number from full dataset to images_df
id_spec <- read_csv(here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_Dec2019.csv")) %>% 
  select(id, specimen_number)

images_df <- merge(id_spec, images_df) #merges by `id`


#Testing with one image:
download.file(images_df[1,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[1,1], ".jpg", sep = "")), method = "auto", mode = "wb")

#Here I've set the filenames to be from the `id` column, but maybe `specimen_number` would be better?
#id might differ (?) between cch1 and cch2 datasets


#Trying 10 before going big...
for(i in 1:10){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[i,1], ".jpg", sep = "")), method = "auto", mode = "wb")
}


######
# WARNING!
######
#
# DO NOT run the following code UNLESS 
# you are prepared to wait hours to download all of these images AND you have sufficient space on your local machine (Roughly 4.7 Gigabytes)

# Also
# I would suggest un-staging these images before you commit/push to github. Alternatively, specify a gitignore file to untrack the image folder.


#Save all images:
for(i in 1:length(images_df[,3])){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[i,1], ".jpg", sep = "")), method = "auto", mode = "wb")
}
beep("mario")


