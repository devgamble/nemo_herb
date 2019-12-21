###########
#
## Download Specimen Images from 'images' csv file from CCH2
#
###########

# Devin Gamble
# Dec 20, 2019

#Load Packages
library(tidyverse)
library(here)
library(beepr)


images_df <- read_csv(here("data_cleaning", "images_cch2_Dec19.csv"))


#Add specimen_nummber to images_df
id_spec <- read_csv(here("data_cleaning", "nemo_cch2_Dec2019.csv")) %>% 
  select(id, specimen_number)

images_df <- merge(id_spec, images_df)


#Testing with one image:
download.file(images_df[1,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[1,1], ".jpg", sep = "")), method = "auto", mode = "wb")

#Here I've set the filenames to be the `id` column, but maybe `specimen_number` would be better?
#id likely to differ (?) between cch1 and cch2 datasets

#Note: It may be more appropriate (and less risky) to download images to a folder outside the R project this script was saved in. This will avoid complications with commiting/pushing to Github.


#Trying 10 before going big...
for(i in 1:10){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[i,1], ".jpg", sep = "")), method = "auto", mode = "wb")
}


######
# WARNING!
######
#
# DO NOT
# run the following code 
# UNLESS 
# you are prepared to wait HOURS to download these all of these images AND you have sufficient space on your local machine (Roughly 4.7 Gigabytes)

# Also
# I would suggest un-staging these images before you commit/push to github, if you choose to download these images inside the R project folder as I have done here.


#SAVE ALL IMAGES:
for(i in 1:length(images_df[,3])){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images", paste(images_df[i,1], ".jpg", sep = "")), method = "auto", mode = "wb")
}
beep("mario")


