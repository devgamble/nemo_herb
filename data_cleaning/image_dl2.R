###########
## Download Specimen Images from **V2**
###########

# Devin Gamble, March 12 2020

#Load Packages
library(tidyverse)
library(here)
library(beepr)


#UPDATED script to grab images for V2 cleaned CCH2 data!


#Images dataframe with identifier and urls of high-res images (downloaded from cch2)
images_df <- read_csv(here("data_cleaning", "CCH2_scripts_data", "images_cch2_Dec19.csv"))


#Add specimen_number from full dataset to images_df
id_spec <- read_csv(here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned_V2.csv")) %>% 
  select(id, specimen_number)

#Merge to images_df so both specimen identifiers are listed
images_df <- merge(id_spec, images_df, by = "id", all.x = FALSE) #merges by `id` # extra obs from images not included
#But now theres 1349 + 1 obs. Not sure where the extra came from


#If you have cloned/downloaded this project from Github (i.e. you are not me), you need to create your own local "cch2_images" folder and specify it's pathway in the following code. I suggest adding it to your  .gitignore file so that images you download are not shared to the main github project. You can do this by right-clicking the folder in the `Git` pane and clicking `ignore`. 


#Testing with one image:
download.file(images_df[1,3], destfile = here("data_cleaning", "cch2_images_n", paste(images_df[1,2], ".jpg", sep = "")), method = "auto", mode = "wb")

#Here I've set the filenames to be from the `specimen_number` column. Change column to [,1] for for `id`
#Downloading images with this code will replace images with the same filename


#Trying 10 before going big...
for(i in 1:10){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images_n", paste(images_df[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}


######
# WARNING!
######
#
# DO NOT run the following code UNLESS 
# you are prepared to wait hours to download all of these images AND you have sufficient space on your local machine (Roughly 4.7 Gigabytes). DO NOT push changes in the images folder to the main GitHub project!



#Save all images:
for(i in 1:length(images_df[,3])){
  download.file(images_df[i,3], destfile = here("data_cleaning", "cch2_images_n", paste(images_df[i,2], ".jpg", sep = "")), method = "auto", mode = "wb")
}
beep("mario")


