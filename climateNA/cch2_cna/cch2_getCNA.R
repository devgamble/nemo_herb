#
######
# Obtaining ClimateNA data for CCH2 records
######
# Devin Gamble, Jan 18, 2020


#This script provides a workflow for obtaining ClimateNA variables (Annual, monthly (or seasonal)) specific to herbarium records collection locations (lat/long). I read in the CLEANED version (see `data_cleaning` folder) of a csv of herbarium records downloaded from CCH2, standardize the columns so as to retreive historical climate data, and organize the climate data into new csv files.


# Please see `climate_import-clean.Rmd` file for general intsructions on downloading ClimateNA data using a standardized herbarium records csv.


#This script will need to be *RE-RUN* after new coordinates are added to `nemo_cch1/2_cleaned` to download climate data for those records (and elevations updated?)

#Packages
library(tidyverse)
library(here)
library(stringr)


#Standardizing specimen observations for ClimateNA-compatible csv input
#Columns should exatcly match c("ID1", "ID2", "lat", "long", "el")

nemo_2df <- read_csv(here::here("data_cleaning", "CCH2_scripts_data", "nemo_cch2_cleaned.csv")) %>% 
  rename(YoC = year) # Change year to YoC to avoid confusion and for easier merging later on

nemo_2df_CNA <- nemo_2df %>% 
  select(specimen_number, YoC, lat, long, elev_m) %>% 
  rename(ID1 = specimen_number, ID2 = YoC, el = elev_m) #specimen_number and year used as identifiers

str(nemo_2df_CNA)
# 5 variables, Number of observations should match that in read-in csv


#remove obs missing lat/long
nemo_2df_CNA <- nemo_2df_CNA %>% 
  filter(!is.na(lat)) 


#Export this compatible df to a csv for input into ClimateNA

write_csv(nemo_2df_CNA, here::here("ClimateNA", "cch2_cna", "cch2_CNA_sd.csv"), na = "") #Use <blank> instead of NA for missing elevation values

#For some reason, I could only get the input file to run with ClimateNA AFTER opening the csv and `clearing contents` of all the cells adjacent and below the main chunk of observations :/

#Note: Return to this script after lat/long are updated to increase # observations!


##
#####
# Organizing data from ClimateNA
#####
##
#See `climate_import-clean.Rmd` for instructions on downloading climateNA data

#Important! Ensure that all the col outputs from ClimateNA (ID1:elev_m) are renamed to be exaclty the same in the following dataframes. Otherwise merging them (below) will be difficult.

##
# Climate Normals for 1950-1981 and 1981-2010 (Annual averages over 30-year periods)

norms_1951_1980 <- read_csv(here::here("climateNA", "cch2_cna", "cch2_ann_Normal_1951_1980Y.csv"), na = c("-9999", "-9999.0")) %>% #Fix NAs
  rename(specimen_number = ID1, YoC = ID2, lat = Latitude, long = Longitude, elev_m = Elevation) 
#Fixing names - is this a good place to do this?

norms_1981_2010 <- read_csv(here::here("climateNA", "cch2_cna", "cch2_ann_Normal_1981_2010Y.csv"), na = c("-9999", "-9999.0")) %>% #Fix NAs
  rename(specimen_number = ID1, YoC = ID2, lat = Latitude, long = Longitude, elev_m = Elevation) 

##
# Climate TS 1901 - 2018 (117 years of data)
#Annual Variables
#
TS_ann <- read_csv(here::here("climateNA", "cch2_cna", "cch2_ann_1901-2018Y.csv"), col_types = cols(.default = "d", MAR = "d", ID1 = "c"), na = c("-9999", "-9999.0")) %>% #Specify col types to avoid errors due to many NAs 
  rename(specimen_number = ID1, YoC = ID2, lat = Latitude, long = Longitude, elev_m = Elevation) 


#Monthly Variables
#
TS_mon <- read_csv(here::here("climateNA", "cch2_cna", "cch2_mon_1901-2018M.csv"), col_types = cols(.default = "d", ID1 = "c"), na = c("-9999", "-9999.0")) %>% #Specify col types to avoid errors due to many NAs 
  rename(specimen_number = ID1, YoC = ID2, lat = Latitude, long = Longitude, elev_m = Elevation) 


#Function for specifying YoC annual/monthly variables by adding a character string
rename_ftn <- function(x, apnd, rang){         
  names_x <- colnames(x)[rang]
  print(paste(names_x, apnd, sep = ""))
} 


#Creating 
#YoC Annual data
YoC_ann <- TS_ann %>% 
  filter(YoC == Year) 
colnames(YoC_ann)[7:29] <- rename_ftn(YoC_ann, apnd = "_a", rang = 7:29) 
# _a indicates climate data from the YoC
YoC_ann <- YoC_ann %>% select(-Year) #remove ClimateNA provided year


#There is definitely an easier way to do this with rename_at() but I can't figure it out'
#Avoid using '-' in column names - looks weird in View()

#YoC monthly data
YoC_mon <- TS_mon %>% 
  filter(YoC == Year) 
colnames(YoC_mon)[7:174] <- rename_ftn(YoC_mon, apnd = "_m", rang = 7:174)
# _m indicates monthly climate data from the YoC
YoC_mon <- YoC_mon %>% select(-Year)

#YoC - 1 monthly data
YoC_minus1_mon <- TS_mon %>% 
  filter(Year == YoC - 1)
colnames(YoC_minus1_mon)[7:174] <- rename_ftn(YoC_minus1_mon, apnd = "_m_1", rang = 7:174)
# _m_1 indicates monthly climate data from the year prior to collection
YoC_minus1_mon <- YoC_minus1_mon %>% select(-Year)

#Normals
norms51 <- norms_1951_1980
colnames(norms51)[6:28] <- rename_ftn(norms51, apnd = "_n51", rang = 6:28) #_n51 or _51_80?

norms81 <- norms_1981_2010
colnames(norms81)[6:28] <- rename_ftn(norms81, apnd = "_n81", rang = 6:28)

#####
# Combining Individual climate datsets
#####
# The tricky part

# Both Normal dfs (1951-1980 and 1981-2010) should have the same number of observations as the standardized df input to ClimateNA (`nemo_2df_CNA`), which has missing lat/long's removed.
#YoC dfs will have fewer observations (only years 1901-2017). Check YoC_ann = YoC_mon > YoC_minus1_mon

norms_df <- merge(norms51, norms81) #5 common cols, 23 unique. new total = 46 + 5 = 51
# Similar to innner join

write_csv(norms_df, here::here("climateNA", "cch2_cna", "CNA_normals_cch2.csv"), na = "")


YoC_1 <- merge(YoC_minus1_mon,YoC_mon, all.x = TRUE) # cols = 173 + 173 - 5 = 341
YoC_df <- merge(YoC_ann, YoC_1, all.x = TRUE) # cols = 341 + 28 - 5 = 364
#Similar to a left join
#Contains Annual and monthly data for YoC, plus monthly data for YoC - 1

write_csv(YoC_df, here::here("climateNA", "cch2_cna", "CNA_YoC_ann_mon_cch2.csv"), na = "")


#Annual TS Data
# What to do?


#Fix/group more before writing csv?



#Group by 
#3-Dimensional Array




# Optional
#Combine Norms and YoC data
#YoC_norms <- merge(norms_df, YoC_ann, all.x = TRUE) #23 + 51 = 74 cols





#Easier to save both normals to one csv and annual/monthly to another?





