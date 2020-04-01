#####
# Combining cch1 & cch2 herbarium data VERSION 2
#####

#Load Packages
library(stringr)
library(magrittr)
library(tidyverse)
library(here)
library(janitor)



#Objective: Combine cch1 and cch2 data V2 - georeferencing complete
#MOST duplicates have been removed
#All records are georeferenced (have lat/long)
#CCH2 specimens will be scored for PI concurrently and scores will be added at a later time


## CCH1 Import and cleanup

#C
nemo_cch1_CP_g <- 
  read_csv(here::here("data_cleaning", "CCH1_scripts_data", "georeferenced_cch1_splits", "nemo_cch1_C-P.csv"))

cch1_g_C <- nemo_cch1_CP_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" | datum == "NAD 1983" ~ "NAD83",
                           datum == "NAD27" | datum == "NAD 27" | datum == "NAD 1927" ~ "NAD27",
                           datum == "NAD83/WGS84" | datum == "WGS84/NAD83" ~ "WGS84/NAD83")) %>%  
  select(c(-remove_obs))
#287 obs


#R
nemo_cch1_R_g <- 
  read_csv(here::here("data_cleaning", "CCH1_scripts_data", "georeferenced_cch1_splits", "nemo_cch1_R.csv"))

cch1_g_R <- nemo_cch1_R_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" | datum == "NAD 1983" ~ "NAD83",
                           datum == "NAD27" | datum == "NAD 27" | datum == "NAD 1927" ~ "NAD27",
                           datum == "NAD83/WGS84" | datum == "WGS84/NAD83" ~ "WGS84/NAD83")) %>%  
  select(c(-remove_obs))
#335 obs


#S
nemo_cch1_SY_g <- 
  read_csv(here::here("data_cleaning", "CCH1_scripts_data", "georeferenced_cch1_splits", "nemo_cch1_S-Y.csv"))

cch1_g_S <- nemo_cch1_SY_g %>% 
  filter(!(remove_obs %in% "y")) %>% 
  select(specimen_number:DOY, sub_sp, county, locality, elev_m, lat:long, error_dist_m, georef_by, georef_notes, datum, source, everything()) %>% 
  mutate(datum = case_when(georef_by != '' | datum == "WGS84" ~ "WGS84",
                           datum == "NAD83" | datum == "NAD 83" | datum == "NAD 1983" ~ "NAD83",
                           datum == "NAD27" | datum == "NAD 27" | datum == "NAD 1927" ~ "NAD27",
                           datum == "NAD83/WGS84" | datum == "WGS84/NAD83" ~ "WGS84/NAD83")) %>%  
  select(c(-remove_obs))
#189 obs


#Merge all the cch1 data

nemo_cch1_g <- bind_rows(cch1_g_C, cch1_g_R, cch1_g_S)
#Beautiful!! 811 observations in CCH1 after georeferencing



#Assembling the georeferenced CCH2 data - already cleaned up from `split_by_herb_PI.r`

cch2_AC <- #31 obs
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_AC_p.csv"))
cch2_DL <- #247 obs
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_DL_p.csv"))
cch2_OS <- #344
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_OS_p.csv"))
cch2_SU <- #396
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_SU_p.csv"))
cch2_UCR <- #223
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCR_p.csv"))
cch2_UCSB <- #81
  read_csv(here::here("data_cleaning", "CCH2_scripts_data", "splits_for_PI", "nemo_cch2_UCSB_p.csv"))
#should total 1322

#Joining all
nemo_cch2_g <- bind_rows(cch2_AC, cch2_DL, cch2_OS, cch2_SU, cch2_UCR, cch2_UCSB)
#Looks good! 1322 obs



#Joining Both!

nemo_all_1 <- bind_rows(nemo_cch1_g, nemo_cch2_g)
#2133 obs!


#########
#Objective: Remove further duplicates (and clean) this combined dataset, retaining all information. This will produce the first iteration of the final CCH dataset (before phenological scores are added)
#########
#Many of these 'duplicates' are distinct specimens, but were collected at the same location on the same exact date!
#Limit removal bias: Delete duplicates in cch1 (lack pictures). If both duplicates are cch2, delete whichever record has less information (habitat, sub_sp, repro, locality...). Otherwise, delete second dupe.
#Potential duplicates are considered distinct when the difference in lat/long estimate is > 0.5 and/or the listed localities are dissimilar enough to imply different collection locations.

length(which(duplicated(nemo_all_1$date_new, nemo_all_1$lat, nemo_all_1$long)))
#No dupes found when including date_new, lat, long, and county; but

nemo_all_dupes1 <- get_dupes(nemo_all_1, date_new, lat, long) %>% 
  select(specimen_number, source, everything())

specrm_1 <- c("CAS-DS-135673", "RSA0003798", "POM3489", "JEPS1793", "UCD120224", "RSA0036099", "UCD120077", "SBBG6133", "UCD35998", "UCD120229", "UCD162873", "UCD120223", "UCD120075", "UCD120245", "UCD120079", "UCD120233", "SFV11583", "RSA201828", "UCD120237", "BSCA0946", "UCD82053", "UCD160137", "UCD120248", "UCD159735", "UCD158879", "UCD160061", "UCD159790", "UCD158890", "UCD120217", "UCD160445", "UCD120246", "UCD120247", "UCD120214", "UCD120255", "UCD120254", "RSA0036743", "UCD19423", "UCD102389", "BSCA0941", "BSCA0942", "BSCA4977", "UCD120251", "UCD89680", "UCD149685", "UCD56723", "UCD120252", "UCR0076997", "UCR0076994", "RSA0073978", "RSA0043193", "RSA0022321", "RSA0068234", "UCD118354", "RSA0043164", "RSA0011369", "UCD140571", "UCD131927", "RSA0042499", "RSA0067586", "RSA0073853", "RSA0065634", "RSA0073849")

#CCH2 records removed: CAS-DS-135673, RSA0003798, RSA0036099, BSCA0946, BSCA0941, BSCA0942, BSCA4977, UCR0076997, UCR0076994, RSA0068234, RSA0042499

#Many records have the same specimen ID!!! Remove matches w/ specimen number & source = cch1

nemo_all_2 <- nemo_all_1 %>% 
  filter(!(specimen_number %in% specrm_1 & source == "cch1"))

specrm_2 <- c("CAS-DS-135673", "RSA0003798", "RSA0036099", "BSCA0946", "BSCA0941", "BSCA0942", "BSCA4977", "UCR0076997", "UCR0076994", "RSA0068234", "RSA0042499")

nemo_all_3 <- nemo_all_2 %>% 
  filter(!(specimen_number %in% specrm_2 & source == "cch2"))
#Should remove a total of 62 dupes from original 2133, and it does! 2071 obs

#######
#Now,
#Check for dupes with manually georeferenced records
#######
#Retain all cch2 records, followed by all manually referenced cch2 records with more precise error distances
nemo_all_dupes2 <- get_dupes(nemo_all_3, date_new, specimen_number) %>% 
  select(specimen_number, source, lat, long, everything()) #identical specimen numbers, almost the same lat/long ('human error')


specrm_3 <- c("RSA0003800", "UC303377", "UC24222", "UC67144", "UC146183", "RSA0059611", "JEPS1804", "JEPS1794", "UC249525", "UC249523", "UC1103811", "UC562583", "JEPS1497", "JEPS1518", "RSA0043192", "RSA0051320", "JEPS1883", "UC571711", "RSA0028725", "JEPS1477", "RSA0043165", "JEPS1791", "RSA0036056", "JEPS1481", "JEPS104716", "UC606903", "UC606908", "UC1239737", "JEPS1478", "UC660076", "RSA0059738", "JEPS45200", "JEPS76985", "JEPS60186", "JEPS85033", "RSA0070947", "JEPS83176", "RSA0035444", "RSA0060576", "UC1871057", "UC1871236", "RSA0070948", "RSA0046820", "RSA0043194", "UC1922890", "RSA0094763")
#Only cch1 dupe records

nemo_all_4 <- nemo_all_3 %>% 
  filter(!(specimen_number %in% specrm_3 & source == "cch1")) #specify source = cch1
#46 dupes rm --- 2025 obs



#checking county dupes (though county may differ: 'San Bernardino' vs 'San Bernardino County')
#Remove all within-source duplicates (e.g. same date & location within cch1)
nemo_all_dupes3 <- get_dupes(nemo_all_4, date_new, county, source) %>% 
  select(specimen_number, source, lat, long, georef_by, everything())

specrm_4 <- c("UCR0077197", "UC24243", "SD00010671", "POM88484", "HSC13579", "HSC13597", "RSA10775", "PASA2116", "UC404764", "UC494164", "UC691704", "JEPS1883", "UC797643", "JEPS1886", "UC606910", "UC1057474", "SBBG164528", "UCSB030069", "UC774697", "SEINET7063430", "OBI106052", "UC1541101", "RSA60205", "SBBG164503", "UC1285314", "SBBG82156", "UC1057251", "UCSB030032", "SBBG164504", "UCR0077202", "POM320434", "SBBG164518", "SDSU06106", "SBBG164514", "UCR0077193", "HSC34570", "HSC37202", "HSC35256", "RSA307343", "UCR0077184", "UCR0077174", "UCR0077206",  "SBBG164501", "UC1608275", "SBBG123782", "UCR0077181", "UCR0077120", "UCR0077179", "UC1787762", "UCR0077020", "RSA708611", "UC1922890", "UCR0077018", "UCR0077027", "SFV112654", "UC1789991", "CHSC97984", "UCR0077129", "SD00010596", "UCR0077185", "UCR0077170", "UCR0077172", "UCR0077052","UCR0077014")
#Many of these simply share collection dates and were collected from different locations

#CCH2 records removed [2]: UCR0077197, UC24243, SD00010671, UC404764, UC494164, UC691704, JEPS1883, UC797643, JEPS1886, UC606910, UC1057474, UC774697, OBI106052, UC1541101, SBBG164503, UC1285314, UC1057251, UCSB030032, SBBG164504, UCR0077202, SBBG164518, SDSU06106, SBBG164514, UCR0077193, UCR0077184, UCR0077174, UCR0077206, SBBG164501, UC1608275, UCR0077181, UCR0077120, UCR0077179, UC1787762, UCR0077020, UC1922890, UCR0077018, UCR0077027, SFV112654, UC1789991, UCR0077129,  SD00010596, UCR0077185, UCR0077170, UCR0077172, UCR0077052, UCR0077014


#All specimen numbers should now be unique, should not need to specify source
nemo_all_5 <- nemo_all_4 %>% 
  filter(!(specimen_number %in% specrm_4))
#1961 obs now



#There seems to be no easy, automated way to remove further duplicates (very minor differences in multiple fields) --- Will have to manually check remaining potential duplicates (oh boy...)
#Mainly checking for near IDENTICAL LOCALITY STRINGS

#Same day collections in close vicinity considered separate when AT LEAST 1 MILE APART (change later?)
nemo_all_dupes4 <- get_dupes(nemo_all_5, date_new) %>% 
  select(specimen_number, source, lat, long, dupe_count, locality, georef_by,everything())



specrm_5 <- c("NY337175", "CAS-BOT-BC7218", "POM65303", "POM65254", "CAS-DS-135678", "POM65288", "UC131446", "POM179734", "RSA0003801", "POM65313", "SEINET5521029", "POM65358", "POM73114", "CAS-DS-24652", "POM73962", "POM126468", "POM14755", "POM3472", "JEPS1880", "RSA75866", "POM97049", "POM97043", "POM96520", "RSA421364", "DAV300339", "RSA421338", "HSC13595", "POM184247", "POM182246", "POM231318", "OBI43248", "SBBG6143", "RSA421354", "RSA523374", "SD38532", "RSA604728", "SD42585", "POM248968", "RSA75885", "POM249032", "POM249022", "POM248998", "RSA75892", "RSA75891", "POM248973", "RSA75890", "RSA796428", "SBBG6137", "UCD120231", "SBBG117353", "POM248105", "RSA24986", "SDSU6124", "SEINET3091664", "UCSB030406", "RSA42344", "UCD120239", "RSA49705", "HSC13590", "DAV300347", "UCR0077004", "UCD32468", "OBI13914", "UCR0077209", "UCD120244", "SBBG82621", "RSA127738", "DAV300365", "HSC13581", "UCD120212", "SFV112661", "SFV112676", "HSC13602", "OBI13917", "HSC13591", "HSC13594", "RSA201829", "OBI40889", "OBI56258", "SFV112655", "CSUSB120", "CSUSB122", "CSUSB119", "OBI13915", "CSUSB117", "OBI19531", "SD215562", "RSA615510", "OBI6323", "RSA609597", "OBI3000", "HSC23559", "UCD40102", "RSA226948", "HSC65237", "HSC27545", "HSC32847", "UCR16823", "HSC32387", "CSUSB123", "HSC35261", "HSC34960", "HSC37991", "UCD120083", "HSC71063", "OBI77727", "OBI39146", "DAV300329", "HSC48300", "SBBG54841", "CSUSB2020", "HSC51156", "DAV300348", "HSC87901", "HSC87900", "HSC73815", "CHSC52497", "HSC77392", "RSA648209", "RSA656034", "HSC89473", "OBI46188", "UCD120241", "RSA0037196", "RSA562471", "SFV112657", "RSA666506", "UCR0077156", "UCD120243", "UCD120253", "UCD120238", "RSA575133", "RSA574968", "RSA628204", "HSC97903", "RSA718849", "SFV16101", "RSA721602", "RSA586935", "RSA596954", "RSA605075", "RSA620357", "RSA613472", "RSA598872", "OBI54498", "RSA613194", "UCD166683", "RSA633299", "RSA729931", "RSA729927", "RSA655478", "RSA729397", "RSA729936", "RSA715310", "RSA701641", "RSA733445", "SEINET7048075", "SBBG121241", "UCR122527", "RSA674245", "RSA682600", "RSA0105076", "IRVC26484", "RSA701528", "RSA717582", "RSA703103", "BSCA0944", "HSC96266", "RSA733241", "IRVC29323", "DAV300402", "UCR0077210", "RSA733465", "DAV300403", "HSC97503", "RSA747361", "UCD78281", "DAV300375", "RSA748852", "RSA0093734", "RSA0073978", "RSA769524", "CAS-BOT-BC224919", "RSA794025", "RSA798328", "RSA802257", "SD244363", "CHSC116618", "JEPS111831", "SD261570", "RSA0128925", "POM347131", "UCSB030079") 

#CCH2 Records Removed [3]: CAS-DS-135678, RSA0003801, DAV300339, UCSB030406, DAV300347, UCR0077004, UCR0077209, DAV300365, SFV112661, SFV112676, SFV112655, DAV300329, DAV300348, SFV112657, UCR0077156, BSCA0944, DAV300402, UCR0077210, DAV300375, 	RSA0093734, RSA0073978, JEPS111831, UCSB030079


nemo_all_6 <- nemo_all_5 %>% 
  filter(!(specimen_number %in% specrm_5))
#193 dupes removed for total of 1768 obs!

#nemo_dupes_ttt <- get_dupes(nemo_all_6, lat, long) %>% #Hunting for more dupes
#  select(specimen_number, source, lat, long, dupe_count, locality, georef_by,everything())




write_csv(nemo_all_6, here::here("data_cleaning", "nemo_all_1.csv"))




