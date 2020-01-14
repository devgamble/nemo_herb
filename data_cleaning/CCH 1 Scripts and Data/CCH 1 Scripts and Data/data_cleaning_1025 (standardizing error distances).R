setwd("C:/Users/Ivana/Desktop/NemophilaProject/chh_datacleaning")
library(tidyverse)
nemo_with_doy011320 <- readr::read_csv("nemo_with_doy.csv")
View(nemo_with_doy011320)
dim(nemo_with_doy011320)
# 1824 observations 

# how many observations of latitude are NA?
length(which(is.na(nemo_with_doy011320[["latitude"]])))
# 489 cases where lat is NA
# how many observations of longitude are NA?
length(which(is.na(nemo_with_doy011320[["longitude"]])))
# 489 cases where long is NA

# HERE I removed any cases where latitude (and therefore longitude) are NA,
# HOWEVER, we may choose to georeference these cases in the future
nemo_nonlatitude_removed <- nemo_with_doy011320[- which(is.na(nemo_with_doy011320[["latitude"]])), ]
View(nemo_nonlatitude_removed)
dim(nemo_nonlatitude_removed)
# The dataset went from 1824 -> 1335 observations when removing cases where there are no latitude or longitude values
# take our new data "nemo_nonlatitude_removed" and then show me a table of the type of units used for error distance and the amount of observations using each type of units
nemo_nonlatitude_removed %>%
   dplyr::count(units)
# i.e. there are 10 cases where the error distance units are kilometers

# Km to miles = 1.60934, we will standardize them to all be kilometers

# there are two columns we are working with for error distance, 
# one is the actual numerical error distance called "error_distance", and the other is the units that the error distance is measured in called "units"
# in some cases, the error_distance is a strange string of letters (i.e. null, not recorded, $ltd, Lat: 1000; Long: 1000)
# IF WE WANT TO REMOVE THEM, WE CAN WITH THE CODE BELOW, BUT I WILL LEAVE THEM FOR NOW (JAN. 13 2020) BECAUSE WE CAN JUST CALL THEM NA,
# AND GROUP THEM WITH THE OTHER OBSERVATIONS WHERE ERR DIST IS NA
nemo_remove_letters_error_dist <- nemo_nonlatitude_removed[- stringr::str_which(nemo_nonlatitude_removed[["error_distance"]], "[a-zA-Z]"), ]
View(nemo_remove_letters_error_dist)

# USE ONLY WHEN REMOVING THE STRING OF LETTER ERR DISTANCES, NOT APPLICABLE
nemo_remove_letters_error_dist[["units"]][nemo_remove_letters_error_dist[["units"]] %in% c("mi", "Mi", "mi.", "miles") ] <- "miles"
nemo_remove_letters_error_dist[["units"]][nemo_remove_letters_error_dist[["units"]] %in% c("m", "meters") ] <- "meters"

nemo_convert_to_km <- nemo_remove_letters_error_dist %>%
  dplyr::mutate(error_distance = as.numeric(error_distance)) %>%
  dplyr::mutate(new_error_distance = ifelse(units == "miles", error_distance * 1.60934, error_distance)) %>%
    dplyr::mutate(new_error_distance = ifelse(units == "meters", new_error_distance * 0.001, new_error_distance)) 

# JAN 13, 2020, WHERE WE KEEP THE STRING OF LETTERS ERR DISTANCES
# take our updated dataset (where observations with no lat and long were removed) 
# in the "units" column, take the cases were "units" are mi, Mi, mi., or miles, and change them all to be "miles"
nemo_nonlatitude_removed[["units"]][nemo_nonlatitude_removed[["units"]] %in% c("mi", "Mi", "mi.", "miles") ] <- "miles"
# take the cases where "units" are m or meters and change them to all be "meters"
nemo_nonlatitude_removed[["units"]][nemo_nonlatitude_removed[["units"]] %in% c("m", "meters") ] <- "meters"

# convert all to kilometers by using the conversion amount for miles to kilometers and meters to kilometers, creates a new column "new_error_distance" 
# all values in column "new_error_distance" are in units of km
nemo_convert_to_km <- nemo_nonlatitude_removed %>%
  dplyr::mutate(error_distance = as.numeric(error_distance)) %>%
  dplyr::mutate(new_error_distance = ifelse(units == "miles", error_distance * 1.60934, error_distance)) %>%
  dplyr::mutate(new_error_distance = ifelse(units == "meters", new_error_distance * 0.001, new_error_distance)) 

View(nemo_convert_to_km)

nemo_error_dist_one_km <- nemo_convert_to_km %>%
  filter(new_error_distance <=  1)

nemo_error_dist_two_km <- nemo_convert_to_km %>%
  filter(new_error_distance <=  2)

nemo_error_dist_three_km <- nemo_convert_to_km %>%
  filter(new_error_distance <=  3)

nemo_error_dist_four_km <- nemo_convert_to_km %>%
  filter(new_error_distance <=  4)

nemo_error_dist_five_km <- nemo_convert_to_km %>%
  filter(new_error_distance <=  5)

nemo_convert_to_km %>%
  filter(new_error_distance <=  5) %>%
  dim()

View(nemo_convert_to_km)
nemo_convert_to_km %>%
  dplyr::count(units)

#nemo_convert_to_km$error_distance <- NULL
#nemo_convert_to_km$units <- "km"
nemo_convert_to_km %>%
  dplyr::mutate(error_distance = as.numeric(error_distance)) %>%
  dplyr::mutate(error_distance = error_distance * 1.60934)

write.csv(nemo_convert_to_km, file = "nemo_standardized_err_dist.csv",row.names=FALSE)

write.csv(nemo_error_dist_one_km, file = "Nemo_err_one.csv",row.names=FALSE)

write.csv(nemo_error_dist_two_km, file = "Nemo_err_two.csv",row.names=FALSE)

write.csv(nemo_error_dist_three_km, file = "Nemo_err_three.csv",row.names=FALSE)

write.csv(nemo_error_dist_four_km, file = "Nemo_err_four.csv",row.names=FALSE)

write.csv(nemo_error_dist_five_km, file = "Nemo_err_five.csv",row.names=FALSE)

# This dataset includes climatic variables, but we will start over in retrieving this information. Most updated CCH 1 data has 1335 observations and
# includes doy and new_error_distance in units of "km"
Nemophila_CCH_Gomez_Mazer <- readr::read_csv("Nemophila_CCH_Gomez_Mazer.csv")
View(Nemophila_CCH_Gomez_Mazer)

str(Nemophila_CCH_Gomez_Mazer$verbatim_date)
#being read as a string, R doesn't know want it to be date data type 

lubridate::as_date(Nemophila_CCH_Gomez_Mazer$verbatim_date)

Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date <- lubridate::as_date(Nemophila_CCH_Gomez_Mazer$verbatim_date)

lubridate::year(Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date)

Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date
Nemophila_CCH_Gomez_Mazer$verbatim_date

lubridate::as_datetime(Nemophila_CCH_Gomez_Mazer$verbatim_date, format = "%m/%d/%Y")

Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date <- lubridate::as_date(Nemophila_CCH_Gomez_Mazer$verbatim_date)

Nemophila_CCH_Gomez_Mazer$verbatim_date

lubridate::as_datetime(Nemophila_CCH_Gomez_Mazer$verbatim_date)

Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date <- lubridate::as_datetime(Nemophila_CCH_Gomez_Mazer$verbatim_date, format = "%m/%d/%Y")

lubridate::year(Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date)

Nemophila_CCH_Gomez_Mazer$year_extract <- lubridate::year(Nemophila_CCH_Gomez_Mazer$fixed_verbatim_date)

Nemophila_CCH_Gomez_Mazer$year_extract
