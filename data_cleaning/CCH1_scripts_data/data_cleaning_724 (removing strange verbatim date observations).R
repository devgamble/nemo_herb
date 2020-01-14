library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(here)

here::here()
nemo <- readr::read_delim("cchdata_email_724.txt", delim = "\t", quote = "")
dim(nemo)
View(nemo)

# data cleaning
# str_which allows us to find strings in a specific column that have a specific pattern 
# in the cases in this script, we are finding strings inside the "verbatim date" column that have a specific pattern  
# R outputs indexes (row numbers) of which observations have the occurence we are searching for
# to search for info on a specific index or row #, we could do nemo[["verbatim_date"]][420] or nemo[["verbatim_date"]][c(420, 1)] (ex is searching for row 420)

# before we begin making changes, here is an example where we would use the str_which function in R
stringr::str_which(nemo[["verbatim_date"]], "--")
# many verbatim_date observations in this dataset are not properly formatted and cannot be recognized as a date in R
# in the example above, we are searching for instances when the "verbatim date" has two dashes instead of 1

# gsub allows us to tell R to find a certain specified pattern in the dataset, and replace occurences with something we tell it to
# in this case we are taking the nemo dataset, "verbatim date" column, and replacing all instances of the pattern "--" with a space
nemo[["verbatim_date"]] <- gsub(pattern = "--", replacement = "", nemo[["verbatim_date"]])
# we can re-run this line of code to check (called a sanity check) if we successfully made the replacement
stringr::str_which(nemo[["verbatim_date"]], "--")

# there were cases where the verbatim date begins with a "-" ex: -Jan-01-1901
# [] around dash says to look for only the dashes before, everytime see a slash at front of the string then only remove those dashes
str_which(nemo[["verbatim_date"]], "^[-]")
# here we will replace the dashes that occur at the beginning of a verbatim_date with a space. REPLACE - SPECIFYING ONLY THE ONES AT BEGINNING OF DATE
nemo[["verbatim_date"]] <- gsub(pattern = "^[-]", replacement = "", nemo[["verbatim_date"]])
# sanity check
str_which(nemo[["verbatim_date"]], "^[-]")


# start data cleaning on nemo dataset!

# Remove rows where verbatim_date is "NA"
na_indices <- which(is.na(nemo[["verbatim_date"]]))
nemo <- nemo[-na_indices, ]
dim(nemo)
# went from 2000 -> 1960 when removing NAs


# which are the cases where the verbatim_date is formatted as ""Month, Year"" with quotation marks
# to identify quotation marks in R, we use forward slash then quoatation (/")
str_which(nemo[["verbatim_date"]], "\"")
# now we want to remove the quatoation marks, NOT removing cases where there are quotation marks, just the quotation marks themselves
nemo[["verbatim_date"]] <- gsub(pattern = "\"", replacement = "", nemo[["verbatim_date"]])
# then sanity check
str_which(nemo[["verbatim_date"]], "\"")


# Check dimensions
dim(nemo)
# went from 2000 observations to 1960 when removing all cases of NA


# Which rows have "verbatim date" formatted as date ranges such as May 17 1901 - June 01 1901
# to search for this, we can ask which verbatim date observations have a string greater than 12 characters
which(nchar(nemo[["verbatim_date"]]) > 12 )
# to remove rows where there are date ranges as the verbatim_date, tell R to call those occurences "date_range_indices"
# nchar takes a character vector as an argument and returns a vector whose elements contain the sizes of the corresponding elements of x
# i.e. which values in the column have more than 12 values
# ex: through dates/ date range
date_ranges_indices <- which(nchar(nemo[["verbatim_date"]]) > 12 )
# length(date_range_indices) will tell us how many of those occurences there are in the dataset 
length(date_ranges_indices)
# there are 13 occurences where verbatim_date is formatted as a date range


# exclude date_ranges_indices in the new dataset
nemo <- nemo[- date_ranges_indices, ]
# Check dimensions
dim(nemo)
# the dataset went from 1960 -> 1947 when removing date range indices
#sanity check
nemo[["verbatim_date"]][which(nchar(nemo[["verbatim_date"]]) > 12 )] 


# which verbatim_dates are formatted as "Season Year" or "Month Year" aka has no day 
# a tough one, but regex (regular expressions) allows us to search for very specific patterns of letters and numbers
# which observations of verbatim_date begin with a string of more than 2 characters of letter a-z (capital or lower case), are followed by a space, and end with a string of more than 1 character of numbers
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}$")
# call these instances "no_day_indices"
no_day_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}$")
# output nemo dataset without the no_day-indices
nemo <- nemo[-no_day_indices, ]
#check dimensions
dim(nemo)
# went from 2000 -> 1960 when removing NAs, went from 1960 -> 1947 when removing date ranges 
# went from 1947 -> 1928 when removing cases where there is no day!
# sanity check
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}$")] 
# look for all strings that begin with a-z both lower and upper case and are greater than two (that's ^[a-zA-Z]{2,})
# then looks for space (thats [])
# and then looks for all strings that begin with 0-9 and are greater than 1 (that's [0-9{1,}])
# examples of no_day_indices: "apr 1898", "Spring 1930" or "before 1897"
# note: the {} represents the ranges so if its {1,} it means 1 or more. {2,4} means between 2 and 4 values


# look for all strings that begin with 0-9 and are greater than 3, then followed by a dash, and string 0-9 that is greater than 1
# ex of what we are seaching for: "1901-03"
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[-]*[0-9]{1,}$")]
# there are 44 insstances
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[-]*[0-9]{1,}$")]
# remove the observations where verbatim date is just a year or a year and month, no day
year_no_day_indices <- str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[-]*[0-9]{1,}$")
nemo <- nemo[-year_no_day_indices, ]
# sanity check
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[-]*[0-9]{1,}$")]  
dim(nemo)
# went from 1928 -> 1884 when removing cases where the verbatim_date is a year or a year and a month only (no day)


# look for the cases where the verbatim_date is a 4-digit year with a "s" at the end (i.e. 1800s or 1900s)
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[s]$")]
# remove cases where the verbatim_date is a 4-digit year with an "s"
ends_w_s_indices <- str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[s]$")
nemo <- nemo[-ends_w_s_indices, ]
# sanity check
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{3,}[s]$")] 
# looks for strings that begin with 0-9 and are greater than 3, followed by a "s" (i.e. 1900s)
dim(nemo)
# went from 1884 -> 1879 when removing cases where verbatim)date is a 4-digit year with an "s"


# look for cases where verbatim_date is a "Month, Year" and has no day
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[,][ ]*[0-9]{1,}$")]

# remove cases where verbatim_date is a "Month, Year" and has no day
has_comma_no_day_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[,][ ]*[0-9]{1,}$")
nemo <- nemo[-has_comma_no_day_indices, ]
#sanity check 
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[,][ ]*[0-9]{1,}$") 
dim(nemo)
#went from 1879 -> 1861 when removing cases where verbatim_date is "Month, Year"


# look for cases where verbatim_date is a "Mon. Year"
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][ ]*[0-9]{1,}$")]
# remove cases where verbatim_date is "Mon. Year"
month_w_period_year_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][ ]*[0-9]{1,}$")
nemo <- nemo[-month_w_period_year_indices, ]
#sanity 
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][ ]*[0-9]{1,}$") 
dim(nemo)
# went from 1861 -> 1854 when removing cases where verbatim_date is "Mon. Year"


# Apr. 1, 1923 need to be changed, could we make those an array, then choose to replace the commas and periods within that array
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][ ]*[0-9]{1,}[,][ ][0-9]{2,}$")]


#look for cases where verbatim_date is a Month, day, no year
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}[a-zA-Z]{1,}$")]
# is just one case, March 3rd
# remove March 3rd case
march_3rd_index <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}[a-zA-Z]{1,}$")
nemo <- nemo[-march_3rd_index, ]
# santiy check
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[ ]*[0-9]{1,}[a-zA-Z]{1,}$") 
dim(nemo)
# went from 1854 -> 1853 when removing case "March 3rd"


# look for cases where verbatim_date is a 3 letter month, followed by a 2 digit number, which could be either day or year but not both
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[-]*[0-9]{1,}$")]
# remove
month_dash_no_day_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[-]*[0-9]{1,}$")
nemo <- nemo[-month_dash_no_day_indices, ]
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[-]*[0-9]{1,}$") #sanity check
dim(nemo)
#went from 1853 -> 1831 when removing month-year or month-day


# look for cases where verbatim_date is "unknown"
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "unknown")]
unknown_indices <- str_which(nemo[["verbatim_date"]], "unknown")
nemo <- nemo[-unknown_indices, ]
str_which(nemo[["verbatim_date"]], "unknown") #sanity check 
# removed unknowns
dim(nemo)
# went from 1831 -> 1830 when removing the one case where verbatim_date was unknown

# look for cases where verbatim date is a 2-digit numerical month and 4-digit year
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{1,}[ ]*[0-9]{1,}$")]
month_space_year_indices <- str_which(nemo[["verbatim_date"]], "^[0-9]{1,}[ ]*[0-9]{1,}$")
# remove month with 4-digit year cases
nemo <- nemo[-month_space_year_indices, ]
str_which(nemo[["verbatim_date"]], "^[0-9]{1,}[ ]*[0-9]{1,}$") #sanity check
# went from 1830 -> 1829


# look for cases where verbatim date is a letter character month, followed by a period and comma, then year (i.e. Apr., 2020)
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][,][ ]*[0-9]{1,}$")]
month_period_comma_year_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][,][ ]*[0-9]{1,}$")
nemo <- nemo[- month_period_comma_year_indices, ]
# remove month with comma period year
str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{2,}[.][,][ ]*[0-9]{1,}$") #sanity check
dim(nemo)
# went from 1829 -> 1828


# look for cases where verbatim date is formatted as a single digit day-Mon-2 digit year (i.e. 6-Apr-66)
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{1}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")]
# we are calling them "weird_indices" because they are formatted in a way that R will not recognize as a date
weird_indices <- str_which(nemo[["verbatim_date"]], "^[0-9]{1}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")
length(weird_indices)
# there are 47 "weird indices"
# INSTEAD OF REMOVING INDICES, we are now interested in TRANSFORMING these indices so that they are formatted as a date
d <- lubridate::dmy(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{1}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")], 
                    format = "%d-%b-%y")
# all but one verbatim date were transformed into a day-month-year format
# 8-Apr-79 is the case that failed
length(d)
na_index_converted_dates <- which(is.na(d))
d <- d[-na_index_converted_dates]

new_format <- as.Date(ifelse(d > "2016-01-01", 
               format(d, format = "19%y-%m-%d"),
               format(d)))

nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = weird_indices, values = as.character(new_format))
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{1}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")]


# look for cases where verbatim date is formatted as 2-digit day-month-2-digit year
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{2}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")]
# call them "weird_indices_long_day" (as opposed to the weird indices before that were single digit days)
weird_indices_long_day <- str_which(nemo[["verbatim_date"]], "^[0-9]{2}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")
length(weird_indices_long_day)
# there are 311 !

# change format of "weird_indices_long_day"
e <- lubridate::dmy(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{2}[-]*[a-zA-Z]{1,}[-]*[0-9]{2}$")], 
                    format = "%d-%b-%y")
# 8-Apr-79 failed to parse
length(e)
na_index_converted_dates_long_day <- which(is.na(e))
e <- e[-na_index_converted_dates_long_day]

new_format_long_day <- as.Date(ifelse(e > "2016-01-01", 
                             format(e, format = "19%y-%m-%d"),
                             format(e)))

nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = weird_indices_long_day, values = as.character(new_format_long_day))
dim(nemo)


nemo <- nemo[- str_which("1903-6-\\N", nemo[["verbatim_date"]]), ]


# REMOVING COMMAS AND PERIODS 
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], ',')] <- str_replace(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], ',')], ',', '')

nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], '[.]')] <- str_replace(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], '[.]')], '[.]', '')

# FINAL STUFFFFFFFFFFF
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{3,}[ ]*[0-9]{1,}[ ]*[0-9]{4}$")]

includes_names_of_months_indices <- str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{3,}[ ]*[0-9]{1,}[ ]*[0-9]{4}$")

converted_dates <- lubridate::mdy(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[a-zA-Z]{3,}[ ]*[0-9]{1,}[ ]*[0-9]{4}$")], 
               format = "%b %d %Y")

right_dates_indices <- which(is.na(converted_dates))
converted_dates <- as.character(converted_dates[-right_dates_indices])

nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = includes_names_of_months_indices, values = converted_dates)
# END 

str_which(nemo[["verbatim_date"]], "^[0-9]{4}[-]*[a-zA-Z]{2,}[-]*[0-9]{1,}$")
includes_year_name_diff_format <- str_which(nemo[["verbatim_date"]], "^[0-9]{4}[-]*[a-zA-Z]{2,}[-]*[0-9]{1,}$")

converted_dates_diff_format <- lubridate::ymd(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^[0-9]{4}[-]*[a-zA-Z]{2,}[-]*[0-9]{1,}$")], 
                                  format = "%Y-%b-%d")

right_dates_diff_format_na <- which(is.na(converted_dates_diff_format))
converted_dates_diff_format <- converted_dates_diff_format[-right_dates_diff_format_na]
converted_dates_diff_format <- format(converted_dates_diff_format, format = "%m/%d/%Y")
nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = includes_year_name_diff_format, values = as.character(converted_dates_diff_format))


## FDKSJLSDKFHJ
last_format_change_indices <- str_which(nemo[["verbatim_date"]], "^[0-9]{4}[-]*[0-9]{2}[-]*[0-9]{2}$")

new_converted_to_right_format <- as.character(format(as.Date(nemo[["verbatim_date"]][last_format_change_indices], format = "%Y-%m-%d"), "%m/%d/%Y"))

nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = last_format_change_indices, values = new_converted_to_right_format)

# END
nemo <- nemo[- str_which(nemo[["verbatim_date"]], "1970-Mar-"), ] 

# NEW
remove_leading_zero_indices <- str_which(nemo[["verbatim_date"]], "^0")
nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^0")]

remove_zeroes_convert_date <- str_replace(nemo[["verbatim_date"]][str_which(nemo[["verbatim_date"]], "^0")], "^0", "")



nemo[["verbatim_date"]] <- replace(nemo[["verbatim_date"]], list = remove_leading_zero_indices, values = remove_zeroes_convert_date)

# FINAL FORREAL 
nemo <- nemo[-47, ]

nemo[["verbatim_date"]] <- as.Date(nemo[["verbatim_date"]], format = "%m/%d/%Y")
dim(nemo )
dim(nemo %>%
  filter((verbatim_date > as.Date("1901-01-01")) & 
           (verbatim_date < as.Date("2015-12-31"))))

# Find Day of Year

#just for Susan
write.csv(nemo, file = "nemo.csv")


nemo %>%
  mutate(doy = lubridate::yday(verbatim_date)) %>%
  select(doy)

View(nemo)


nemo_with_doy <- nemo %>%
  mutate(doy = lubridate::yday(verbatim_date)) %>%
  select(doy)


nemo <- nemo %>%
  mutate(doy = lubridate::yday(verbatim_date)) 



rm(nemo_with_doy)







#find all where column = x (ex: verbatim_date = 1901 then export it out as output_1901)

write.csv(nemo, file = "nemo_with_doy.csv")

#merge datasets key fields 

#innerjoin- keep things that are present in both #default merge(df1, df2, by = "ID field")


#order of operations
#30 year normals on dataset
#one file each year for annual data
#pull climate data for that year, each time thru climate NA
#concatenate annual fields vertically  #rbind(a,b) stacks datasets
