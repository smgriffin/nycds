# import libraries

library("RSocrata")
library("dplyr")
library("lubridate")
library("tibbletime")

# read data from Socrata API and select columns
url <- "https://data.cityofnewyork.us/resource/gpwd-npar.json?$SELECT=incident_address AS address, borough, created_date as date, status, resolution_action AS resolution, closed_date, zip_code, latitude, longitude"
graffiti <- read.socrata(url, app_token = NULL)

# change date formats
graffiti$date <- as_date(graffiti[,"date"], tz = NULL)
graffiti$closed_date <- as_date(graffiti[, "closed_date"], tz = NULL)

# Finding which boroughs have the most incidents of graffiti
boroughCount <- graffiti %>% 
  group_by(borough) %>%
  summarise(count = n())

# Finding which months have the highest incidents of graffiti
monthlyCount <- graffiti %>%
  group_by(Month = month(date)) %>%
  summarise(Count = n())
