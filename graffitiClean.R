# import libraries

library("RSocrata")
library("dplyr")
library("lubridate")
library("tibbletime")
library("rusps")
library("XML")

# usps username
username <- "XXXXX"

# read data from Socrata API and select columns
url <- "https://data.cityofnewyork.us/resource/gpwd-npar.json?$SELECT=incident_address AS address, borough, created_date as date, status, resolution_action AS resolution, closed_date, zip_code, latitude, longitude"
graffiti <- read.socrata(url, app_token = NULL)

# change date formats
graffiti$date <- as_date(graffiti[,"date"], tz = NULL)
graffiti$closed_date <- as_date(graffiti[, "closed_date"], tz = NULL)

# fix zip_code using rusps and drop unresolvable addresses
graffiti$zip_code <- validate_address_usps(street = graffiti$address,
                                         city = graffiti$borough,
                                         state = 'NY',
                                         username = username)
graffiti <- graffiti %>% drop_na(zip_code)

# Finding which boroughs have the most incidents of graffiti
boroughCount <- graffiti %>% 
  group_by(borough) %>%
  summarise(count = n())


# Finding which months have the highest incidents of graffiti
monthlyCount <- graffiti %>%
  group_by(Month = month(date)) %>%
  summarise(count = n())

# Finding which zip codes have the highest incidents of graffiti
zipCount <- graffiti %>%
  group_by(zipcode = zip_code) %>%
  summarise(count = n())

zipCount <- zipCount %>% arrange(desc(count))