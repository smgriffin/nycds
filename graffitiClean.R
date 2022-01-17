# import libraries

library(RSocrata)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(lubridate)
library(mapsf)
library(rvest)
#library(rusps)
#library(XML)

# usps username
username <- ""

# read data from Socrata API and select columns
url <- "https://data.cityofnewyork.us/resource/gpwd-npar.json?$SELECT=incident_address AS address, borough, created_date as date, status, resolution_action AS resolution, closed_date, zip_code, latitude, longitude"
graffiti <- read.socrata(url, app_token = NULL)

# change date formats
graffiti$date <- as_date(graffiti[,"date"], tz = NULL)
graffiti$closed_date <- as_date(graffiti[, "closed_date"], tz = NULL)

# fix zip_code using rusps and drop unresolvable addresses
# graffiti$zip_code <- validate_address_usps(street = graffiti$address,
#                                          city = graffiti$borough,
#                                          state = 'NY',
#                                          username = username)
graffiti <- graffiti %>% drop_na(zip_code)

# Run this if any boroughs are unspecified
graffiti <- graffiti[!grepl('UNSPECIFIED', graffiti$borough),]

graffiti %>% 
  group_by(borough) %>%
  summarise(count = n())


# Finding which months have the highest incidents of graffiti
monthlyCount <- graffiti %>%
  group_by(month = month(date)) %>%
  summarise(count = n())

monthnames <- c("January", "February", "March", "April",
                "May", "June", "July", "August",
                "September", "October", "November", "December")

monthlyCount$month <- monthnames

# Finding which zip codes have the highest incidents of graffiti

# Get all nyc zipcodes from worldpopulationreview.com

#html <- read_html('https://worldpopulationreview.com/zips/new-york')

html <- read_html('https://www.nycbynatives.com/nyc_info/new_york_city_zip_codes.php')


ziplist <- html %>% 
          html_element('table') %>%
          html_table() %>%
          select('X1', 'X4')

ziplist <- ziplist %>%
           transform(X1 = as.character(X1), X4 = as.character(X4)) %>%
           gather(value = zipcode)
           
ziplist <- data.frame(ziplist[,-1])
ziplist <- ziplist %>% rename(zipcode = 1)

zipCount <- graffiti %>%
  group_by(zipcode = zip_code) %>%
  summarise(count = n())
           
zipCount <- full_join(ziplist, zipCount, by = 'zipcode') 

zipCount <- zipCount %>%
            replace_na(list(count = 0)) %>%
            arrange(desc(count))
  
zipTop <- zipCount %>% head(20)

# download shapfile (for map)
options(tigris_use_cache = TRUE)
geo <- st_as_sf(zctas(cb = TRUE, starts_with = zipCount$zipcode))
sf_use_s2(FALSE)

zipCount <- merge(geo, zipCount, by.x="ZCTA5CE10", by.y="zipcode")


# Map Plot
mf_map(x = zipCount, 
       var = "count",
       type = "choro",
       pal = "Burg",
       breaks = "quantile",
       border = "white",
       leg_title = "Reported Incidents",
       leg_val_rnd = 0,
       add = TRUE)

mf_title("Graffiti Reports in NYC since 1/30/21")
mf_credits("NYC Open Data / nycbynatives.com")


       


# How well are the boroughs resolving the problem?
graffitiStatus <- graffiti %>% group_by(borough) %>% count(status)
graffitiPivot <- graffitiStatus %>% 
                  pivot_wider(id_cols = borough, 
                  names_from = status, 
                  values_from = n) %>%
                  replace_na(list(Closed = 0))

# Add total and percent columns
graffitiPivot <- graffitiPivot %>% mutate(total = sum(Closed, Open), 
                                          percent = (Closed/total) * 100)

# Some graphs


