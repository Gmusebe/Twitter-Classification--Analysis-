# Twitter Analysis

# Install rtweet package:
install.packages('rtweet')

# Install multiple libraries:
install.packages(c('ggmap', 'maps'))

# Load the libraries:
library(RgoogleMaps)
library(tidyverse)
library(tidytext)
library(twitteR)
library(stringr)
library(ggplot2)
library(leaflet)
library(rtweet)
library(ggmap)
library(dplyr)
library(maps)
library(sf)



# Link Twitter Application and R.
# Store Keys and Tokens into variables:
API_Key <- "9JDmpjYlbTDelVDjAvC83eJBH"
API_Secret_Key <- "INN7ZWx5hFWVggLmNekXPNVhR9iNXnwhzGZvOwyAJG3Fr4rTsI"
Bearer_Token <- "AAAAAAAAAAAAAAAAAAAAAKnqQQEAAAAANvWhE74DwzQqTyusbSzvbZ1h60o%3DBbYAHh30S6GuUzoRs2GU6vJvwCwnKVmBWQUswgIk6cKTkEOE19"
Access_Token <- "1263742897154818049-dGT6nKdbX8XPCTtRcBIsDVOuKsrcJb"
Access_Token_Secret <-"Vko3wJIuEicPYv552eA7ocomjcxavlsxEiNj0YhMQejVx"

# Set Google API Key:
register_google(key = "AIzaSyDLgK2Ld_sClr5PZgBxJYhlfq_4nXCaVzc")


# Setup Authentication
setup_twitter_oauth(API_Key, API_Secret_Key, Access_Token, Access_Token_Secret)

# Search and extract crime data:
hashtags <- c('#rape', '#murder', '#killing', '#violence', '#robbery', 
              '#theft', '#criminal', '#fraud', '#police', '#crime', '#protests', 'protest',
              '#civilunrest', '#burglary', 'war', 'hatecrime', 'criminaljustice', '#murder',
              '#serialkiller', '#serialkillers', '#justice', '#death', '#terrorism', '#terrorist',
              '#terroristattack', '#violence', '#isis')

needle <- paste(hashtags, collapse = " OR ")

security_info <- search_tweets(needle,
                               n = 18000,
                               include_rts = FALSE,
                               geocode = lookup_coords("usa"))


# To export the tweets into a csv for gathering of data for a standard subscription:
write_as_csv(security_info, "security_info.csv")

# Extracting Tweet Geographic Coordinates
#The third source for geographic information is the geotagged precise location point 
# coordinates of where the tweet was tweeted.
# To extract the longitudes and latitudes of the tweet locations use the function lat_lng().
## create lat/lng variables using all available tweet and profile geo-location data
# The function creates two new columns in the data set, lat and lng, which represent the latitude and longitude coordinates, respectively.
# Not all tweets are geotagged. Let’s keep the tweets with lat/long info using the filter() command.

security_info_geo <- lat_lng(security_info) %>%
  filter(is.na(lat) == FALSE & is.na(lng) == FALSE)


security_info_geo.sf <- st_as_sf(security_info_geo , coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircles(data = security_info_geo.sf, 
             color = "blue")



## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(security_info, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# For locations

user_info <- lookup_users(unique(security_info$user_id))

discard(user_info$location, `==`, "") %>% 
  ggmap::geocode() -> coded

coded$location <- discard(user_info$location, `==`, "")

user_info <- left_join(user_info, coded, "location")


st_crs(la.metro) == st_crs(security_info_geo.sf)
