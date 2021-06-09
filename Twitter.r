# Twitter Analysis

# Install relevant libraries
install.packages(c("twitteR", "tidyverse", "tidytext"))

# Load the libraries:
library(rtweet)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggmap)
library(RgoogleMaps)
library(map)

# Link Twitter Application and R.
# Store Keys and Tokens into variables:
API_Key <- "9JDmpjYlbTDelVDjAvC83eJBH"
API_Secret_Key <- "INN7ZWx5hFWVggLmNekXPNVhR9iNXnwhzGZvOwyAJG3Fr4rTsI"
Bearer_Token <- "AAAAAAAAAAAAAAAAAAAAAKnqQQEAAAAANvWhE74DwzQqTyusbSzvbZ1h60o%3DBbYAHh30S6GuUzoRs2GU6vJvwCwnKVmBWQUswgIk6cKTkEOE19"
Access_Token <- "1263742897154818049-dGT6nKdbX8XPCTtRcBIsDVOuKsrcJb"
Access_Token_Secret <-"Vko3wJIuEicPYv552eA7ocomjcxavlsxEiNj0YhMQejVx"

# Setup Authentication
setup_twitter_oauth(API_Key, API_Secret_Key, Access_Token, Access_Token_Secret)

# Search and extract crime data:
hashtags <- c('#rape', '#murder', '#killing', '#violence', '#robbery', 
              '#theft', '#criminal', '#fraud', '#police', '#crime', '#protests')
needle <- paste(hashtags, collapse = " OR ")

security_info <- search_tweets(needle,
                               n = 500,
                               include_rts = FALSE,
                               geocode = lookup_coords("usa"))

## create lat/lng variables using all available tweet and profile geo-location data
security_info <- lat_lng(security_info)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(security_info, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# For locations
register_google(key = "AIzaSyDLgK2Ld_sClr5PZgBxJYhlfq_4nXCaVzc")
user_info <- lookup_users(unique(security_info$user_id))

discard(user_info$location, `==`, "") %>% 
  ggmap::geocode() -> coded

coded$location <- discard(user_info$location, `==`, "")

user_info <- left_join(user_info, coded, "location")

