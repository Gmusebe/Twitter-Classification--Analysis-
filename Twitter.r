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
                               n = 50000,
                               include_rts = FALSE,
                               geocode = lookup_coords("usa"),
                               retryonratelimit = TRUE)

# Unique users
length(unique(security_info$user_id))
# [1] 21684

# Create a word cloud
library(wordcloud)
library(RColorBrewer)

#plot occurrences of each keyword in hashtags
hashtags <-data.frame(table(unlist(security_info$hashtags)))

#remove hashtags that appear less than 4 times
hashtags <- hashtags[which(hashtags$Freq > 4),] 

#remove obvious and unrelated hashtags
related_hashtags <- c("abuse", "AlQaeda", "arrests", "assault", "bomb", "cannabis", "ChildSexTrafficking", 
                      "CIA", "CivilWar", "ClimateCrisis", "coldwar", "ColdWar", "corruption", "crime", "Crime",
                      "CrimesAgainstHumanity", "CrimeStoppers", "criminal", "Criminal", "CRIMINAL",
                      "criminaljustice", "CriminalJustice", "CultureWar", "cyberattacks", "cybercrime", "CyberCrime",
                      "cybersecurity", "CyberSecurity", "cyberthreats", "danger", "Death", "death", "dead","DrugCartel", "drugs",
                      "Embezzlement", "EndProxyWar", "EndSARS", "FBI", "Federal", "federal", "Fire", "FIRE", "fire",
                      "FRAUD", "guns", "hatecrime", "HumanTrafficking", "insurancefraud", "ISIS", "justice",
                      "Justice", "killed", "murder", "Murder", "PoliceBrutality", "protest", "Protest", "protests", "Protests",
                      "racism", "racist", "Rape", "rape", "robbery", "SerialKiller", 	
                      "serialKiller", "serialKillers", "shooting", "Shooting", "terrorism", "Terrorism", "terrorist",
                      "Terrorist")


hashtags = hashtags[which(hashtags$Var1 %in% related_hashtags),]

# Decapitalize all characters to lower:
hashtags <- hashtags %>% 
  mutate(Var1 = tolower(Var1)) %>% ddply("Var1", numcolwise(sum))

# The WordCloud
wordcloud(word = hashtags$Var1, freq = hashtags$Freq, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


#Geographical Analysis of Tweets
# table of the 10 red zone areas:
security_info %>% 
  filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
  dplyr::count(place_full_name, sort = TRUE) %>% 
  slice(1:10)

# Visualize:
security_info %>%
  dplyr::count(place_full_name, sort = TRUE) %>%
  mutate(location = reorder(place_full_name,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Place",
       y = "Count",
       title = "Security Incidents -Locations ")


# Extracting Tweet Geographic Coordinates
security_info <- lat_lng(security_info)

security_info_geo <- lat_lng(security_info) %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)


# Mapping Tweets
security_info_geo.sf <- st_as_sf(security_info_geo , coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircles(data = security_info_geo.sf, 
             color = "blue")


# End