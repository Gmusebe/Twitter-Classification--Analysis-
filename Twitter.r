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
library(plyr)
library(maps)
library(sf)



# Link Twitter Application and R.
# Store Keys and Tokens into variables:
API_Key <- "xxx"
API_Secret_Key <- "xxxx"
Bearer_Token <- "xxxx"
Access_Token <- "xxxx"
Access_Token_Secret <-"xxxx"

# Set Google API Key:
register_google(key = "xxxx")


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

# Top 10 locations where security incidents were reported:
security_info %>%
  dplyr::count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Place",
       y = "Count",
       title = "Security Incident users - unique locations ")

# Sentimental Analysis
library(tm)

# Extract tweets from data:
security_tweets <- security_info$text

# Cleaning the tweets
corp <- Corpus(VectorSource(security_tweets))

# Custom functions
twitterHandleRemover <- function(x) gsub("@\\S+","", x)
hashtagRemover <- function(x) gsub("#\\S+","", x)
emojiRemover <- function(x) gsub("[^\x01-\x74F]","",x)
toSpace = content_transformer(function(x,pattern)gsub(pattern,"",x))

cleaner <- function(corp){
  corp <- tm_map(corp, toSpace," ?(f|ht)tp(s?)://(.*)[.][a-z]+")
  corp <- tm_map(corp, content_transformer(twitterHandleRemover))
  corp <- tm_map(corp, content_transformer(hashtagRemover))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, emojiRemover)
  corp <- tm_map(corp, stemDocument)
  corp <- tm_map(corp, content_transformer(tolower))
  return(corp)
}

corp <- cleaner(corp)

new_tweetsdf <- data.frame(text = sapply(corp, as.character), stringsAsFactors = FALSE)

#unlist list column 
new_tweetsdf <- unlist(new_tweetsdf)

# Check missing values
sum(!complete.cases(new_tweetsdf)) # No missing rows


#Sentimental Analysis Algorithm
# Download Libraries
install.packages(c("exploratory", "sentimentr", "devtools", "gofastr"))
devtools::install_github("trinker/termco")

# Load libraries
library(exploratory)
library(sentimentr)
library(devtools)
library(gofastr)
library(plotly)

sentiments_df <- sentiment_attributes(new_tweetsdf)
new_2 <- get_sentences(new_tweetsdf)
tweet_sentiment<-sentiment_by(new_2, averaging.function = average_weighted_mixed_sentiment)

  
# Make the graph
sentiment_graph = plot_ly(
  x=tweet_sentiment$word_count,y=tweet_sentiment$ave_sentiment,
  mode="markers",
  colors =c("red","yellow"),size=abs(tweet_sentiment$ave_sentiment)/3,
  color=ifelse(tweet_sentiment$ave_sentiment>0,"Positive","Negative")
) %>%
  #Change hover mode in the layout argument
  layout(
    hovermode="closest",title="Sentiment analysis by Tweet",
    xaxis= list(title = "Number of words per Tweet",size=18),
    yaxis = list(title = "Sentiments by Tweet",size=18)
  )

# show the graph
sentiment_graph

# Sentiment Calculation
negative <- tweet_sentiment[tweet_sentiment$ave_sentiment < 0]
positive<- tweet_sentiment[tweet_sentiment$ave_sentiment > 0]
neutral <- tweet_sentiment[tweet_sentiment$ave_sentiment == 0]

#Negative plots:
negative_sentiment_graph = plot_ly(
  x=negative$word_count,y=negative$ave_sentiment,
  mode="markers") %>%
  layout(
    hovermode="closest",title= "Negatives",
    xaxis= list(title = "Number of words per Tweet",size=18),
    yaxis = list(title = "Ave_Sentiments",size=18)
  )
  
negative_sentiment_graph

# Positive plots:
positive_sentiment_graph = plot_ly(
  x=positive$word_count,y=positive$ave_sentiment,
  mode="markers") %>%
  layout(
    hovermode="closest",title= "Positives",
    xaxis= list(title = "Number of words per Tweet",size=18),
    yaxis = list(title = "Ave_Sentiments",size=18)
  )

positive_sentiment_graph

# Conditional Probability:
# Summary of data:
summary(tweet_sentiment$word_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   13.00   18.00   20.69   26.00   65.00

n <- nrow(tweet_sentiment)

#joint probability
neg_31 <- nrow(negative[negative$word_count > 31])/n
pos_31 <- nrow(positive[positive$word_count > 31])/n
neu_31 <- nrow(neutral[neutral$word_count > 31])/n

# Bayes theorem
p_neglong <- scales::percent(neg_31/(neg_31+pos_31+neu_31)) #scales::percent converts number to percent
p_poslong <- scales::percent(pos_31/(neg_31+pos_31+neu_31))
p_neulong <- scales::percent(neu_31/(neg_31+pos_31+neu_31))
prob<- data.frame(p_neglong,p_poslong,p_neulong)

# Renaming columns
colnames(prob)<- c('P(Neg|Long)','P(Pos|Long)','P(Neg|Long)')

# Device Access:
table_tweet <- data.frame(head(sort(table(security_info$source),decreasing=T),20))

ggplot(table_tweet, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Tweets by device/source", x="Device/Source",y="Frequency")+
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)


# Classification Model:
library(tm)
library(RTextTools)

# Attach the sentiment scores to the original data:
security_info$sent_score = tweet_sentiment$ave_sentiment

# Define a new variable called Negative.
security_info$Negative <- as.factor(security_info$sent_score <= -1)

# Factor Count:
table(security_info$Negative)

# Positive column:
security_info$Positive = as.factor(security_info$sent_score >= 1)

# Create a corpus from tweets:
security_tweets <- security_info$text
corp <- Corpus(VectorSource(security_tweets))

# Custom functions
twitterHandleRemover <- function(x) gsub("@\\S+","", x)
hashtagRemover <- function(x) gsub("#\\S+","", x)
emojiRemover <- function(x) gsub("[^\x01-\x74F]","",x)
toSpace = content_transformer(function(x,pattern)gsub(pattern,"",x))

cleaner <- function(corp){
  corp <- tm_map(corp, toSpace," ?(f|ht)tp(s?)://(.*)[.][a-z]+")
  corp <- tm_map(corp, content_transformer(twitterHandleRemover))
  corp <- tm_map(corp, content_transformer(hashtagRemover))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, emojiRemover)
  corp <- tm_map(corp, stemDocument)
  corp <- tm_map(corp, content_transformer(tolower))
  return(corp)
}

corp <- cleaner(corp)

# Stemming:
corp <- tm_map(corp, stemDocument)

# Craete DTM
dtm = DocumentTermMatrix(corp)

# Inspect dtm:
inspect(dtm[1000:1005, 505:515])

# Remove sparse terms:
sparse_dtm <- removeSparseTerms(dtm, 0.995)

# Convert the dtm to a data frame
dtm_df <- as.data.frame(as.matrix(sparse_dtm))


# Add the dependent variables:
dtm_df$Negative <- security_info$Negative 
dtm_df$Positive <- security_info$Positive

# Manual classification for training set:

library(caTools)
library(rpart)
library(rpart.plot)
set.seed(16102016)

#split data into training and testing data:
negative_split <- sample.split(dtm_df$Negative, SplitRatio = 0.7)

negative_train <- subset(dtm_df, negative_split == TRUE)
negative_test <- subset(dtm_df, negative_split == FALSE)


positive_split <- sample.split(dtm_df$Negative, SplitRatio = 0.7)

positive_train <- subset(dtm_df, positive_split == TRUE)
positive_test <- subset(dtm_df, positive_split == FALSE)


# Negative Predictive Sentiment model:
npm <- rpart(Negative ~ . , data = negative_train, method = "class")

# Positive Predictive Sentiment model:
ppm <- rpart(Positive ~ . , data = positive_train, method = "class")

# Predictions
# Negative:
predict_negative <- predict(npm, newdata = negative_test, type = "class")

# Negative Model Accuracy:
# Confusion Matrix
ng_cm <-  table(negative_test$Negative , predict_negative)
ng_cm

# Model Accuracy:
negative_predictive_acc <- (ng_ac[1,1] + ng_ac[2,2])/sum(ng_ac)
negative_predictive_acc

# Positive:
predict_positive <- predict(ppm, newdata = positive_test, type = "class")
pg_cm <-  table(positive_test$Positive , predict_positive)

# Model Accuracy:
positive_predictive_acc <- (pg_ac[1,1] + pg_ac[2,2])/sum(pg_ac)
positive_predictive_acc

# End