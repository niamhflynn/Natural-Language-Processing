#################################################################################
#READ IN AND SET UP VARIABLES
#################################################################################
#SET WORKING DIRECTORY TO FOLDER WHERE DATA STORED
setwd("C:/Users/Niamh/OneDrive/Stats3")

#ALTER NAME OF 'US_TWEET_DAT' TO CHOSEN DATASET
tweets = read.csv("US-tweet-dat.csv", na.strings=c ("", "NA"), stringsAsFactors = F, header = T)

#DROP IRRELEVANT COLUMNS, SUCH AS TWEET_COORD, TWEET_LOCATION AND USER_LOCATION
#TO REMOVE ANOTHER COLUMN, REPLACE ONE OF THE NAMES BELOW WITH YOUR SELECTED COLUMN NAME
#OR ADD , 'COLUMN_TO_DELETE' 
tweets = tweets[, !names(tweets) %in% c('tweet_coord', 'tweet_location', 'user_location', 'user_timezone')]

#SET FACTORS
tweets$airline_sentiment = as.factor(tweets$airline_sentiment)
tweets$negativereason = as.factor(tweets$negativereason)
tweets$airline = as.factor(tweets$airline)

#CREATE NEW COLUMN TO STORE SENTIMENT SCORES FROM [-1,1]
tweets = transform(tweets, sentiment = factor(tweets$airline_sentiment, 
                                              levels = c("negative", "neutral", "positive"),
                                              labels = c(-1, 0, 1)))
#GET DATE, DAY AND HOUR EACH TWEET WAS CREATED AND STORE IN TWEET DATAFRAME
tweets$date = strftime(tweets$tweet_created, format = "%Y-%m-%d")
tweets$day = weekdays(as.Date(tweets$date))
tweets$hours = strftime(tweets$tweet_created,'%H')

#REMOVE TWEET_CREATED, AS WE HAVE EXTRACTED NECESSARY ELEMENTS
tweets = tweets[, !names(tweets) %in% c('tweet_created')]

#CREATE SENTIMENT COLUMN TO STORE TWEETS AS -1 [NEGATIVE], 0 [NEUTRAL] OR +1 [POSITIVE] 
tweets$sentiment = as.integer(tweets$sentiment)-2

#CREATE POWER SCORE- COMBINES NUMBER OF RETWEETS AND CONFIDENCE IN SENTIMENT
#APPLIED SENTIMENT = CERTAINTY OF SENTIMENT ALLOCATED, = SENTIMENT*CONFIDENCE
tweets$applied_sentiment = tweets$sentiment * tweets$airline_sentiment_confidence

#POWER SCORE = APPLIED SCORE*NUMBER OF RETWEETS(+1)- IS A MESAURE OF IMPACT OF TWEET
#ADD ONE TO NUMBER OF RETWEETS AS PERSON WHO TWEETED SHARES SAME SENTIMENT
tweets$power_score = tweets$applied_sentiment*(tweets$retweet_count+1)

#################################################################################
#CREATE SUBSETS
#################################################################################
#DATA FRAME WITH ONLY NEGATIVE REVIEWS WITH RELEVANT REASONS
neg_reviews = subset(tweets, tweets$airline_sentiment=="negative")
neg_reviews = na.omit(neg_reviews)
neg_reviews =subset(neg_reviews, negativereason != "Can't Tell")
neg_reviews$negativereason = factor(neg_reviews$negativereason)

levels(neg_reviews$negativereason)
#DATA FRAMES CONTAIING POS/NEUTRAL STATEMENTS ONLY
pos_reviews = subset(tweets, tweets$airline_sentiment == "positive")
neutral_reviews = subset(tweets, tweets$airline_sentiment == "neutral")

#################################################################################
#CREATE MATRIX OF NEGATIVE REASON CONFIDENCE FOR EACH AIRLINE
#################################################################################
install.packages("plyr")
library("plyr")
neg_reasons_confidence = count(neg_reviews, 'negativereason', 'negativereason_confidence')
neg_reasons = count(neg_reviews, 'negativereason')
#GET NEGATIVE REASON BREAKDOWN FOR EACH AIRLINE
airline_neg_scores = matrix(nrow = length(levels(neg_reviews$airline)), ncol = (length(levels(neg_reviews$negativereason))))
for(i in 1:length(levels(neg_reviews$airline))){
  current_airline = subset(neg_reviews, neg_reviews$airline == levels(neg_reviews$airline)[i])
  cur_reason = count(current_airline, 'negativereason', 'negativereason_confidence')
  airline_neg_scores[i,] = cur_reason$freq
}
colnames(airline_neg_scores) = levels(neg_reviews$negativereason)
rownames(airline_neg_scores) = levels(neg_reviews$airline)
airline_neg_scores

#################################################################################
#CREATE PACK CIRCLE 
################################################################################## 
install.packages("packcircles")
install.packages("ggplot2")
library(packcircles)
library(ggplot2)

#TRANSPOSE ABOVE, CREATE NEW DATA FRAME TO STORE DATA
data = as.data.frame(t(airline_neg_scores))
#DATA$AMERICAN GIVES BREAKDOWN FOR AMERICAN AIRLINES, ALTER FOR REQUIRED AIRLINE
d = data$American

#CREATE TOTAL FOR PERCENTAGES
total = sum(d)

#VECTORS STORING NEGATIVE REVIEWS AND AIRLINE NAMES
airlines = neg_reviews$airline
sent = neg_reviews$negativereason[1:length(levels(neg_reviews$negativereason))]
# CREATE DATA OF NEGATIVE REASONS FOR SPECIFIC AIRLINE
data=data.frame(group=paste(round(((d/total)*100),2), "%", levels(neg_reviews$negativereason)[1:length(levels(neg_reviews$negativereason))]), value=d) 

create_circle = function(data) {
# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$value, sizetype='area')

# We can add these packing information to the initial data frame
data = cbind(data, packing)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=value*40, label = group)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
}

#################################################################################
#WORDCLOUD
################################################################################## 
##Install Packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
##Load Require Library
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

##Read the Data
#remove airline name, @ symbols, and the word air
#THIS SUBSET WILL INCLUDE WORDS ASSOCIATED WITH CANCELLED/LATE FLIGHTS
#TO CHANGE THIS ALTER == "Cancelled Flight" TO DESIRED NAME
airline_names = as.vector(gsub(" ", "", levels(tweets$airline)))
word_sub = subset(neg_reviews, (neg_reviews$negativereason == "Cancelled Flight" | neg_reviews$negativereason == "Late Flight" ))
generate_wordcloud = function(word_sub){
tweetsDS.Corpus <- Corpus(VectorSource(word_sub))
cust_stop1 = paste("@", airline_names, sep = "")
cust_stop2 = paste("",paste("@", airline_names, sep=""), "air", sep="")
cust_stop3 = paste("", airline_names, sep = "")
cust_stop4 = paste("",paste("", airline_names, sep=""), "air", sep="")


#examining the world plot found usairways was included, removing it
#other words you would like to delete can be entered in this array
cust_stopwords = c(cust_stop1, cust_stop2, cust_stop3, cust_stop4, "usairway")

##Data Cleaning and Wrangling
tweetsDS.Clean<-tm_map(tweetsDS.Corpus, PlainTextDocument)
tweetsDS.Clean<-tm_map(tweetsDS.Corpus,tolower)
cust_stopwords <-sapply(cust_stopwords,tolower)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeNumbers)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeWords,stopwords("english"))
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removePunctuation)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stripWhitespace)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stemDocument)
tweetsDS.Clean = tm_map(tweetsDS.Clean, removeWords, cust_stopwords)


wordcloud(words = tweetsDS.Clean, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))                       
}
#################################################################
#TimeSeries Plot
#################################################################
#COUNT NEGATIVE REVIEWS EACH DAY FOR POSITIVE AND NEG REVIEWS
review_date1 = count(pos_reviews,'date', 'power_score')
review_date2 = count(neg_reviews, 'date' ,'power_score')
#CREATE DATAFRAME CONTAINING NO POSITIVE NAD NEGATIVE REVIEWS EACH DAY
time_series = merge(review_date1, review_date2, by = c('date'))

library(plotly)
#GET SPECIFIC DAY AND MONTH AS INTEGER
day = strftime(time_series$date,'%d')
month = strftime(time_series$date, "%m")
#CREATE VECTOR OF DAYS AND MONTHS
spec_date = paste(day, month, sep="/")
#add column to time series holding the date
time_series$date = spec_date
#create plot, titles seen in layout
plot_ly(time_series, x=~date, y=~freq.x,type = "scatter", mode="lines", line=list(color = "darkgreen") )%>%
  add_trace(time_series, x=~date, y=~freq.y,type = "scatter", mode="lines", line=list(color = "red") )%>%
  layout(title = 'Tweets Over Time',
         xaxis = list(title = 'Date'),
         yaxis  = list(title = 'Power Score')
  )


##########################################################################
#BARPLOT
##########################################################################
barchart = neg_reviews
barchart_df = count(neg_reviews, 'negativereason', 'power_score')
barchart_df

#CREATE VECTOR OF AIRLINE NAMES
airlines = as.vector(levels(barchart$airline))

#CREATING VECTORS FOR POWERSCORE OF EACH NEGATIVE REASON, FOR EACH AIRLINE
for( i in 1:length(airlines)) {
  cur_sub = subset(barchart, barchart$airline == levels(barchart$airline[i]))
  cur_breakdown = count(cur_sub, 'negativereason', 'power_score')
  
}

#CREATING A VECTOR OF ALL NEGATIVE REASONS
problems = as.vector(levels(neg_reviews$negativereason))
data = as.data.frame(airline_neg_scores)
data = as.data.frame(t(data))

#CREATE PLOT P BARCHART
p <- plot_ly(data, x = ~problems, y =~American , type = 'bar', name = 'American', marker = list(color = "lightslateblue")) %>%
  add_trace(y = ~Delta, name = 'Delta', marker = list(color = "lightseagreen")) %>%
  add_trace(y = ~Southwest, name = 'Southwest', marker = list(color = "lightskyblue")) %>%
  add_trace(y = ~`US Airways`, name = 'US Airways', marker = list(color = "lightsteelblue")) %>%
  add_trace(y = ~`Virgin America`, name = 'Virgin America', marker = list(color = "lightslategrey")) %>%
  layout(yaxis = list(title = 'Power Score'), barmode = 'stack',  xaxis = list(showgrid=T, title=""))

p

#################################################
#timeSeries
################################################
neg_reviews = subset(tweets, airline_sentiment=="negative")
twe_hr = count(neg_reviews, 'hours', 'power_score')
plot_ly(twe_hr, x=~hours, y=~freq,type = "scatter", mode="lines", line=list(color = "black"))


################################################
#BARCHART COMPARISON
###############################################
#plots the percentage breakdown of tweet sentiment grouped by each airline
cur_plot <- ggplot(tweets, aes(airline_sentiment, group = airline)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = abbreviate) +
  ylab("Relative Frequencies") +
  xlab("") +
  labs(fill = "Sentiment") +
  ggtitle("Individual Airline Sentiments") +
  facet_grid(~airline)
cur_plot


################################################
#PIE CHART
###############################################
air_pos = count(pos_reviews, 'airline', 'power_score')
airline_pie_data <- data.frame("Airline" = rownames(air_pos) , air_pos)
data <- airline_pie_data[, c('airline', 'freq')]

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')


p <- plot_ly(data, labels = ~airline, values = ~freq, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(freq, "positive tweets", sep= " "),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'US Airlines Market Share',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p


################################################
#BARCHART COMPARISON
###############################################
hist_data = count(neg_reviews, 'negativereason', 'retweet_count')
p <- plot_ly(x = ~ (hist_data$negativereason), y = ~(hist_data$freq), type = "bar", color = ~hist_data$negativereason)%>%
  layout(title = 'Retweets per Negative Reason',
         xaxis = list(title = ''),
         yaxis  = list(title = 'Number of Retweets')
  )
p
