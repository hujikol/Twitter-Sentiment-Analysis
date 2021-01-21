library(SnowballC)
library(twitteR)
library(tm)
library(NLP)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

#library sentiment bayes
source("C:/Users/nicolas/Documents/R/ProjectAkhir/create_matrix.R")
source("C:/Users/nicolas/Documents/R/ProjectAkhir/classify_polarity.R")
source("C:/Users/nicolas/Documents/R/ProjectAkhir/classify_emotion.R")

#API Twitter
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "T2bNPe3AU1WerRD2o5iiy7PDn" 
CUSTOMER_SECRET <- "us1ZoVTzo5XqRgTPaGnGtMgsp18CeCgkszkHVDM2zd5sqfkozj" 
ACCESS_TOKEN <- "1318222554-1BZg9zxgfiSEt3ebkejlkYCdRLnydngTSdBT22R" 
ACCESS_secret <- "OOSa6QiNgo9XGAvkm2MADbJM9rf9Yxixp7tEHOH69f8nV" 
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

#mengambil tweet
tweetsData <- searchTwitter('covid -filter:retweets',  
                            since='2019-12-01', until='2021-01-15', n=1000, lang="en") #mendefinisikan keyword
tweetsText <- sapply(tweetsData, function(x) x$getText()) 
write.csv(tweetsText, file = 'C:/Users/nicolas/Documents/R/ProjectAkhir/dataTwitter.csv')

#membuat data frame tweets
tweets = data.frame(Uncleaned_Tweets=tweetsText, stringsAsFactors=FALSE)

#cleaning data
tweetsText = sapply(tweetsData, function(x) x$getText())
tweetsText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetsText) 
tweetsText = gsub("@\\w+", "", tweetsText) 
tweetsText = gsub("[[:punct:]]", "", tweetsText) 
tweetsText = gsub("[[:digit:]]", "", tweetsText) 
tweetsText = gsub("http\\w+", "", tweetsText) 
tweetsText = gsub("[ \t]{2,}", "", tweetsText) 
tweetsText = gsub("^\\s+|\\s+$", "", tweetsText)
tweetsText = gsub("note", "", tweetsText)

# define "tolower error handling" function
try.error = function(x) {
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
tweetsText = sapply(tweetsText, try.error)

# remove NAs in tweetsText
tweetsText = tweetsText[!is.na(tweetsText)]
names(tweetsText) = NULL

write.csv(tweetsText, file = "C:/Users/nicolas/Documents/R/ProjectAkhir/dataCleaned.csv")

#membuat data frame cleaned tweets
cleanedtweets = data.frame(Cleaned_Tweets=tweetsText, stringsAsFactors=FALSE)

# classify emotion
class_emo = classify_emotion(tweetsText, algorithm="bayes", prior=1.0, verbose = FALSE)
emotion = class_emo[,7] # get emotion best fit
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(tweetsText, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=tweetsText, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'C:/Users/nicolas/Documents/R/ProjectAkhir/dataSentimen.csv')

### Word Cloud ###
corpus = Corpus(VectorSource(tweetsText))                          # convert tweets to corpus

#some more cleaning
corpus = tm_map(corpus, removeWords, stopwords("english"))   #remove stopwords like "and","the" and"that"
corpus = tm_map(corpus, stripWhitespace)                     # remove whitespace

# word frequency
uniqwords = as.matrix(TermDocumentMatrix(corpus))            # covert corpus to Term Document matrix 
wordfreq = sort(rowSums(uniqwords),decreasing=TRUE)          # Count frequency of words in each tweet
WCinput = data.frame(word = names(wordfreq),freq=wordfreq)   # word frequency dataframe

#generate the wordcloud
wordcloud(words = WCinput$word, freq = WCinput$freq, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.1,
          colors=brewer.pal(8, "Dark2"))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Set3") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Analisis Sentiment COVID-19",
       plot.title = element_text(size=12))

plotSentiments1 <- function(sentiment_dataframe, title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Set3") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Jumlah Tweets") + 
    xlab("Kategori Emosi")
}
#plotting emotions
plotSentiments1(sent_df, "Analisis Sentiment COVID-19")


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of COVID-19",
       plot.title = element_text(size=12))

plotSentiments2 <- function(sent_df, title){
  library(ggplot2)
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Jumlah Tweets") +
    xlab("Kategori Polarity")
}
#plotting polarity
plotSentiments2(sent_df, "Analisis Polaritas COVID-19")
