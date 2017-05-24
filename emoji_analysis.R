# load packages and set options
options(stringsAsFactors = FALSE)
library(dplyr)
library(twitteR)
library(stringr)
library(rvest)
library(Unicode)
library(tm)
library(ggplot2)

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

## ---- utility functions ----
# this function outputs the emojis found in a string as well as their occurences
count_matches <- function(string, matchto, description, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) & length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}

# this function applies count_matches on a vector o texts and outputs a data.frame
emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  
  texts %>% 
    lapply(count_matches, matchto = matchto, description = description, sentiment = sentiment) %>%
    bind_rows
  
}

# function that separates capital letters hashtags
hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE)
    }
    return(text)
  }
  unname(sapply(text, hg))
}

# tweets cleaning pipe
cleanPosts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

# function that outputs a df of emojis with their top 5 words (by frequency)
wordFreqEmojis <- function(df, text = df$text, description = df$description, top = 5) {
  
  
  lapply(unique(description), function(x) {
    
    dat <- df %>% 
      filter(description == x)
    
    myCorpus <- Corpus(VectorSource(dat$text)) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(myCorpus)
    # find the sum of words in each Document
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    # collapse matrix by summing over columns
    freq <- colSums(as.matrix(dtm))
    # create sort order (descending)
    ord <- order(freq, decreasing=TRUE)
    
    list(emoji = rep(x, top), words = names(freq[ord][1:top]), frequency = freq[ord][1:top]) 
    
  }) %>% 
    bind_rows
  
}

## ---- setup ----
# load twitter credentials and authorize
# input your own credentials here
load("twitCred.Rdata")
api_key <- twitCred$consumerKey
api_secret <- twitCred$consumerSecret
access_token <- twitCred$oauthKey
access_token_secret <- twitCred$oauthSecret
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# read in emoji dictionary
# I used to get the dictionary from Felipe: https://github.com/felipesua
# but he put it down, so I uploaded the csv file to my github profile: 
# https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emojis.csv
# input your custom path to file
emDict_raw <- read.csv2("emojis.csv") %>% 
  select(EN, ftu8, unicode) %>% 
  rename(description = EN, r.encoding = ftu8)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and remove skin tone info in description
emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

matchto <- emDict$r.encoding
description <- emDict$description

# get some sample data
raw_usermedia <- userTimeline(user = "parishilton", n = 3200) %>%
  twListToDF
# convert to a format we can work with
usermedia <- raw_usermedia %>% 
  mutate(text = iconv(text, from = "latin1", to = "ascii", sub = "byte"))

## ---- most used emoji ----
# rank emojis by occurence in data
rank <- emojis_matching(usermedia$text, matchto, description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count)) %>%
  arrange(-n)

head(rank, 10)

## ---- tweets with most emojis ----
tweets <- emojis_matching(usermedia$text, matchto, description) %>% 
  group_by(text) %>% 
  summarise(n = sum(count)) %>%
  # I add the time created because it makes usermedia_merged %>% mutate(date = as.Date(created)) %>% group_by(date) %>% summarise(sent = mean(sentiment_score, na.rm = TRUE)) %>% ggplot + aes(x = date, y = sent) + geom_point() + geom_line()it easiert to look up certain tweets
  merge(usermedia, by = "text") %>% 
  select(text, n, created) %>%
  arrange(-n)

mean(tweets$n, na.rm = TRUE)

## ---- sentiment analysis with emojis ---- 
# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", "neutral", 
                   "positive", "sentiment_score", "description", "block")
# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 

str(emojis)
# unicode column is unicode character class

# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emDict, by = "unicode")
# emojis %>% filter(!unicode %in% emDict$unicode) %>% View
# we loose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

new_matchto <- emojis_merged$r.encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

sentiments <- emojis_matching(usermedia$text, new_matchto, new_description, sentiment) %>%
  mutate(sentiment = count*as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(sentiment_score = sum(sentiment))

usermedia_merged <- usermedia %>% 
  select(text, created) %>% 
  merge(sentiments, by = "text", all.x = TRUE)
# some tweets don't have sentiment scores

# this is how it looksl ike over time:
usermedia_merged %>% 
  mutate(date = as.Date(created)) %>% 
  group_by(date) %>% 
  summarise(sent = mean(sentiment_score, na.rm = TRUE)) %>% 
    ggplot + 
    aes(x = date, y = sent) + 
    geom_point() + 
    geom_line()

## ---- emojis associated with words in tweets ----
# tweets
raw_texts <- emojis_matching(usermedia$text, matchto, description) %>% 
  select(-sentiment, -count) %>%
  mutate(text = cleanPosts(text)) %>%
  filter(text != "") %>% 
  filter(!is.na(description))
word_emojis <- wordFreqEmojis(raw_texts, raw_texts$text, raw_texts$description) %>% 
  filter(!is.na(words))

## ---- emojis and weekdays ----
emojis_matching(usermedia$text, matchto, description) %>%
  merge(usermedia %>% select(text, created), by = "text") %>% 
  select(description, created) %>% 
  mutate(weekday = weekdays(created)) %>% 
  select(-created) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

 