require(stringi)
library(magrittr)
library(twitteR)
library(tidyr)

# oauth
load("twitterOauthEmoticons.Rdata")
consumer_key <- my_oauth$consumerKey
consumer_secret <- my_oauth$consumerSecret
access_token <- my_oauth$oauthKey
access_secret <- my_oauth$oauthSecret
setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)

# post emoticons
tweetslist <- paste0(stri_unescape_unicode(alltogether$Native)," ", alltogether$Description)
# don´t post all at once <- rate limit per 15 minutes!
lapply(tweetslist[1:100], tweet)
lapply(tweetslist[101:200], tweet)
lapply(tweetslist[201:300], tweet)
lapply(tweetslist[301:400], tweet)
lapply(tweetslist[401:500], tweet)
lapply(tweetslist[501:600], tweet)
lapply(tweetslist[601:700], tweet)
lapply(tweetslist[701:800], tweet)
lapply(tweetslist[806:length(tweetslist)], tweet)
# I posted two parts per day, 15 minutes appart

# retrieve emoticons
tweetsback <- userTimeline("Remoticons", n = 900) %>%
    twListToDF

# split one column into df with the columns "bytes" and "decription"
tweetsback2 <- data.frame(text = iconv(tweetsback$text, "latin1", "ASCII", "byte"), 
                          stringsAsFactors = FALSE)
# kind of an ugly workaround, but the bytes made it impossible to create a df the proper way
column1 <- separate(tweetsback2, text, into = c("Bytes", "Description"), sep = "\\ ")
column2 <- separate(tweetsback2, text, into = c("Bytes", "Description"), sep = "^[^\\s]*\\s")
df <- data.frame(Bytes = column1$Bytes, Description = column2$Description)

# merge retrieved encoding with original encoding to one df &v write to file
eotw1 <- merge(alltogether, df1, by = "Description")
write.csv2(eotw1, file = "eotw1.csv", row.names = FALSE)