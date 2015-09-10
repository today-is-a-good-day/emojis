# post emoticons
require(stringi)
tweetslist <- paste0(stri_unescape_unicode(alltogether$Native)," ", alltogether$Description)
lapply(tweetslist, tweet)

# oauth - not working yet
load()
# retrieve emoticons
tweetsback <- userTimeline("Remoticons", n = 900) %>%
    twListToDF
