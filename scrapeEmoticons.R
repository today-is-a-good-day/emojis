library(rvest)
library(magrittr)
library(dplyr)

# reference website
url <- "http://apps.timwhitlock.info/emoji/tables/unicode"

# get emoticons
emoticons <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[1]') %>%
    html_table()
emoticons <- data.frame(emoticons[[1]]$Native, emoticons[[1]]$Bytes, 
                           emoticons[[1]]$Description)
names(emoticons) <- c("Native", "Bytes", "Description")

# get additional emoticons
addemoticons <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[6]') %>%
    html_table()
addemoticons <- data.frame(addemoticons[[1]]$Native, addemoticons[[1]]$Bytes, 
                              addemoticons[[1]]$Description)
names(addemoticons) <- c("Native", "Bytes", "Description")

# get dingbats
dingbats <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[2]') %>%
    html_table()
dingbats <- data.frame(dingbats[[1]]$Native, dingbats[[1]]$Bytes, 
                          dingbats[[1]]$Description)
names(dingbats) <- c("Native", "Bytes", "Description")

# get transports
transport <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[3]') %>%
    html_table()
transport <- data.frame(transport[[1]]$Native, transport[[1]]$Bytes, 
                           transport[[1]]$Description)
names(transport) <- c("Native", "Bytes", "Description")

# get additional transports
addtransport <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[7]') %>%
    html_table()
addtransport <- data.frame(addtransport[[1]]$Native, addtransport[[1]]$Bytes, 
                              addtransport[[1]]$Description)
names(addtransport) <- c("Native", "Bytes", "Description")

# get enclosed emoticons
enclosed <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[4]') %>%
    html_table()
enclosed <- data.frame(enclosed[[1]]$Native, enclosed[[1]]$Bytes, 
                          enclosed[[1]]$Description)
names(enclosed) <- c("Native", "Bytes", "Description")

# get uncategorized emoticons
uncategorized <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[5]') %>%
    html_table()
uncategorized <- data.frame(uncategorized[[1]]$Native, uncategorized[[1]]$Bytes, 
                               uncategorized[[1]]$Description)
names(uncategorized) <- c("Native", "Bytes", "Description")

# get additional other emoticons
addothers <- url %>%
    html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[8]') %>%
    html_table()
addothers <- data.frame(addothers[[1]]$Native, addothers[[1]]$Bytes, 
                           addothers[[1]]$Description)
names(addothers) <- c("Native", "Bytes", "Description")

# combine all dataframes to overall dataframe
alltogether <- bind_rows(list(emoticons, addemoticons, dingbats, transport, 
                              addtransport, enclosed, uncategorized, addothers))