library(dplyr)
library(magrittr)

data = readLines(file("tweets.txt"))
tweets = gsub("^.*\\|.*\\|","",x = data)

utf8tweets = iconv(tweets,to="latin2//TRANSLIT")

words = strsplit(tweets," ") %>%
  unlist() %>%
  gsub("http.*","",.) %>%
  gsub("[\\.\\,\\!\\\"\\'\\?]"," ",.) %>%
  gsub("\\@.*"," ",.) %>%
  strsplit(.,"[ \\.\\,\\-]|\\&amp\\;") %>%
  unlist() %>%
  unique()
