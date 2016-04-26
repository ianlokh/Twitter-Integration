#install the necessary packages
library(twitteR)

#necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'consumer_key'
consumer_secret <- 'consumer_secret'
access_token <- 'access_token'
access_secret <- 'access_secret'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#retrieve the tweets from twitter - however the create date is in UTC! so must +8:00 hrs to get to SGT
ltaTwtr <- searchTwitter("LTATrafficNews + Accident", n=500)
length(ltaTwtr)

#save text
#ltaTwtr_text <- sapply(ltaTwtr, function(x) x$getText())

#make data frame
tmpDf <- do.call("rbind", lapply(ltaTwtr, as.data.frame))
