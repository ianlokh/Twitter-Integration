#install the necessary packages
library(twitteR)
library(tm)
library(wordcloud)
library(XLConnect)

#necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#read in existing spreadsheet
ltaDataWB <- XLConnect::loadWorkbook("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/LTATwttrData-2015.xlsx")
ltaData = XLConnect::readWorksheet(ltaDataWB, sheet=1,
                        header = TRUE,
                        colTypes = c(XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.BOOLEAN,
                                     XLC$DATA_TYPE.NUMERIC,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.DATETIME,
                                     XLC$DATA_TYPE.BOOLEAN,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.NUMERIC,
                                     XLC$DATA_TYPE.BOOLEAN,
                                     XLC$DATA_TYPE.BOOLEAN,
                                     XLC$DATA_TYPE.STRING,
                                     XLC$DATA_TYPE.STRING),
                        forceConversion = TRUE,
                        dateTimeFormat = "%Y-%m-%d %H:%M:%S")


#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
#consumer_key <- 'TYrWFPkFAkn4G5BbkWINYw'
#consumer_secret <- 'qjOkmKYU9kWfUFWmekJuu5tztE9aEfLbt26WlhZL8'
#access_token <- '2853850382-iqWOBCkw0EoLpfgaOVorOZnbf3n79WOi0KqsYH2'
#access_secret <- 'bd49o8uzeNBbtzVaalOUJlOoe9fbIQmwcLaNr7fbbx53R'

consumer_key <- '7PPYKH38pXjxdTCMR2gW7idoZ'
consumer_secret <- 'JHaymz2hrb0E95AZBERRYDFPCLhewVdzCkVT1Ws1ZORh3uuOpJ'
access_token <- '2853850382-G876Yy7oSiwFDL3KFiewSuZiIHqUS7BXQ5WOg2v'
access_secret <- 'Y1tb155NjjJUaM8TNgA9E71GFseYGfZ8VyVEOjDJJ0CsP'

setup_twitter_oauth(consumer_key,
                    consumer_secret)

#retrieve the tweets from twitter - however the create date is in UTC! so must +8:00 hrs to get to SGT
ltaTwtr <- searchTwitter("LTAsg + Accident", n=500)
length(ltaTwtr)

#save text
#ltaTwtr_text <- sapply(ltaTwtr, function(x) x$getText())

#make data frame
tmpDf <- do.call("rbind", lapply(ltaTwtr, as.data.frame))
#merge into main data frame
ltaData <- rbind(ltaData,tmpDf)
#remove duplicate data based on ID
ltaData <- ltaData[(order(as.Date(ltaData$created), decreasing=TRUE)),]
ltaData <- ltaData[!duplicated(ltaData[,c("id")]), ]

#save lta data to xls data file (xlsx package)
#filename <- paste("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/LTATwttrData-",Sys.Date(),".xlsx",sep = "")
#write.xlsx(ltaDf, file = filename, sheetName = paste("LTATwttrData-",Sys.Date(),sep = ""), row.names = FALSE)
writeWorksheet(ltaDataWB, ltaData, sheet = 1)
saveWorkbook(ltaDataWB)



library(xlsx)
neaTwtr <- searchTwitter("@NEAsg", n=1000)
length(neaTwtr)

#make data frame
neaDf <- do.call("rbind", lapply(neaTwtr, as.data.frame))

#save nea data to xls data file
filename <- paste("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/NEATwttrData-",Sys.Date(),".xlsx",sep = "")
write.xlsx(neaDf, file = filename, sheetName = paste("NEATwttrData-",Sys.Date(),sep = ""), row.names = FALSE)

downloadNEAData <- function(day, year="2000", month="01")
{
  f <- paste("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/NEATwttrData-",year,"-",month,"-",day,".xlsx",sep="")
  #read in existing spreadsheet
  xWB <- XLConnect::loadWorkbook(f)
  x = XLConnect::readWorksheet(xWB, sheet=1,
                               header = TRUE,
                               forceConversion = TRUE,
                               dateTimeFormat = "%Y-%m-%d %H:%M:%S")
  x
}


getNEAData <- function(neaData, startDate, endDate, year="2000", month="01")
{
  #NEATwttrData-2015-05-22
  year = paste(formatC(year, width=4, flag="0"), sep="")
  month = paste(formatC(month, width=2, flag="0"), sep="")
  
  startDate = paste(formatC(startDate, width=2, flag="0"), sep="")
  endDate = paste(formatC(endDate, width=2, flag="0"), sep="")
  dates <- c(startDate:endDate)
  
  for (d in dates)
  {
    try(neaData <- rbind(neaData,downloadMSSData(d, year, month)), silent=TRUE)
  }
  neaData
}

neaDataWB <- XLConnect::loadWorkbook("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/NEATwttrData-2015.xlsx")
neaData = XLConnect::readWorksheet(neaDataWB, sheet=1,
                                   header = TRUE,
                                   forceConversion = TRUE,
                                   dateTimeFormat = "%Y-%m-%d %H:%M:%S")
neaData <- getMSSData(neaData, "22", "30","2015","05")
neaData <- getMSSData(neaData, "01", "30","2015","06")
neaData <- getMSSData(neaData, "01", "30","2015","07")

neaData <- neaData[(order(as.Date(neaData$created), decreasing=TRUE)),]
neaData <- neaData[!duplicated(neaData[,c("id")]), ]

writeWorksheet(neaDataWB, neaData, sheet = 1)
saveWorkbook(neaDataWB)




# Downloading of weather data from Metrological Service from Singapore

downloadMSSData <- function(stationID, year="2000", month="01")
{
  #fileUrl <- "http://www.weather.gov.sg/files/dailydata/DAILYDATA_S24_201504.csv"
  #download.file(fileUrl, paste("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/MSS_Data_",Sys.Date(),".csv",sep = ""),method="curl")
  f <- paste("http://www.weather.gov.sg/files/dailydata/DAILYDATA_S",stationID,"_",year,month,".csv",sep="")
  download.file(f, paste("/Users/ianlo/Documents/MTech-EBAC/Twitter Integration/MSS_Data_S",stationID,"_",year,month,".csv",sep = ""),method="curl")
}


getMSSData <- function(startStation, endStation, year="2000", month="01")
{

  year = paste(formatC(year, width=4, flag="0"), sep="")
  month = paste(formatC(month, width=2, flag="0"), sep="")
  
  startStation = paste(formatC(startStation, width=2, flag="0"), sep="")
  endStation = paste(formatC(endStation, width=2, flag="0"), sep="")
  stationID <- c(startStation:endStation)

  for (sID in stationID)
  {
    #
    try(downloadMSSData(sID, year, month), silent=FALSE)
  }
}


getMSSData(23,123,"2015","05")

















#parse data
df.accident <- lapply(df$text, substr,1,8)

#parse data
df.accident <- lapply(df$text, substr,1,8)


#create corpus
r_stats_text_corpus <- Corpus(VectorSource(df$text))
inspect(r_stats_text_corpus)

#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#alternative steps if you're running into problems 
r_stats<- searchTwitter("#Rstats", n=1500)
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)
