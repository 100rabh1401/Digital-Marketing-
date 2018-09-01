remove(list=ls())

#Set working directory
setwd("F:/Cosmodea")

#Import file
Report <- fread("Search term report(1).csv") 
Report <- Report[-17743,]

#Summary of file
summary(Report)

#Change the column names
setnames(Report,
         c("Match type", "Search term", "Added/Excluded", "Campaign", 
           "Ad group", "Clicks", "Impressions", "CTR", "Avg. CPC", "Cost", 
           "Avg. position", "Conversions", "Cost / conv.", "Conv. rate", 
           "All conv.", "View-through conv."),
         c("Match_type", "Search_term", "Added/Excluded", "Campaign", 
           "Ad_group", "Clicks", "Impressions", "CTR", "Avg.CPC", "Cost", 
           "Avg.position", "Conversions", "Cost/conv", "Conv.rate", 
           "All_conv", "View_through_conv"))

#Convert CTR, Cost, Conv.rate & Conv.rate into numeric
Report <- Report %>%
  mutate(
    CTR = as.numeric(sub("%","",CTR)),
    Cost = as.numeric(str_replace_all(Cost,",","")),
    `Cost/conv` = as.numeric(str_replace_all(`Cost/conv`,",","")),
    Conv.rate = as.numeric(sub("%","",Conv.rate))
  )

#Convert character variables into factor
Report[,c(1:5)] <- lapply(Report[,c(1:5)], factor)

#Creating corpus
docs <- Report[,2]
docs_n <- lapply(docs, PlainTextDocument)  #it didnt worked
docs_nn <- Corpus(VectorSource(docs))

#inspect a particular document
writeLines(as.character(docs[[30]]))

#getTransformations()

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return(gsub(pattern," ",x))})

#content transformer to eliminate colons and hypens
docs_nn <- tm_map(docs_nn,removePunctuation)

#remove stopwords using the standard list in tm
#docs_nn <- tm_map(docs_nn, removeWords, stopwords("english"))

#Strip whitespace (cosmetic?)
docs_nn <- tm_map(docs_nn, stripWhitespace)

#Stemming
writeLines(as.character(docs_nn[[30]]))

#load library
library(SnowballC)
#Stem document
docs_nn <- tm_map(docs_nn,stemDocument)
writeLines(as.character(docs[[300]]))

#Document Term Matrix
dtm <- DocumentTermMatrix(docs_nn)
dtm

inspect(dtm[1:2,1000:1005])

#Mining the corpus
freq <- colSums(as.matrix(dtm))
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#orderrrrrrrrrrrrrrr <- freq[ord]
#write.csv(orderrrrrrrrrrrrrrr,"new_unique.csv")

#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)]  

dtmr <-DocumentTermMatrix(docs_nn, control=list(wordLengths=c(4, 20),
                                             bounds = list(global = c(3,27))))
dtmr

findFreqTerms(dtmr,lowfreq=5)

findAssocs(dtmr,"color",0.1)
