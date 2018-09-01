library(data.table)
library(FSelector)
library(stringr)
library(dplyr)
library(caret)
library(lubridate)
library(tm)
library(proxy)

#Set working Directory
setwd("F:/Cosmodea/Cosmodea_Clusteting_Part1")

#Import Data
Report <- fread("Search term report(1).csv")

#Check data imorted properly
head(Report)
tail(Report)

#Remove Last observation "Total"
Report <- Report[-17743,]

#Summary
summary(Report)

#Structure
str(Report)

#Set Colnames
setnames(Report,
         c("Match type", "Search term", "Added/Excluded", "Campaign", 
           "Ad group", "Clicks", "Impressions", "CTR", "Avg. CPC", "Cost", 
           "Avg. position", "Conversions", "Cost / conv.", "Conv. rate", 
           "All conv.", "View-through conv."),
         c("Match_type", "Search_term", "Added/Excluded", "Campaign", 
           "Ad_group", "Clicks", "Impressions", "CTR", "Avg.CPC", "Cost", 
           "Avg.position", "Conversions", "Cost/conv", "Conv.rate", 
           "All_conv", "View_through_conv"))

#Mutate the columns CTR, Cost, Cost/Conv, Conv.rate
Report <- Report %>%
  mutate(
    CTR = as.numeric(sub("%","",CTR)),
    Cost = as.numeric(str_replace_all(Cost,",","")),
    `Cost/conv` = as.numeric(str_replace_all(`Cost/conv`,",","")),
    Conv.rate = as.numeric(sub("%","",Conv.rate))
  )

#Converting Text to numeric
#Calculate Term frequency
Report_Searchterm <- Corpus(VectorSource(Report$Search_term))
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
Searchterm_tf <- TermDocumentMatrix(Report_Searchterm,
                                    control = control_list)
Searchterm_tf_matrix <- as.matrix(Searchterm_tf)

trans_tf <- as.data.frame(t(Searchterm_tf_matrix))
row.names(trans_tf) <- Report$Search_term
write.csv(trans_tf,"tf.csv")

# idf
idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
idf_cross <- crossprod(tf, idf) 

colnames(idf_cross) <- rownames(tf)
9884112889
9884432889

write.csv(idf_cross,"Searchterm_tf_idf.csv")
