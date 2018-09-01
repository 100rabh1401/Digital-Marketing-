#Set working directory
setwd("F:/Cosmodea")
remove(list = ls())

#Attach Libraries
library(data.table)
library(FSelector)
library(stringr)
library(dplyr)
library(caret)
library(lubridate)
library(klaR)

#Import file
Report <- fread("Search term report(1).csv") 

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

#Check Structure of file
str(Report)

#Convert character variables into factor
Report[,c(1:4)] <- lapply(Report[,c(1:4)], factor)
#Check for important varibales#
att.scores <- random.forest.importance(Search_term ~ ., Report)
names(att.scores)
att_numeric <- att.scores$attr_importance 
sort(att.scores$attr_importance,decreasing = T)
att.scores$attr_importance <- colnames(SAheart[,-10])

#PLot
cor(Report_new[,-1])


#Check the uniques items of Search_term
unique(Report$Search_term)

match(Report, Report$Search_term)
a <- as.data.frame(table(Report$Search_term))
write.csv(a,"Table.csv")

#Make New data
Report_new <- Report[,-c(1,3,4,5)]
Report_new$Search_term <- as.factor(Report_new$Search_term)

#Check for appropriate K value
wtss<-c()
for(i in 1:20)
{
  model1<-kmeans(Report_new,i,iter.max = 5)
  wtss<-c(wtss,model1$tot.withinss)
}
plot(wtss)

btss<-c()
for(i in 1:50)
{
  model2<-kmeans(Report_new,i,iter.max = 5)
  btss<-c(btss,model2$betweenss)
}
plot(btss)

model3 <- kmodes(Report_new,8,iter.max = 5)
model3

model4 <- kmeans(Report_new,8,iter.max = 100,nstart = 20)



Report_Searchterm <- Report$`Search term`
Report_1 <- fread("tf.csv")
Report_1 <- Report_1[-17744,]
MODEL_7 <- kmeans(Report_1,7,iter.max = 1,algorithm = "Forgy")
