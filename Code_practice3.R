remove(list=ls())

#Set working directory
setwd("F:/Cosmodea")

#Attach Libraries
library(data.table)
library(tm)
library(FSelector)
library(stringr)
library(dplyr)
library(caret)
library(lubridate)
library(SiteCatalyst)
library(RTextTools) 
library(cluster)

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

unique_search <- unique(Report$Search_term)
##write.csv(unique_search,"Search_term.csv")
##Removing Factor columns
Report_new <- Report[,-c(1,3,4,5)]

#sample.df.cor <- cor(Report[,-c(1:5)]) 
#sample.df.highly.cor <- findCorrelation(sample.df.cor, cutoff=.7)

#Check Structure of file
str(Report)

#Take one row out from the data table
df <- as.character(Report$Search_term)

#COnvert data table into vector
df_matrix <- create_matrix(df,
                     stemWords=TRUE,
                     minWordLength=1,
                     removePunctuation= TRUE,
                     weighting=weightTf)
names(df_matrix)

unique_words <- findFreqTerms(df_matrix)
#binding <- cbind(df_matrix$i,df_matrix$j,df_matrix$v)

#Build model
kmeans.model_7 <- kmeans(df_matrix, 7)

#Build plot for the cluster
clusplot(as.data.frame(df_matrix), kmeans.model_7$cluster, 
         main='Representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Making the cluster
kw_with_cluster <- as.data.frame(cbind(df_vec, kmeans.model_7$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans_20")

cluster1 <- subset(kw_with_cluster, subset=kmeans_20 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans_20 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans_20 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans_20 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans_20 == 5)
cluster6 <- subset(kw_with_cluster, subset=kmeans_20 == 6)
cluster7 <- subset(kw_with_cluster, subset=kmeans_20 == 7)
cluster8 <- subset(kw_with_cluster, subset=kmeans_20 == 8)
cluster9 <- subset(kw_with_cluster, subset=kmeans_20 == 9)
cluster10 <- subset(kw_with_cluster, subset=kmeans_20 == 10)
cluster11 <- subset(kw_with_cluster, subset=kmeans_20 == 11)
cluster12 <- subset(kw_with_cluster, subset=kmeans_20 == 12)
cluster13 <- subset(kw_with_cluster, subset=kmeans_20 == 13)
cluster14 <- subset(kw_with_cluster, subset=kmeans_20 == 14)
cluster15 <- subset(kw_with_cluster, subset=kmeans_20 == 15)
cluster16 <- subset(kw_with_cluster, subset=kmeans_20 == 16)
cluster17 <- subset(kw_with_cluster, subset=kmeans_20 == 17)
cluster18 <- subset(kw_with_cluster, subset=kmeans_20 == 18)
cluster19 <- subset(kw_with_cluster, subset=kmeans_20 == 19)
cluster20 <- subset(kw_with_cluster, subset=kmeans_20 == 20)

bb <- rbind(cluster1,cluster2,cluster3,cluster4,cluster5,cluster6,cluster7,cluster8,
            cluster9,cluster10,cluster11,cluster12,cluster13,cluster14,cluster15,
            cluster16,cluster17,cluster18,cluster19,cluster20)

#Export the clusters file
write.csv(bb,"alldata_in_clusters_20.csv")


#Check for the appropriate value of k by plotting
wtss<-c()
for(i in 1:20)
{
  model1<-kmeans(df_matrix,i)
  wtss<-c(wtss,model1$tot.withinss)
}
plot(wtss, type="b")

#Develop the model where distance calculator is Hamming
kmeans_133 <- kmeans(df_matrix, 133)

#Making the cluster
kw_with_cluster <- as.data.frame(cbind(df_vec, kmeans_133$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans_20")
