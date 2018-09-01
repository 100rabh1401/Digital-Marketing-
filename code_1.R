#Set working directory
setwd("F:/Cosmodea")

library(data.table)
library("RSiteCatalyst")
library("RTextTools") #Loads many packages useful for text mining

#Import data
df <- fread("Practice.csv")
df1 <- df[,1]

#COnvert data table into vector
df_vec <- as.matrix(df1[c(1:499),])
dtm <- create_matrix(df_vec,
                     stemWords=TRUE,
                     removeStopwords=FALSE,
                     minWordLength=1,
                     removePunctuation= TRUE)
dtm
findFreqTerms(dtm, lowfreq=4)

#Build model
kmeans5<- kmeans(dtm, 5)

kw_with_cluster <- as.data.frame(cbind(df_vec, kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")


cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)


write.csv(cluster1,"clsuter1.csv")
