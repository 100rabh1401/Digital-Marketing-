
#########################################################################
library(tm)
library(proxy)
library(dplyr)
# a slightly larger dataset
setwd("F:/Cosmodea")
Searchterm <- read.csv("Search term report(1).csv", stringsAsFactors = FALSE)
list( head(Searchterm), dim(Searchterm) )

unique(Searchterm$Search.term)

# tf 
Searchterm_corpus  <- Corpus( VectorSource(Searchterm$Search.term) )
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tf <- TermDocumentMatrix(Searchterm_corpus, control = control_list) %>% as.matrix()
tf <- as.matrix(tf) 

trans_tf <- as.data.frame(t(tf))

write.csv(trans_tf,"tf.csv")
# idf
idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
idf_cross <- crossprod(tf, idf) 

colnames(idf_cross) <- rownames(tf)


write.csv(idf_cross,"Searchterm_tf_idf.csv")
