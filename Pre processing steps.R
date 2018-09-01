library("wordnet")
library("tm")

searchTerms=read.csv("SearchTermData.csv", stringsAsFactors = FALSE,header = TRUE)
dim(data)
class(searchTerms)

str(searchTerms)
nrow(searchTerms)

docs <- Corpus(VectorSource( searchTerms))
docs <-tm_map(docs,content_ transformer(tolower))
KeywordsCorpus <- tm_map(docs, removePunctuation)
KeywordsCorpus <- tm_map(KeywordsCorpus, removeWords, stopwords('english'))
dtm <- DocumentTermMatrix( KeywordsCorpus)
m <- as.matrix(dtm)
dim(m)

d <- data.frame(Term = colnames(m), word_count = m[1,])
terms=d$Term

all_pos <- c("ADJECTIVE", "ADVERB", "NOUN","VERB")
syns <- vector("list", length(all_pos))

for(w in seq(nrow(terms))){
  # if sysns of (d$Term[w]) has been calculated skip over current w 
  emf <- getTermFilter(" ExactMatchFilter", as.character(d$Term[w]), TRUE)  
  for(i in seq_along(syns)){
    terms <- getIndexTerms(all_pos[i], 1, emf)
    if(is.null(terms)){
      syns[i] <- NA
    } else{
      syns[[i]] <-  getSynonyms(terms[[1]])
    }
  }
  # store the results of syns for current w 
}

synonyms(result, "NOUN")


getSynonyms("table")

#for seting dictionary
Sys.setenv(WNHOME = "/usr/bin/wordnet") 
install.packages("wordnet") 
library(wordnet) 
path <- file.path("usr", "share", "dict") 
setDict(path)


#Reproducible data - Quotes from  Wuthering Heights by  Emily Bronte
library(stringr)
#Spliting into sentence based on carriage return
s <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))

library(NLP)
library(openNLP)

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

#---------------replace one alphabet from a string
result <- lapply(s,tagPOS)
result <- as.data.frame(do.call(rbind, result))

group <- c("12357e", "12575e", "197e18", "e18947")
group

gsub("e", "", group)
#---------------------
library(wordnet)
inWordnet <- function(w, pos =  c("ADJECTIVE", "ADVERB", "NOUN", "VERB")) {
  for (x in pos) {
    filter <- getTermFilter(" ExactMatchFilter", w, TRUE)
    terms <- getIndexTerms(x, 5, filter)
    if (!is.null(terms)) return(TRUE)
  }
  return(FALSE)
}

inWordnet("car")


#-----------correct spelling mistakes
getwd()
dictionary1=read.csv("customDict.csv")
nrow(dictionary1)
class(dictionary)
dictionary=unique(dictionary)

dictionary=tolower(dictionary$ V1)

library(qdap)
d=searchTerms$searchTerm
class(terms)
terms1=as.data.frame(terms)
nrow(terms1)
dictionary=as.character( dictionary)
dict1 <- c(qdapDictionaries:: GradyAugmented, dictionary)
#dict_test <- c(qdapDictionaries:: GradyAugmented, "benz")
#which(dict1=="upto")

d1=as.data.frame(d)
aa=c()
for(i in 1:nrow(terms1)){
  aa=rbind(aa,which_misspelled( terms1[i,],range = 12, suggest=TRUE,assume.first. correct = FALSE, 
                                dictionary = dict1, method = "lv"))
  
}
misspelled=aa$not.found
suggest=aa$more.suggestions
misspelled1=unique(misspelled)
suggest1=unique(suggest)
which_misspelled("accessory", range = 12, suggest=TRUE,assume.first. correct = FALSE, dictionary = dict1, method = "lv")
#misspelled1=as.data.frame( misspelled1,stringsAsFactors = FALSE)
#to find length of suggest (list)
max.length <- max(sapply(suggest, length))
#to fill with na till max length
suggest1 <- lapply(suggest1, function(v) { c(v, rep(NA, max.length-length(v)))})
suggest1=as.data.frame( suggest1,stringsAsFactors = FALSE)
nrow(suggest1)

library(data.table)
tsuggest=transpose(suggest1)
View(tsuggest)
dat=cbind(misspelled1, tsuggest)
View(dat)
write.csv(dat,"misspelledSuggestions28Aug. csv")
dictt=as.data.frame( dictionary)

for(i in 1:nrow(dat)){
  for(j in 2:ncol(dat))
  {
    if(!is.na(dat[i,j])){
      for(jj in 1:nrow(dictt)){
        if(dat[i,j]==dictt[jj,])
          dat[i,2]=dat[i,j]
      }
    }
  }   
}
nrow(dictt)
View(dat)

write.csv(dat,"MisspelledReplacement28Aug. csv")

write.csv(aa$not.found," misspelledwords28Aug.csv")
aaa=rbind(aa$not.found,aa$ suggestion)
write.csv(aaa,"misspelledwordsSuggestions28Au g.csv")



