library(quanteda)
library(tm)
#Load Text
searchTerms <- read.csv("Search term report(1).csv", stringsAsFactors = TRUE)
searchTerms <- searchTerms[,2]
searchTermsUnique <- as.character(unique(searchTerms))

# create a corpus from the immigration texts from UK party platforms
SearchtermCorpus <- 
  corpus(searchTermsUnique,
         docvars = data.frame(party = names(searchTermsUnique)),
         metacorpus = list(notes = "Search Terms"))
SearchtermCorpus

summary(SearchtermCorpus)
names(SearchtermCorpus)
sclass <- kwic(SearchtermCorpus, 
               c("sclass","s class","s320","s200","s350","s500",
                 "s63","mercedes benz 500","mercedes benz 520",
                 "mercedes benz 520d","mercedes benz 520 d"), 20)
sclass$Searchterm <- paste(sclass$pre,sclass$keyword,sclass$post)
sclass$Target <- "sclass"
head(sclass)
#write.csv(sclass,"sclass.csv")
cclass <- kwic(SearchtermCorpus,
               c("c200","c 200","c220","c 220","c250","c 250",
                 "c220d","c 220d","c63","c 63","c180","c 180",
                 "mercedes220","mercedes 220","mercedes220d",
                 "mercedes 220d","mercedes250","mercedes 250",
                 "c300","c 300","kompressor","cclass","c class"),
               20)
cclass$Searchterm <- paste(cclass$pre,cclass$keyword,cclass$post)
cclass$Target <- "cclass"

#write.csv(cclass,"cclass.csv")
eclass <- kwic(SearchtermCorpus,
               c("eclass","e class","e200","e 200","e250",
                 "e 250","e350d","e 350d","e220d","e 220d",
                 "ecabriolet","e cabriolet","e400","e 400",
                 "e220","e 220","e270","e 270","e280","e 280",
                 "e300","e 300","e320","e 320","e63","e 63"),
               20)
eclass$Searchterm <- paste(eclass$pre,eclass$keyword,eclass$post)
eclass$Target <- "eclass"

#write.csv(eclass,"eclass.csv")
aclass <- kwic(SearchtermCorpus,
               c("aclass","a class","a180","a 180","a200",
                 "a 200","a200d","a 200d","mercedes180",
                 "mercedes 180","aseries","a series",
                 "hatchback"),20)
aclass$Searchterm <- paste(aclass$pre,aclass$keyword,aclass$post)
aclass$Target <- "aclass"

#write.csv(aclass,"aclass.csv")
bclass <- kwic(SearchtermCorpus,
               c("bclass","b class","b180","b 180","b200",
                 "b 200"),20)
bclass$Searchterm <- paste(bclass$pre,bclass$keyword,bclass$post)
bclass$Target <- "bclass"

#write.csv(bclass,"bclass.csv")
gle <- kwic(SearchtermCorpus,
            c("gle","gle250","gle 250","gle250d","gle 250d",
              "gle350","gle 350","gle350d","gle 350d",
              "gle450","gle 450","gle63","gle 63","glecoupe",
              "gle coupe","4matic","4 matic","gl350","gl 350",
              "gl63","gl 63","gl450","gl 450","gl500","gl 500",
              "glclass","gl class","gl200","gl 200","gl250",
              "gl 250","mercedes 7 seater","mercedes 8 seater",
              "7 seater","8 seater"),20)
gle$Searchterm <- paste(gle$pre,gle$keyword,gle$post)
gle$Target <- "gle"

#write.csv(gle,"gle.csv")
gla <- kwic(SearchtermCorpus,
            c("gla","gla200","gla 200","gla200d","gla 200d",
              "gla45","gla 45","gla220","gla 220","gla250",
              "gla 250","gla350","gla 350","gla200cdi",
              "gla 200cdi","gla 200 cdi","mercedes200",
              "mercedes 200","200 cdi","suv","suvs",
              "benz200","benz 200","benz220","benz 220",
              "benz350","benz 350"),20)
gla$Searchterm <- paste(gla$pre,gla$keyword,gla$post)
gla$Target <- "gla"

#write.csv(gla,"gla.csv")
cla <- kwic(SearchtermCorpus,
            c("cla","cla200","cla 200","cla200d","cla 200d",
              "cla220","cla 220","cla250","cla 250","cla45",
              "cla 45"),20)
cla$Searchterm <- paste(cla$pre,cla$keyword,cla$post)
cla$Target <- "cla"

#write.csv(cla,"cla.csv")
cls <- kwic(SearchtermCorpus,
            c("cls","cls250","cls 250","cls250d","cls 250d",
              "cls350","cls 350","cls500","cls 500"),20)
cls$Searchterm <- paste(cls$pre,cls$keyword,cls$post)
cls$Target <- "cls"

#write.csv(cls,"cls.csv")
glc <- kwic(SearchtermCorpus,
            c("glc","glc220d","glc 220d","glc300d","glc 300d",
              "glc300","glc 300","glc200","glc 200",
              "mercedes amg"),20)
glc$Searchterm <- paste(glc$pre,glc$keyword,glc$post)
glc$Target <- "glc"

#write.csv(glc,"glc.csv")
g63 <- kwic(SearchtermCorpus,
            c("g63","g 63","g55","g 55","gclass","g class",
              "gwagon","g wagon","g500","g 500","gclass",
              "g class"),20)
g63$Searchterm <- paste(g63$pre,g63$keyword,g63$post)
g63$Target <- "g63"

#write.csv(g63,"g63.csv")
gls <- kwic(SearchtermCorpus,
            c("gls","gls200","gls 200","gls240","gls 240",
              "gls350","gls 350","gls350d","gls 350d",
              "gls400","gls 400"),20)
gls$Searchterm <- paste(gls$pre,gls$keyword,gls$post)
gls$Target <- "gls"

#write.csv(gls,"gls.csv")
ml <- kwic(SearchtermCorpus,
           c("mclass","m class","ml benz","m benz","mbenz",
             "ml250","ml 250","ml350","ml 350","mercedes300",
             "mercedes 300","mercedes300d","mercedes 300d",
             "mercedes320","mercedes 320","mercedes320d",
             "mercedes 320d","mercedes350","mercedes 350",
             "ml63","ml 63"),20)
ml$Searchterm <- paste(ml$pre,ml$keyword,ml$post)
ml$Target <- "ml"

#write.csv(ml,"ml.csv")
negative <- kwic(SearchtermCorpus,
                 c("volvo","bus","olx","bicycle","minibus",
                   "truck","bikes","bus price","buses",
                   "used","second hand","rent","job available",
                   "photos","images","old","lease","pre owned",
                   "preowned","careers","pictures","mi","redmi",
                   "xiaomi","resale","vacancy","zoomcar","watch",
                   "watches","biome","leasing","t shirt","tshirt",
                   "tempo","van","vans","travels"),20)
negative$Searchterm <- paste(negative$pre,negative$keyword,negative$post)
negative$Target <- "negative"

#write.csv(negative,"negative.csv")

others <- kwic(SearchtermCorpus,
               c("sls","slc","slc43","slc 43","slk","slk200",
                 "slk 200","slk350","slk 350","slr",
                 "mclaren","maybach","may bach","f015"),20)
others$Searchterm <- paste(others$pre,others$keyword,others$post)
others$Target <- "others"

#write.csv(others,"others.csv")
total <- rbind(sclass,cclass,eclass,aclass,bclass,gle,gla,cla,
               cls,glc,g63,gls,ml,others,negative)

write.csv(total,"total.csv")

sclass1 <- fread("sclass.csv")

km1 <- kmeans(sclass1$Searchterms,k=20,iter.max = 20,nstart = 5)
km1

wtss <- c()
for (i in 2:20){
  km1 <- kmeans(data2,i,iter.max = 10,nstart = 20)
  wtss <- c(wtss,km1$tot.withinss)
}
plot(2:20,wtss)
