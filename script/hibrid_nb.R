
#############################################################
############# Text classification with lexi+NB #############
#############################################################

### read corpus ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


corpus <- read.csv2("./78302_clear_withNER_neg_withtraining_lexi.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

corpus$finalsenti <- as.factor(corpus$finalsenti)


### read lexicon ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/dictionaries/senti")
lexicon <- read.csv2("./senti_with_negated.csv", header = T, sep = ";", 
                     fileEncoding = 'UTF-8', stringsAsFactors = F)

set.seed(15650)

### required packages ###

library(tm)
install.packages("qdap")
library(qdap)
library(e1071)
library(pROC)
corpus2 <- corpus

### required functions ###

source('fun.R')

### negations are concatenated with "_", which cannot be handled by DTM --> remove "_" (also "-" due to NER) ###

corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "nem_",replacement = "nem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "nem ",replacement = "nem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "sem ",replacement = "sem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "_–_",replacement = "_")

### Removing named entities ###

corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*_[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*",replacement = "")

corpus$date <- gsub("/", "-", corpus$publication_time)
corpus$date <- as.Date(corpus$date, "%Y-%m-%d")
corpus$cleartext_neg <- gsub("\\bnem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("\\bsem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("_", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("ellenzéki", "ellenzék", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("választási", "választás", corpus$cleartext_neg)

##### create DTM #####

MyCorpus <- VCorpus(VectorSource(corpus$cleartext_neg), 
                    readerControl = list(language = "hungarian", 
                                         encoding = "UTF_8"))

DTM  <- DocumentTermMatrix(MyCorpus , 
                           control = list(bounds = list(global=c(2, Inf)), 
                                          weightTfIdf, encoding = 'UTF-8'))

## terms:
terms <- data.frame(DTM[["dimnames"]][["Terms"]])

## negative DTM

lexicon$concat <- gsub("nem_", "nem", lexicon$word)
df <- lookup(DTM[["dimnames"]][["Terms"]], 
             data.frame(lexicon$concat[lexicon$value == -1],
                        lexicon$value[lexicon$value == -1]), missing=NA)

lista <- as.character(unlist(terms[is.na(df)==F,1]))

negdtm <- DTM[,lista]

## positive DTM:

df <- lookup(DTM[["dimnames"]][["Terms"]], 
             data.frame(lexicon$concat[lexicon$value == 1],
                        lexicon$value[lexicon$value == 1]), missing=NA)

lista <- as.character(unlist(terms[is.na(df)==F,1]))

pozdtm <- DTM[,lista]


negdtm <- negdtm * (-1)
inspect(negdtm)

dim(negdtm) + dim(pozdtm)

## find opinion words in DTM:

df <- lookup(DTM[["dimnames"]][["Terms"]], 
             data.frame(lexicon$concat, lexicon$value), missing=NA)

## a végleges DTMben szerepeljenek azok a véleményszavak, melyek a dokuk legalább 0.1%-ában
## a szótárban szereplő szavak közül csak azok, amik a dokuk min. 0.3%-ban

opinion <- as.character(unlist(terms[is.na(df)==F,1]))

biased_dtm <- DTM[,opinion]
dim(biased_dtm)
check_op <- removeSparseTerms(biased_dtm, 0.999)
dim(check_op)
checkterms_op <- check_op[["dimnames"]][["Terms"]]

neutralterms <- as.character(unlist(terms[is.na(df)==T,1]))

neutral_dtm <- DTM[,neutralterms]

check <- removeSparseTerms(neutral_dtm, 0.997)
dim(check)

checkterms <- check[["dimnames"]][["Terms"]]

total_dtm <- DTM[,c(checkterms_op, checkterms)]

inspect(total_dtm)
dim(total_dtm)

rm(biased_dtm, check, negdtm, pozdtm, negscore, neutral_dtm, terms, y, lista, checkterms, df, neutralterms, opinion)

validation <- row.names(corpus[corpus$senti==F,])
train <- sample(row.names(corpus[corpus$senti==T,]),2000)

df.train <- corpus[train,]
df.validation <- corpus[validation,]

dtm.train <- total_dtm[train,]
dtm.validation <- total_dtm[validation,]

dim(dtm.train)
dim(dtm.validation)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

validationNB <- apply(dtm.validation, 2, convert_count)
rm(total_dtm, DTM, train, validation, checkterms, checkterms_op, MyCorpus, dtm.validation, corpus2,
   corpus, check_op, check, lexicon)

trainNB <- apply(dtm.train, 2, convert_count)


### Prediction ###

rm(dtm.train)

#x <- sample(nrow(validationNB), 500)

classifier <- naiveBayes(trainNB, as.factor(df.train$score), laplace = 0.1)
pred <- predict(classifier, newdata=validationNB)

### Performance ###

get_performance(pred, df.validation$finalsenti)

