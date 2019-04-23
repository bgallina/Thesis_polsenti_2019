 
#############################################################
############# Text classification with lexi+SVM #############
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

dtm.validation.svm <- dtm.validation %>% apply(MARGIN=2, FUN=convert_counts)
rm(dtm.validation, DTM, MyCorpus, lexicon, total_dtm, train, validation, corpus, corpus2, 
   check_op, checkterms, checkterms_op)

dtm.train.svm <- dtm.train %>% apply(MARGIN=2, FUN=convert_counts)
dim(dtm.validation.svm)

dtm.train.svm <- as.data.frame(dtm.train.svm)
dtm.validation.svm <- as.data.frame(dtm.validation.svm)


dtm.train.svm1 <- cbind(cat=factor(df.train$score), src = as.factor(df.train$source.y), dtm.train.svm)
dtm.validation.svm1 <- cbind(cat=as.factor(df.validation$finalsenti), src = as.factor(df.validation$source.y), dtm.validation.svm)

colnames(dtm.train.svm1)


### Prediction ###


svm_tune <- tune(svm, cat~., data=dtm.train.svm1,
                 kernel="radial", scale = F, ranges=list(cost=seq(20,70,10), gamma = 10^(-4:0)))


summary(svm_tune)
print(svm_tune)

#x <- sample(nrow(dtm.validation.svm1), 500)

model  <- svm(cat~., data = dtm.train.svm1, kernel = "radial", cost = 30, gamma = 0.01, epsilon = 0, scale = F) 
fit1.pred <- predict(model, dtm.validation.svm1)

### Performance ###

get_performance(fit1.pred, dtm.validation.svm1$cat)

