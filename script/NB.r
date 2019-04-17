#############################################################
########### Text classification with Naive Bayes ############
#############################################################

### read corpus ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


corpus <- read.csv2("./78302_clear_withNER_neg_withtraining_lexi.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

corpus$finalsenti <- as.factor(corpus$finalsenti)

set.seed(15650)

### required packages ###

library(tm)
#install.packages("qdap")
library(qdap)
#install.packages("e1071")
library(e1071)
#install.packages("pROC")
library(pROC)



row.names(corpus) <- 1:nrow(corpus)
corpus2 <- corpus

## negations are concatenated with "_", which cannot be handled by DTM --> remove "_" (also "-" due to NER)

corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "nem_",replacement = "nem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "nem ",replacement = "nem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "sem ",replacement = "sem")
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "_–_",replacement = "_")


## Removing named entities
corpus$cleartext_neg <- gsub(x = corpus$cleartext_neg,pattern = "[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*_[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*",replacement = "")

corpus$date <- gsub("/", "-", corpus$publication_time)
corpus$date <- as.Date(corpus$date, "%Y-%m-%d")


corpus$cleartext_neg <- gsub("\\bnem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("\\bsem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("_", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("ellenzéki", "ellenzék", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("választási", "választás", corpus$cleartext_neg)


MyCorpus <- VCorpus(VectorSource(corpus$cleartext_neg), 
                    readerControl = list(language = "hungarian", 
                                         encoding = "UTF_8"))

##### create DTM #####

# install.packages("RWeka")
## to bigrams

#library(RWeka)
#BigramTokenizer<- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

DTM  <- DocumentTermMatrix(MyCorpus , 
                           control = list(bounds = list(global=c(2, Inf)), 
                                          weightTfIdf, encoding = 'UTF-8'))

#DTM  <- DocumentTermMatrix(MyCorpus , 
#                           control = list(tokenize = BigramTokenizer, bounds = list(global=c(2, Inf)), 
#                                          weightTfIdf, encoding = 'UTF-8'))


##### split to training and test set #####

labeled <- row.names(corpus[corpus$senti==F,])
train <- sample(labeled,2000)
validation <- labeled[labeled %in% train == FALSE]
test <- row.names(corpus[corpus$senti==T,])

df.train <- corpus[train,]
df.validation <- corpus[validation,]
df.labeled <- corpus[labeled,]
df.test <- corpus[test,]

table(df.train$finalsenti)/2000
table(df.validation$finalsenti)/500

dtm.train <- DTM[train,]
dtm.validation <- DTM[validation,]
dtm.labeled <- DTM[labeled,]
dtm.test <- DTM[test,]


dim(dtm.train)


#### remove sparse terms (keep terms which appear in at least 0.1% of the texts) ####

dtm.train.nb <- removeSparseTerms(dtm.train, 0.999)
terms <- dtm.train.nb[["dimnames"]][["Terms"]]
dtm.validation.nb <- dtm.validation[,terms]

dtm.labeled.nb <- removeSparseTerms(dtm.labeled, 0.999)
terms <- dtm.labeled.nb[["dimnames"]][["Terms"]]
dtm.test.nb <- dtm.test[,terms]


#### replace freq with y/n ####

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


trainNB <- apply(dtm.train.nb, 2, convert_count)

validationNB <- apply(dtm.validation.nb, 2, convert_count)

labeledNB <- apply(dtm.labeled.nb, 2, convert_count)


trainNB_source <- cbind(trainNB, as.factor(df.train$source.y))
validationNB_source <- cbind(validationNB,as.factor(df.validation$source.y))
labeledNB_source <- cbind(labeledNB,as.factor(df.labeled$source.y))

classifier <- naiveBayes(trainNB_source, df.train$finalsenti, laplace = 0.1)
pred <- predict(classifier, newdata=validationNB_source)


### performance ###

get_performance(pred, df.validation$finalsenti)


### fit model to test set ###

testNB <- apply(dtm.test.nb, 2, convert_count)

testNB_source <- cbind(testNB,as.factor(df.test$source.y))

# Prediction:

classifier <- naiveBayes(labeledNB_source, df.labeled$finalsenti, laplace = 0.1)
pred <- predict(classifier, newdata=testNB_source)

corpus$pred <- corpus$GyurcsányF
corpus$pred[is.na(corpus$finalsenti)==T] <- pred