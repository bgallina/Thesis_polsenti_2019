###################################
##### accuracy by sample size #####
###################################


### read corpus ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


corpus <- read.csv2("./78302_clear_withNER_neg_withtraining_lexi.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

corpus$finalsenti <- as.factor(corpus$finalsenti)

set.seed(15650)

library(tm)
#install.packages("qdap")
library(qdap)
#install.packages("e1071")
library(e1071)


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

##### split to training and test set #####

labeled <- row.names(corpus[corpus$senti==F,])

samples <- seq(50, 2500, 50)

samples <- rep(samples, 10)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

### get accuracy by diff. sample sizes ###

NB <- function(DTM, corpus, labeled, samples, t_size, sparse, laplace){
  labeled_samp <- sample(labeled,samples)
  train <- sample(labeled_samp, samples*t_size)
  validation <- labeled_samp[labeled_samp %in% train == FALSE]
  df.train <- corpus[train,]
  df.validation <- corpus[validation,]
  dtm.train <- DTM[train,]
  dtm.validation <- DTM[validation,]
  dtm.train.nb <- removeSparseTerms(dtm.train, sparse)
  terms <- dtm.train.nb[["dimnames"]][["Terms"]]
  dtm.validation.nb <- dtm.validation[,terms]
  trainNB <- apply(dtm.train.nb, 2, convert_count)
  validationNB <- apply(dtm.validation.nb, 2, convert_count)
  trainNB_source <- cbind(trainNB, as.factor(df.train$source.y))
  validationNB_source <- cbind(validationNB,as.factor(df.validation$source.y))
  classifier <- naiveBayes(trainNB_source, df.train$finalsenti, laplace = laplace)
  pred <- predict(classifier, newdata=validationNB_source)
  mat <- table("Predictions"= pred,  "Actual" = df.validation$finalsenti )
  accuracy <- sum(diag(mat)) / sum(mat)
  return(accuracy)
}

acc <- list()
for (i in 1:length(samples)){
    acc[i] <- NB(DTM, corpus, labeled, samples[i], 0.8, 0.999, 0.1)
}

result <- data.frame(unlist(acc))
result$sample <- samples

colnames(result) <- c('accuracy', 'sampleN')
library(ggplot2)

ggplot(result, aes(sampleN, accuracy))+
  geom_line()+
  xlab('Sample size')+
  ylab('Accuracy')

