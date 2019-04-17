
#############################################################
############### Text classification with SVM ################
#############################################################

### read corpus ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


corpus <- read.csv2("./78302_clear_withNER_neg_withtraining_lexi.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

corpus$finalsenti <- as.factor(corpus$finalsenti)

row.names(corpus) <- 1:nrow(corpus)

set.seed(15650)

### required packages ###

#install.packages("tm")
library(tm)
install.packages("qdap")
library(qdap)
#install.packages("e1071")
library(e1071)
#install.packages("pROC")
library(pROC)
#install.packages("reshape2")
library(reshape2)

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


MyCorpus <- VCorpus(VectorSource(corpus$cleartext_neg), 
                    readerControl = list(language = "hungarian", 
                                         encoding = "UTF_8"))

##### create DTM #####

DTM  <- DocumentTermMatrix(MyCorpus , 
                           control = list(bounds = list(global=c(2, Inf)), 
                                          weightTfIdf, encoding = 'UTF-8'))

### to bigrams ###

# install.packages("RWeka")
#library(RWeka)
#BigramTokenizer<- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#DTM  <- DocumentTermMatrix(MyCorpus , 
#                           control = list(tokenize = BigramTokenizer, bounds = list(global=c(2, Inf)), 
#                                          weightTfIdf, encoding = 'UTF-8'))

##### split corpus #####

labeled <- row.names(corpus[corpus$senti==F,])
train <- sample(labeled,2000)
validation <- labeled[labeled %in% train == FALSE]

df.train <- corpus[train,]
df.validation <- corpus[validation,]

table(df.train$finalsenti)/2000
table(df.validation$finalsenti)/500

dtm.train <- DTM[train,]
dtm.validation <- DTM[validation,]

### remove sparse terms (keep terms which appear in at least 0.1% of the texts) ###

dtm.train.nb <- removeSparseTerms(dtm.train, 0.999)
terms <- dtm.train.nb[["dimnames"]][["Terms"]]
dtm.validation.nb <- dtm.validation[,terms]

### replace weights with y/n ###

dtm.train.svm <- dtm.train.nb %>% apply(MARGIN=2, FUN=convert_counts)
dtm.validation.svm <- dtm.validation.nb %>% apply(MARGIN=2, FUN=convert_counts)

### dtm modification ###

dtm.train.svm <- as.data.frame(dtm.train.svm)
dtm.validation.svm <- as.data.frame(dtm.validation.svm)

dtm.train.svm1 <- cbind(cat=factor(df.train$finalsenti), dtm.train.svm)
dtm.validation.svm1 <- cbind(cat=as.factor(df.validation$finalsenti), dtm.validation.svm)

### tuning parameters ###

cost <- c(0.1, 1, seq(10,40,10))
gamma <- 10^(-2:1)
svm_tune <- tune(svm, cat~., data=dtm.train.svm1,
                 kernel="radial", scale = F, ranges=list(cost=cost, gamma = gamma))
summary(svm_tune)
print(svm_tune)

df <- svm_tune$performances

df <- df[,-c(4)]

error <- matrix(acast(df, cost ~ gamma, mean, value = 'error'), ncol = gamma, nrow=cost)

error <- data.matrix(data.frame(acast(df, cost ~ gamma, mean, value = 'error')))
                
image(cost, gamma, error,
      xlim = c(min(cost), max(cost)),
      ylim = c(min(gamma), max(gamma)))

# smaller units #

cost <- seq(7, 12, 0.5)
gamma <- seq(0,0.04, 0.01)

svm_tune <- tune(svm, cat~., data=dtm.train.svm1,
                 kernel="radial", scale = F, ranges=list(cost=cost, gamma = gamma))
summary(svm_tune)
print(svm_tune)

df <- svm_tune$performances

df <- df[,-c(4)]

error <- matrix(acast(df, cost ~ gamma, mean, value = 'error'), ncol = gamma, nrow=cost)

error <- data.matrix(data.frame(acast(df, cost ~ gamma, mean, value = 'error')))

image(cost, gamma, error,
      xlim = c(min(cost), max(cost)),
      ylim = c(min(gamma), max(gamma)))


### prediction - RBF ###

model  <- svm(cat~., data = dtm.train.svm1, kernel = "radial",  gamma=0.01, cost = 11.5, epsilon = 0, scale = F) 
pred_RBF <- predict(model, dtm.validation.svm1)

### prediction - Polynomial ###

model  <- svm(cat~., data = dtm.train.svm1, kernel = "polynomial",  degree = 3, coef0=0.5) 
pred_poly <- predict(model, dtm.validation.svm1)

### performance ###

get_performance(pred_RBF, df.validation$finalsenti)

get_performance(pred_poly, df.validation$finalsenti)