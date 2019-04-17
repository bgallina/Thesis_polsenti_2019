###########################
#### fit senti lexicon ####
###########################

### read ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus <- read.csv2("./78302_clear_withNER_neg_withtraining.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)


setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/dictionaries/senti")
lexicon <- read.csv2("./senti_with_negated.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

### libraries ###

library(tm)
install.packages("qdap")
library(qdap)
corpus2 <- corpus

corpus$cleartext_neg <- gsub("\\bnem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("\\bsem\\b", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("_", "", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("ellenzéki", "ellenzék", corpus$cleartext_neg)
corpus$cleartext_neg <- gsub("választási", "választás", corpus$cleartext_neg)

### create DTM ###

MyCorpus <- VCorpus(VectorSource(corpus$cleartext_neg), 
                    readerControl = list(language = "hungarian", 
                                         encoding = "UTF_8"))

DTM  <- DocumentTermMatrix(MyCorpus , 
                           control = list(bounds = list(global=c(2, Inf)), 
                                          weightTfIdf, encoding = 'UTF-8'))

inspect(DTM)

### Most freq terms ###
terms <- data.frame(DTM[["dimnames"]][["Terms"]])

y <-findFreqTerms(DTM, 1500)
y

### check most freq opinionwords ###

check <- matrix(ncol = length(y), nrow = nrow(corpus))

for (i in 1:length(y)){
  check[,i] <- grepl(y[i], corpus$cleartext_neg, ignore.case = T)
}
check <- apply(check, 2, as.numeric)
match <- data.frame(check)

colnames(match) <- y

freq <- apply(match, 2, sum)
freq
freq <- data.frame(freq)

freq$terms <- y

colnames(freq) <- c("FQ", "terms")

newdata <- freq[order(freq$FQ, decreasing = T),]

#Vizu:

plot(head(newdata$FQ))

library(ggplot2)


p <- ggplot(data = newdata[1:30,], aes(x = reorder(terms, FQ), y=FQ)) + 
  geom_bar(stat = "identity")

p + coord_flip() + xlab("Szavak") + ylab("Gyakoriság")

# size: 600*680

rm(freq, newdata, match, p, check, i, y)

############################### negatív DTM ####################################

lexicon$concat <- gsub("nem_", "nem", lexicon$word)
df <- lookup(DTM[["dimnames"]][["Terms"]], 
             data.frame(lexicon$concat[lexicon$value == -1],
                        lexicon$value[lexicon$value == -1]), missing=NA)

lista <- as.character(unlist(terms[is.na(df)==F,1]))

negdtm <- DTM[,lista]
inspect(negdtm)

y <-findFreqTerms(negdtm, 150)
y

### Vizu ###

check <- matrix(ncol = length(y), nrow = nrow(corpus))

for (i in 1:length(y)){
  check[,i] <- grepl(y[i], corpus$cleartext_neg, ignore.case = T)
}
check <- apply(check, 2, as.numeric)
match <- data.frame(check)

colnames(match) <- y

freq <- apply(match, 2, sum)
freq
freq <- data.frame(freq)

freq$terms <- y

colnames(freq) <- c("FQ", "terms")

newdata <- freq[order(freq$FQ, decreasing = T),]


p <- ggplot(data = newdata[1:30,], aes(x = reorder(terms, FQ), y=FQ)) + 
  geom_bar(stat = "identity", fill = "#ff7f50")



############################## pozitív DTM #####################################

df <- lookup(DTM[["dimnames"]][["Terms"]], 
             data.frame(lexicon$concat[lexicon$value == 1],
                        lexicon$value[lexicon$value == 1]), missing=NA)

lista <- as.character(unlist(terms[is.na(df)==F,1]))

pozdtm <- DTM[,lista]
inspect(pozdtm)

y_p <-findFreqTerms(pozdtm, 150)
y_p

### Vizu ###
rm(check)
check <- matrix(ncol = length(y_p), nrow = nrow(corpus))

i <- 0
for (i in 1:length(y_p)){
  check[,i] <- grepl(y_p[i], corpus$cleartext_neg, ignore.case = T)
}
check <- apply(check, 2, as.numeric)
match_p <- data.frame(check)

colnames(match_p) <- y_p

freq_p <- apply(match_p, 2, sum)
freq_p
freq_p <- data.frame(freq_p)

freq_p$terms <- y_p

colnames(freq_p) <- c("FQ", "terms")

freq_p <- freq_p[freq_p$terms != "ász",]

newdata_p <- freq_p[order(freq_p$FQ, decreasing = T),]


q <- ggplot(data = newdata_p[1:30,], aes(x = reorder(terms, FQ), y=FQ)) + 
  geom_bar(stat = "identity", fill = "#709963")


p + coord_flip() + xlab("Szavak") + ylab("Gyakoriság")
q + coord_flip() + xlab("Szavak") + ylab("Gyakoriság")


rm(newdata, newdata_p, y, q, p, match, match_p, freq, freq_p, y, y_p, lista, df,
   terms, i, check)


### pos. score ###

pozscore <- data.frame(slam::row_sums(pozdtm, na.rm = T))

corpus$pozscore <- pozscore$slam..row_sums.pozdtm..na.rm...T.


### neg. score ###

negscore <- data.frame(slam::row_sums(negdtm, na.rm = T))

corpus$negscore <- negscore$slam..row_sums.negdtm..na.rm...T.*(-1)


############################# score #########################################


for (i in 1:nrow(corpus)){
  if (corpus$pozscore[i] > abs(corpus$negscore[i])){
    corpus$score[i] <- 1
  } else if (corpus$pozscore[i] == abs(corpus$negscore[i])){
    corpus$score[i] <- 0
  }else{
    corpus$score[i] <- -1
  }
}

for (i in 1:nrow(corpus)){
  if (corpus$score[i] > 0){
    corpus$label[i] <- "positive"
  } else if (corpus$score[i] == 0){
    corpus$label[i] <- "neutral"
  } else{
    corpus$label[i] <- "negative"
  }
}

### Performance ###

x <- sample(nrow(corpus[corpus$senti == F,]), 500)

get_performance(corpus$score[corpus$senti == F], corpus$finalsenti[corpus$senti == F])
get_performance(corpus$score[x], corpus$finalsenti[x])


### write ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")
write.csv2(corpus, file = "./78302_clear_withNER_neg_withtraining_lexi.csv",quote = T, fileEncoding = "UTF-8", row.names = F)



corpus$dates <- gsub("/", "\\-", corpus$publication_time)

corpus$dates <- as.Date(corpus$dates, "%Y-%m-%d")

library(ggplot2)
corpus$publication_time
ggplot(corpus, aes(dates)) + 
  geom_bar(aes(fill=label), width = 0.5) + 
  scale_fill_manual(values=c("#ff7f50", "#708090", "#709963"))

