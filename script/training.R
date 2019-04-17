###############################
##### corpus partitioning #####
###############################

#Training set:

filter_corpus <- read.csv2("./78302_clear_withNER.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

set.seed(1222)

table(filter_corpus$source)

n <- 2000*table(filter_corpus$source)/nrow(filter_corpus)
n

# teljes korpuszbeli arányoknak megfelelően szerepeljenek a trainingben

hu24 <- filter_corpus[filter_corpus$source=="24.hu",]
hu444 <- filter_corpus[filter_corpus$source=="444.hu",]
alfa <- filter_corpus[filter_corpus$source=="alfahir.hu",]
atv <- filter_corpus[filter_corpus$source=="atv.hu",]
index <- filter_corpus[filter_corpus$source=="index.hu",]
magyaridok <- filter_corpus[filter_corpus$source=="magyaridok.hu",]
nepszava <- filter_corpus[filter_corpus$source=="nepszava.hu",]
origo <- filter_corpus[filter_corpus$source=="origo.hu",]
pestisracok <- filter_corpus[filter_corpus$source=="pestisracok.hu",]

row.names(hu24) <- 1:nrow(hu24)
row.names(hu444) <- 1:nrow(hu444)
row.names(alfa) <- 1:nrow(alfa)
row.names(atv) <- 1:nrow(atv)
row.names(index) <- 1:nrow(index)
row.names(magyaridok) <- 1:nrow(magyaridok)
row.names(nepszava) <- 1:nrow(nepszava)
row.names(origo) <- 1:nrow(origo)
row.names(pestisracok) <- 1:nrow(pestisracok)

training_1 <- sample(1:nrow(hu24), round(n[1]))
training_2 <- sample(1:nrow(hu444), round(n[2]))
training_3 <- sample(1:nrow(alfa), round(n[3]))
training_4 <- sample(1:nrow(atv), round(n[4]))
training_5 <- sample(1:nrow(index), round(n[5]))
training_6 <- sample(1:nrow(magyaridok), round(n[6]))
training_7 <- sample(1:nrow(nepszava), round(n[7]))
training_8 <- sample(1:nrow(origo), round(n[8]))
training_9 <- sample(1:nrow(pestisracok), round(n[9]))

training <- rbind(hu24[training_1,], hu444[training_2,], alfa[training_3,],
                  atv[training_4,], index[training_5,], magyaridok[training_6,],
                  nepszava[training_7,], origo[training_8,], pestisracok[training_9,])
training$sorsz <- row.names(training)
table(training$source)
row.names(training) <- 1:nrow(training)
n

rm(training_1, training_2, training_3, training_4, training_5, training_6, 
   training_7, training_8, training_9,n, training_i, hu24, hu444, alfa,
   atv, index, magyaridok, nepszava, origo, pestisracok)


### write ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

write.csv2(training, file = "./training.csv",
           fileEncoding = "UTF-8", row.names = F)


