#######################################
### merge training and total corpus ###
#######################################

### read both ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus <- read.csv2("./78302_clear_withNER_neg.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

# kézi kódolás után:
training <- read.csv2("./training_to_read.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

total <- merge(training, corpus, by = "output", all.y = T)

total$senti <- is.na(total$finalsenti)

x <- data.frame(table(training$output))

total <- total[-c(26737, 29489, 49952, 51954),]

total <- total[,-c(2)]

rm(corpus, x, training)

### write ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")
write.csv2(total, file = "./78302_clear_withNER_neg_withtraining.csv",quote = T, fileEncoding = "UTF-8", row.names = F)

