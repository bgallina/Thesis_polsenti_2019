#################################
##### read lemmatized corpus ####
#################################

### read file ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/python/lemma/final")

lemma <- read.table('./imput.csv', header = F, sep = "|", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F, quote = "")

lemma_imp <- read.table('./imput_275805.csv', header = F, sep = "|", 
                        fileEncoding = 'UTF-8', stringsAsFactors = F, quote = "")

total_lemma <- rbind(lemma, lemma_imp)

lista<-list()

### aggregate by sentences ###

for (i in 1:nrow(total_lemma)){
  lista[total_lemma$V2[i]+1] <- paste(lista[total_lemma$V2[i]+1],total_lemma$V1[i])
}
head(lista)


rm(lemma, lemma_imp)

lista <- gsub("NULL ", replacement = "", lista)

lemmadf <- data.frame(lista)

### Fix ###

# (Tordai helyett hibásan Lovastanyait ad)
lemmadf$lista <- gsub("Lovastanyai", replacement = "Tordai", lemmadf$lista)

rm(lista)

### write ###

write.csv2(lemmadf, file = "./lemma.csv",quote = T, fileEncoding = "UTF-8", row.names = F)

