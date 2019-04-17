
##########################################
######## korpusz előfeldolgozása #########
##########################################

### libraries ###

library(ggplot2)
install.packages('syuzhet')
library(syuzhet)
library(stringr)

### read file ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus2<- read.csv2(file = "./corpus.csv",header = T,fileEncoding = "UTF-8")

### Aggregate + split to sentences ###

corpus_char <- data.frame(lapply(corpus2, as.character), stringsAsFactors=FALSE)



corpus_char$con_text <- paste(corpus_char$keywords, corpus_char$subtitle, 
                              corpus_char$abstract, corpus_char$title, corpus_char$body,
                              sep = ".")


corpus_char$len <- sapply(corpus_char$con_text, nchar)

corpus_char$con_text <- gsub("..", replacement = ".", x=corpus_char$con_text, fixed = T)

corpus_char$con_text <- gsub("^[^a-zA-Z]", replacement = "", x=corpus_char$con_text)  

corpus_char$con_text <- corpus_char$con_text <- gsub("\\.(?=[a-zA-Z])", replacement = "\\. ", 
                                                     x=corpus_char$con_text, perl = T)

rm(corpus2)

Get_sentences <- unnest_sentences(corpus_char, output, con_text)

Get_sentences$output <- gsub('\\"|\\„|\\”|\\[|\\]|\\-|\\?|\\!|\\;|\\:|\\>|\\<|\\@|\\&|\\#', replacement = "", x=Get_sentences$output, perl = T)

rm(corpus_char)

### Neccessary for NER ###

Get_sentences$output_sorszam <- paste(row.names(Get_sentences),"_",Get_sentences$output,sep = "")


corpus <- Get_sentences
rm(Get_sentences)
corpus_char <- data.frame(lapply(corpus, as.character), stringsAsFactors=FALSE)

corpus$len <- nchar(corpus$output)

corpus <- corpus[,-c(10:22)]

### To lemmatization ###

corpus_char$len <- nchar(corpus_char$output)

corpus_char <- corpus_char[corpus_char$len > 5,]

corpus$len <- nchar(corpus$output)

corpus <- corpus[corpus$len > 5,]

### Read lemma ###

# after lemma_read.R code 

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/python")

lemmadf <- read.csv2("./lemma.csv", header = T, sep = ";", 
                     fileEncoding = 'UTF-8', stringsAsFactors = F)

corpus$lemma <- lemmadf$lista

corpus$lemma <- tolower(corpus$lemma)

### Removing numbers ###

corpus$lemma <- gsub('[[:digit:]]+', '', corpus$lemma)

rm(lemmadf)

### Filter on entities###

entities <- c("Orbán Viktor", "Orbán", "miniszterelnök ", "Szél Bernadett", "Vona Gábor", "Gyurcsány Ferenc",
              "Karácsony Gergely", "Gyurcsány", "MSZP", "lmp", "Fidesz", "KDNP", "Jobbik", "Párbeszéd",
              "FIDESZ", "DK", "Demokratikus Koalíció", "Vona", "Szél", "Karácsony", "kormánypárt", 
              "Szocialista Párt", "Fidesszel", "Karácsonnyal", "Gyurcsánnyal", "kormányfő")

check <- matrix(ncol = length(entities), nrow = nrow(corpus))

for (i in 1:length(entities)){
  check[,i] <- grepl(entities[i], corpus$output, ignore.case = T)
}
check <- apply(check, 2, as.numeric)
match <- data.frame(check)

rm(check)

# pártok, politikusok többféleképp fordulnak elő, mindet összeadom, majd 
# cserélem egyre, így dummy változók lesznek

colnames(match) <- entities
match$OrbánV <- match[,1] +  match[,2] + match[,3] + match[,26]
match$GyurcsányF <- match[,6] +  match[,8] + match[,25]
match$DemKo <- match[,16] +  match[,17]
match <-match[-c(1,2,3,6,8,16,17, 26, 25)]
colnames(match)
match$fidesz<- match[,6] +  match[,10] +  match[,14] + match[,16]
match <-match[-c(6,10,14,16)]
match$KarácsonyG <- match[,3] +  match[,11] + match[,13]
match$SzélB <- match[,1] +  match[,10]
match$VonaG <- match[,2] +  match[,9]
match$MSZP <- match[,4] +  match[,12]
match <-match[-c(1,2,3,9,10,11,12,13)]

corpus <- cbind(corpus, match)

corpus$output <- gsub("Szél", "Szél Bernadett", corpus$output)
corpus$output <- gsub("Vona", "Vona Gábor", corpus$output)

corpus$SzélB <- as.numeric(grepl("Szél Bernadett", corpus$output))
corpus$VonaG <- as.numeric(grepl("Vona Gábor", corpus$output))

### as dummy ###

corpus[,15:26] <- apply(corpus[,15:26], 2, function(x){
  ifelse(x>0, 1, 0)
})

### fix mistakes ###

parbeszed <- grepl("mszpp", corpus$lemma)
parbeszed <- as.numeric(parbeszed)
corpus$Párbeszéd <- corpus$Párbeszéd+parbeszed
corpus$Párbeszéd <- gsub("[2345689]", "1", corpus$Párbeszéd)
corpus$Párbeszéd <- as.numeric(corpus$Párbeszéd)

corpus$matches <- apply(corpus[15:26], 1, sum)

corpus_orig <- corpus

corpus <- corpus[corpus$matches > 0,]

rm(match, corpus_char, entities, i, parbeszed)

row.names(corpus) <- 1:nrow(corpus)


parb <- str_count(corpus$output, "Párbeszéd")

corpus$Párbeszéd <- parb

corpus$Párbeszéd <- gsub("[2345689]", "1", corpus$Párbeszéd)

corpus$Párbeszéd <- as.numeric(corpus$Párbeszéd)

corpus$matches <- apply(corpus[15:26], 1, sum)

corpus_orig <- corpus

corpus <- corpus[corpus$matches > 0,]

DK <- str_count(corpus$output, "DK")

corpus$DemKo <- DK

corpus$DemKo <- gsub("[2345689]", "1", corpus$DemKo)

corpus$DemKo <- as.numeric(corpus$DemKo)

corpus$matches <- apply(corpus[15:26], 1, sum)

corpus_orig <- corpus

Karácsony <- str_count(corpus$output, "Karácsony")

corpus$KarácsonyG <- Karácsony

corpus$KarácsonyG <- gsub("[2345689]", "1", corpus$KarácsonyG)

corpus$KarácsonyG <- as.numeric(corpus$KarácsonyG)

corpus$matches <- apply(corpus[15:26], 1, sum)

corpus_orig <- corpus


corpus <- corpus[corpus$matches > 0,]

rm(Karácsony)

### More entities with senti inherit ###

# pártok és hozzá tartozó egyéb pártok, politikusok maradnak, ilyenkor minden
# entitásra vonatkozik a szentiment

corpus$inherit <- 0

jobbik <- corpus$Jobbik+corpus$VonaG
mszp <- corpus$MSZP + corpus$Párbeszéd + corpus$KarácsonyG
fidesz <- corpus$KDNP + corpus$fidesz + corpus$OrbánV
demko <- corpus$DemKo + corpus$GyurcsányF
lmp <- corpus$lmp + corpus$SzélB

jobbik[jobbik < 2] <- 0
jobbik[jobbik == 2] <- 1
mszp[mszp < 2] <- 0
mszp[mszp > 1] <- 1
fidesz[fidesz < 2] <- 0
fidesz[fidesz > 1] <- 1
demko[demko < 2] <- 0
demko[demko == 2] <- 1
lmp[lmp < 2] <- 0
lmp[lmp == 2] <- 1
inherit <- demko | fidesz | jobbik | lmp | mszp

corpus$inherit <- inherit
rm(demko, fidesz, inherit, jobbik, lmp, mszp, DK, parb, Karácsony)


### filter on opinionwords ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/dictionaries/senti")
lexicon <- read.csv("./sentilexicon_long.csv", header = T, sep = ",", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

lista <- list()

i <- 0
for (i in 1:length(lexicon$word)){
  lexicon$word[i]
  if (sum(str_count(corpus$lemma, lexicon$word[i]))>0){
    
    lista <- paste(lista, lexicon$word[i], sep = ",")
  }
}
df <- t(data.frame(as.list(strsplit(lista, ",")[[1]])))

df <- data.frame(df[-c(1),])

names(df) <- "sentiwords"

df$sentiwords <- as.character(df$sentiwords)
rm(lista, i)
check <- list()
check <- sapply(corpus$lemma, function(x) any(str_count(x, pattern = df$sentiwords)))

check <- data.frame(check)
corpus$isopinion <- check$check

### Removing duplicates ###

corpus2 <- corpus[!duplicated(corpus[c("output")]),]
rm(check, df, sentiwords, small_corpus, lexicon)

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")
#Duplikátumok kiszedve, lemma, inherit, triggerwords, opinionwords benne:
write.csv2(corpus2, file = "./corpus_checked_0303_91.csv",quote = T, fileEncoding = "UTF-8", row.names = F)


### Stopword elimination ###
# stopword.R code

corpus2$lemma <- lapply(as.character(corpus2$lemma), removeWords, stopwords)

colnames(corpus2)

corpus2 <- corpus2[,-c(2:4,6:8)]

corpus2 <- corpus2[,-c(7)]

rm(lemma, word, stopwords, removeWords)

corpus2$lemma <- as.character(corpus2$lemma)

corpus2$output <- gsub("\\n", replacement = "", x=corpus2$output, fixed = T)

### Filter out rows with more different entities ###

filter_corpus <- corpus2[corpus2$matches == 1,]

ext1 <- corpus2[corpus2$matches==2 & corpus2$inherit==T,]
ext2 <- corpus2[corpus2$MSZP==1 & corpus2$Párbeszéd==1 & corpus2$KarácsonyG ==1 & corpus2$matches==3,]
ext3 <- corpus2[corpus2$fidesz==1 & corpus2$OrbánV==1 & corpus2$KDNP ==1 & corpus2$matches==3,]
ext2[,8:20] <- apply(ext2[,8:20], 2, as.numeric)


#esetleg megmaradt toldalékos alakokat is találja meg:

corpus2$lemma <- gsub("karácsony", "karácsony gergely", corpus2$lemma)
corpus2$lemma <- gsub("miniszterelnökjelölt", "miniszterelnök jelölt", corpus2$lemma)
corpus2$lemma <- gsub("kormánypárt", "fidesz", corpus2$lemma)
corpus2$lemma <- gsub("orbán", "orbán viktor", corpus2$lemma)
corpus2$lemma <- gsub("gyurcsány", "gyurcsány ferenc", corpus2$lemma)
corpus2$lemma <- gsub("szél", "szél bernadett", corpus2$lemma)
corpus2$lemma <- gsub("magyar szocialista párt", "mszp", corpus2$lemma)
corpus2$lemma <- gsub("szocialista párt", "mszp", corpus2$lemma)
corpus2$lemma <- gsub("vona ", "vona gábor ", corpus2$lemma)
corpus2$lemma <- gsub("vonagábor", "vona gábor ", corpus2$lemma)
corpus2$lemma <- gsub("jóbbik", "jobbik", corpus2$lemma)
jobb_wrong <- grepl("jobb[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*", corpus2$lemma)

x <- str_count(corpus2$lemma, "jobbik")
y <- str_count(corpus2$lemma, "jobb")

jobbikbool <- x == 0 & y > 0 & corpus2$Jobbik == 1

corpus2$lemma[jobbikbool==T] <- gsub("jobb", "jobbik", corpus2$lemma[jobbikbool==T])

rm(x, y, jobbikbool)

corpus2$lemma[corpus2$Jobbik == 1 & jobb_wrong==T] <- gsub("jobb ", "jobbik ", corpus2$lemma[corpus2$Jobbik == 1 & jobb_wrong==T])
rm(jobb_wrong)

vona_wrong <- grepl("vona[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*", corpus2$lemma)
corpus2$lemma[corpus2$VonaG == 1 & vona_wrong==T] <- gsub("vona", "vona gábor", corpus2$lemma[corpus2$VonaG == 1 & vona_wrong==T])
corpus2$lemma <- gsub("vona gábor gábor", "vona gábor", corpus2$lemma)
corpus2$lemma <- gsub("vona gábor gábor gábor", "vona gábor", corpus2$lemma)
corpus2$lemma <- gsub("msz-p", "mszp", corpus2$lemma)

corpus2$lemmalen <- nchar(corpus2$lemma)

corpus2 <- corpus2[corpus2$lemma != "null",]
row.names(corpus2) <- 1:nrow(corpus2)

entilemma <- corpus2$lemma

entilemma <- gsub("miniszterelnök[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|lmp[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|jobbik[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|fidesz[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|mszp[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|párbeszéd[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|kdnp[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|szél bernadett[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|orbán viktor[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|karácsony gergely[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|gyurcsány ferenc[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|dk[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|vona gábor[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|kormányfő[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*|kormánypárt[a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]*", "entity", entilemma)

corpus2$entilemma <- entilemma

corpus2$entilemma <- gsub("(entity ?){2,}", "listing ", corpus2$entilemma)

listing <- grepl("listing", corpus2$entilemma)
entity <- grepl("entity", corpus2$entilemma)

corpus2$listing <- listing == T & entity == F

rm(entities, entilemma, i)


ext4 <- corpus2[corpus2$matches > 1 & corpus2$inherit==F & corpus2$listing==T,]

class(filter_corpus$matches)
class(ext1$matches)
class(ext2$matches)
class(ext3$matches)
class(ext4$matches)

filter_corpus <- rbind(filter_corpus, ext1, ext2, ext3, ext4)

rm(ext1, ext2, ext3, ext4)
filter_corpus <- filter_corpus[!duplicated(filter_corpus[c("output")]),]

rm(listing, entity, vona_wrong, corpus_orig)

filter_corpus2 <- filter_corpus

filter_corpus <- filter_corpus[filter_corpus$isopinion==T,]

row.names(filter_corpus) <- 1:nrow(filter_corpus)

filter_corpus$output <- gsub("\\n", " ", filter_corpus$output)

# More listings in one sentence:

filter_corpus$morelistings <- str_count(filter_corpus$entilemma, "listing")


write.csv2(filter_corpus, file = "./filter_corpus_0303_78324.csv",quote = T, fileEncoding = "UTF-8", row.names = F)


corpus <- read.csv2("./filter_corpus_0303_78324.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

# Vizu:


x <- data.frame(table(corpus$publication_time))

ggplot(data=x, aes(x=Var1, y=Freq, group=1)) +
  geom_line()+
  geom_point()


x$Var1 <- as.Date(x$Var1)
colnames(x) <- c('Date', 'Frequency')
ggplot(data=x, aes(x=Date, y=Frequency))+
  geom_line()

