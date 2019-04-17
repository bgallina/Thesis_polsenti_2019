##############################
##### preprocess to NER ######
##############################

# Nem tud minden névelemet felismerni a Spotligth
# Pl. nem elég az, hogy "Kunhalmi", a teljes név kell
# Néhány gyakori névelemet manuálisan kapcsolok össze
# A lemmatizáló a pártokhoz kapcsolódó toldalékokat nem ismerte fel, ezeket is javítom


### read ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus <- read.csv2("./filter_corpus_0303_78324.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)



### Remove irrelevant listings ### 
corpus <- corpus[-c(77518, 77541, 77711, 77779, 77809, 77860, 77896, 77031, 
                    77035, 78111, 78019),]

corpus$lemma <- gsub("\\_|\\/|\\(|\\)|\\—|\\.|\\-|\\,", "", corpus$lemma)

###  Fix + preprocess to NER ### 
i <- 0
for (i in 1:length(corpus$lemma)){
  if (corpus$KarácsonyG[i] > 0){
    corpus$lemma[i] <- gsub("karácsony gergely", "Karácsony_Gergely", corpus$lemma[i])
  }
}

i <- 0
for (i in 1:length(corpus$lemma)){
  if (corpus$KarácsonyG[i] > 0){
    corpus$lemma[i] <- gsub("karácsony", "Karácsony_Gergely", corpus$lemma[i])
  }
}

corpus$lemma <- gsub("Karácsony_Gergely", " Karácsony_Gergely ", corpus$lemma)




list <- c("mszppárbeszéd", "mszpp", "mszppm", "mszpps", 
          "mszppárbeszédszövetség", "mszpplista", "mszppliberálisokféle", 
          "mszppábeszéd", "mszppárbeszéd", "mszppárbeszédtől", "mszppt",
          "mszppárbeszédnek", "mszppárbeszédliberális", "mszppmes", 
          "mszppárbeszédes", "mszppárbeszédjelölt")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat

corpus$lemma <- gsub(pat, "MSZP_Párbeszéd", corpus$lemma)

list <- c("orbán viktor ", "orbán ", "miniszterelnök", "kormányfő", 
          "negyedikorbán viktorkormány", "köcsögorbán viktorozás",
          "salviniorbán viktortengely", "protoorbán viktori")

pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Orbán_Viktor", corpus$lemma)
corpus$lemma <- gsub("Orbán_Viktor", "Orbán_Viktor ", corpus$lemma)


list <- c("vona  gábor ", "vona ", "hadházyvona gábor ")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Vona_Gábor ", corpus$lemma)


list <- c("gyurcsány ferenc ", "gyurcsány ", "czeglédygyurcsány ferencügy ")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Gyurcsány_Ferenc ", corpus$lemma)


list <- c("szél bernadett ")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Szél_Bernadett ", corpus$lemma)


list <- c("magyar szocialista párt ", "mszp ", "szocialista párt ")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
list <- c("magyar szocialista párt", "mszp", "szocialista párt", "mszps",
          "mszpt", "mszpn", "mszpről")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Magyar_Szocialista_Párt", corpus$lemma)
library(stringr)
corpus$mszpp <- grepl("MSZP_Párbeszéd", corpus$lemma)

corpus$lemma <- gsub("mszp", "mszp ", corpus$lemma)
corpus$lemma <- gsub("mszp ", "Magyar_Szocialista_Párt ", corpus$lemma)

list <- c("lehet más a politika", "lmp", "lmps", "lmpvel", "lmphez", "lmpből",
          "lmpsek", "lmpt", "lmpről", "lmpn", "lmpnek", "lmpre", "lmpb", "lmpnél",
          "lmpsként", "lmpbe")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Lehet_Más_a_Politika", corpus$lemma)


corpus$lemma <- gsub("lmp", " lmp ", corpus$lemma)
corpus$lemma <- gsub(" lmp ", "Lehet_Más_a_Politika ", corpus$lemma)

list <- c("demokratikus koalíció ", "dk ", "\\( dk \\)", "dk ", "dks ", "dkt", "dktól",
          "dkval")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Demokratikus_Koalíció ", corpus$lemma)

list <- c("demokratikus koalíció", "dks", "dkt", "dktól",
          "dkval")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Demokratikus_Koalíció", corpus$lemma)

corpus$lemma <- gsub("\\( dk \\)", "Demokratikus_Koalíció", corpus$lemma)

corpus$lemma <- gsub("  dk  ", "Demokratikus_Koalíció", corpus$lemma)
corpus$lemma <- gsub("dk ", "Demokratikus_Koalíció ", corpus$lemma)

corpus$lemma <- gsub("Demokratikus_Koalíció", " Demokratikus_Koalíció ", corpus$lemma)

row.names(corpus) <- 1:nrow(corpus)

corpus$lemma <- gsub("jobbik", " jobbik ", corpus$lemma)
list <- c("jobbik", "jobbik magyarországért mozgalom")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Jobbik_Mo_Mozgalom", corpus$lemma)

corpus$lemma <- gsub('Gyurcsány_Ferenc ferenc', "Gyurcsány_Ferenc ", corpus$lemma)

corpus$lemma <- gsub('fideszes', "Fidesz", corpus$lemma)
corpus$lemma <- gsub('fidesz', " Fidesz ", corpus$lemma)
corpus$lemma <- gsub('Fidesztől', "Fidesz", corpus$lemma)


list <- c("keresztény demokrata néppárt", "kdnp", "kdnps", "kdnpt", "kdnpre",
          "kdnptől", "kdnpről", "kdnpn", "kdnpnek", "kdnpből")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "keresztény_demokrata_néppárt", corpus$lemma)

corpus$lemma <- gsub("fideszkdnp", "Fidesz_keresztény_demokrata_néppárt", 
                     corpus$lemma)


list <- c("fiatal demokraták szövetsége", "fidesz", "kormánypárt", 
          "fidesz magyar polgári szövetség", "fideszt", "fideszes", "fideszbe",
          "fideszben", "fideszhez", "fideszből", "fidesztől", "fidesznek", "fideszre")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Fidesz", corpus$lemma)

list <- c("európai unió", "unió", "uniós")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Európai_Unió", corpus$lemma)
list <- c(" eu ")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, " Európai_Unió ", corpus$lemma)


list <- c("egyesült államok")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Egyesült_Államok", corpus$lemma)

list <- c("európa parlament", "európai parlament", "európai parlamenti")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Európai_Parlament", corpus$lemma)

list <- c("európai néppárt")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Európai_Néppárt", corpus$lemma)

list <- c("európai bizottság")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Európai_Bizottság", corpus$lemma)

list <- c("európa tanács")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Európa_Tanács", corpus$lemma)

list <- c("soros györgy", "george soros", "soros gyuri")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Soros_György", corpus$lemma)

list <- c("kásler miklós", "kásler")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Kásler_Miklós", corpus$lemma)

list <- c("gulyás gergely", "gulyás")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Gulyás_Gergely", corpus$lemma)

list <- c("palkovics", "palkovics lászló")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Palkovics_László", corpus$lemma)

list <- c("rogán", "rogán antal")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Rogán_Antal", corpus$lemma)

list <- c("semjén zsolt", "semjén")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Semjén_Zsolt", corpus$lemma)


list <- c("habony árpád", "habony")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Habony_Árpád", corpus$lemma)


list <- c("demeter márta")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Demeter_Márta", corpus$lemma)


list <- c("németh szilárd")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Németh_Szilárd", corpus$lemma)


list <- c("hadházy ákos", "hadházy")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Hadházy_Ákos", corpus$lemma)

list <- c("sallai róbert benedek", "sallai róbert", "sallai róbert")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Sallai_Róbert", corpus$lemma)

list <- c("szijjártó péter", "szijjártó")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Szijjártó_Péter", corpus$lemma)


list <- c("trócsányi lászló", "trócsányi")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Trócsányi_László", corpus$lemma)

list <- c("varga mihály")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Varga_Mihály", corpus$lemma)

list <- c("toroczkai lászló", "toroczkai")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Toroczkai_László", corpus$lemma)

list <- c("novák előd")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Novák_Előd", corpus$lemma)

list <- c("dúró dóra")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Dúró_Dóra", corpus$lemma)


list <- c("feketegyőr andrás", "fekete győr andrás")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Fekete_Győr_András", corpus$lemma)

list <- c("márkizay péter")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Márki_Zay_Péter", corpus$lemma)

list <- c("hollik istván", "hollik")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Hollik_István", corpus$lemma)

list <- c("kósa lajos", "kósa")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Kósa_Lajos", corpus$lemma)

list <- c("lázár jános", "lázár")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Lázár_János", corpus$lemma)


list <- c("simicska lajos", "simicska")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Simicska_Lajos", corpus$lemma)

list <- c("ungár péter", "ungár")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Ungár_Péter", corpus$lemma)

list <- c("\\,", "\\(", "\\)", "\\–")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub("\\,", " ", corpus$lemma)
corpus$lemma <- gsub("\\(", " ", corpus$lemma)
corpus$lemma <- gsub("\\)", " ", corpus$lemma)
corpus$lemma <- gsub("\\;", " ", corpus$lemma)
corpus$lemma <- gsub("\\–", " ", corpus$lemma)
corpus$lemma <- gsub("\\—", " ", corpus$lemma)
corpus$lemma <- gsub("\\.\\.", " ", corpus$lemma)
corpus$lemma <- gsub("\\.\\.\\...", " ", corpus$lemma)


list <- c("stop soros")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Stop_Soros", corpus$lemma)

list <- c("gyetvai viktor")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Gyetvai_Viktor", corpus$lemma)

list <- c("szigetvári viktor")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Szigetvári_Viktor", corpus$lemma)

list <- c("jobbikos")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Jobbik_MO_Mozgalom", corpus$lemma)

corpus$lemma <- gsub("exjobbikos", "ex Jobbik_MO_Mozgalom", corpus$lemma)
corpus$lemma <- gsub("jobbikosodához", "Jobbik_MO_Mozgalom", corpus$lemma)
corpus$lemma <- gsub("jobbikostól", "Jobbik_MO_Mozgalom", corpus$lemma)
corpus$lemma <- gsub("legjobbikosabb", "Jobbik_MO_Mozgalom", corpus$lemma)
corpus$lemma <- gsub("jobbikosodás", "Jobbik_MO_Mozgalom", corpus$lemma)
corpus$lemma <- gsub("főjobbikos", "fő Jobbik_MO_Mozgalom", corpus$lemma)

list <- c("kövér lászló")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Kövér_László", corpus$lemma)

list <- c("botka lászló")
pat <- paste0("\\b(", paste0(list, collapse="|"), ")\\b")    
pat
corpus$lemma <- gsub(pat, "Botka_László", corpus$lemma)

i <- 0
for (i in 1:length(corpus$lemma)){
  if (corpus$Párbeszéd[i] > 0){
    corpus$lemma[i] <- gsub("párbeszéd", "MSZP_Párbeszéd", corpus$lemma[i])
  }
}

corpus <- corpus[,-c(25)]

### write ###

write.csv2(corpus, file = "./78302_clear.csv",quote = T, fileEncoding = "UTF-8", row.names = F)


corpus <- read.csv2("./78302_clear.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)
