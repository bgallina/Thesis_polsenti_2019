###############################################
####### read corpus with named entities #######
###############################################

### read file ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/NER")

### libraries ###

#install.packages("jsonlite")
library(jsonlite)

#Mielőtt beolvasom, txt-ben csere:
#}{"@text" --> },{"@text"
#az elejére és a végére: [ és ]

### Read json file (Spotlight output) ###
ner <- list()
ner <- read_json("./outfile_03_05.txt")

### get names entities fn ###

get_named_entities <- function(nerfile){
  # Minden szövegben előforduló felismert névelemet kicserél a Wikipedian
  # található alakra.
  # Argumentuma egy beolvasott json file.
  lista <- list()
  i <- 0
  j <- 0
  for (i in 1:length(nerfile)){
    voltmar_seged <- character()
    lista[i] <- nerfile[[i]][["@text"]]
    for (j in 1:length(nerfile[[i]][["Resources"]])){
      if (is.null(nerfile[[i]][["Resources"]][[j]][["@surfaceForm"]]) == F){
        if (nerfile[[i]][["Resources"]][[j]][["@surfaceForm"]] %in% voltmar_seged == F){
        lista[i] <- gsub(nerfile[[i]][["Resources"]][[j]][["@surfaceForm"]], 
                         nerfile[[i]][["Resources"]][[j]][["@URI"]],
                         lista[i], fixed = T)
        voltmar_seged <- c(voltmar_seged,nerfile[[i]][["Resources"]][[j]][["@surfaceForm"]])
        }
      }
    }
  }
  return(lista)
}

lista <- get_named_entities(ner)

nevelemek <- data.frame(unlist(lista))

colnames(nevelemek) <- "entities"

nevelemek$entities <- gsub("http://hu.dbpedia.org/resource/", replacement = "", nevelemek$entities)


### Fix mistakes ###

nevelemek$entities <- gsub( "Pécsi_Országos_Színházi_Találkozó", 'poszt', nevelemek$entities)
nevelemek$entities <- gsub( "Automated_Transfer_Vehicle", 'atv', nevelemek$entities)
nevelemek$entities <- gsub( "Ron_Weasley", 'ron', nevelemek$entities)
nevelemek$entities <- gsub( "ROM", 'rom', nevelemek$entities)
nevelemek$entities <- gsub( "TETT", 'tett', nevelemek$entities)
nevelemek$entities <- gsub( "Visz", 'visz', nevelemek$entities)
nevelemek$entities <- gsub( "Ősz", 'ősz', nevelemek$entities)
nevelemek$entities <- gsub( "Agy", 'agy', nevelemek$entities)
nevelemek$entities <- gsub( "1956-os_forradalom", 'forradalom', nevelemek$entities)
nevelemek$entities <- gsub( "1848–49-es_forradalom_és_szabadságharc", 'szabadságharc', nevelemek$entities)
nevelemek$entities <- gsub( "Gáz", 'gáz', nevelemek$entities)
nevelemek$entities <- gsub( "Albánia_statútuma", 'statútum', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(film\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(folyóirat\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(hangszer\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(település\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(folyó\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "Cserna_\\(folyó\\,_Krassó-Szörény_megye\\)", 'cserna', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(televíziós_sorozat\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "Os_\\(telepĂĽlĂ©s\\)", 'os', nevelemek$entities)
nevelemek$entities <- gsub( "Fidesz_â€“_Magyar_PolgĂˇri_SzĂ¶vetsĂ©g", 'Fidesz_Magyar_Polgári_Szövetség', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(film\\,_1988\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(film\\,_1939\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "_\\(film,_\\d*\\)", '', nevelemek$entities)
nevelemek$entities <- gsub( "\\(napilap\\,_1945–1948\\)", '', nevelemek$entities)


nevelemek$entities <- gsub("\\-", "", nevelemek$entities)

nevelemek$entities <- gsub("\\(|\\)", "", nevelemek$entities)

nevelemek$entities <- gsub("[0123456789]_ ", "", nevelemek$entities)
nevelemek$entities <- gsub("[0123456789]_", "", nevelemek$entities)
nevelemek$entities <- gsub("[0123456789]", "", nevelemek$entities)

rm(ner, lista, i, j)


### Write ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


write.csv2(corpus, file = "./78302_clear_withNER.csv",quote = T, fileEncoding = "UTF-8", row.names = F)



corpus <- read.csv2("./78302_clear_withNER.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)
