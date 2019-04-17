#### JSON ####

# Fájlok beolvasása, data frame-be alakítása.

#### libraries ####

install.packages("rjson")

library(rjson)

#### fájlok ####

setwd("C:/Users/user/Documents/Egyetemi/survey/Szakdolgozat/src")

files_json <- dir(path = "./corpus/",
                  pattern = ".json",
                  full.names = T,
                  recursive = T)

#### beolvasás ####

file_list <- list()
file_df <- data.frame()
files_df <- data.frame()

for ( i in 1:length(files_json)) {
  file_list <- fromJSON(file = files_json[i],simplify = TRUE)
  file_df <- do.call(rbind,file_list)
  files_df <- rbind(files_df,file_df)
}

rm(files_json,file_list,file_df,i)

#### df elemek listából kibontása, összefűzése ####

kibonto <- function(x,coll) {
  paste(unlist(x,use.names = FALSE),collapse = coll)
}

files_df$body <- sapply(files_df$body,kibonto, coll = ' ')
files_df$source <- sapply(files_df$source,kibonto, coll = ',')
files_df$sourceUrl <- sapply(files_df$sourceUrl,kibonto, coll = ',')
files_df$publication_time <- sapply(files_df$publication_time,kibonto, coll = ',')
files_df$title <- sapply(files_df$title,kibonto, coll = ',')
files_df$subtitle <- sapply(files_df$subtitle,kibonto, coll = ',')
files_df$authors <- sapply(files_df$authors,kibonto, coll = ',')
files_df$abstract <- sapply(files_df$abstract,kibonto, coll = ',')
files_df$keywords <- sapply(files_df$keywords,kibonto, coll = ',')
files_df$images <- sapply(files_df$images,kibonto, coll = ',')


rm(kibonto)

#### dátum formátum ####

files_df$publication_time <- sapply(files_df$publication_time,
                                    gsub,pattern = "-", replacement = "/")


colnames(files_df)
corpus <- files_df[c(1:3,4:9)]

corpus$images_html <- sapply(corpus[2], as.character)

### kimentés ###

write.csv(corpus, file = "./corpus/corpus.csv",quote = T, fileEncoding = "UTF-8", row.names = F)
