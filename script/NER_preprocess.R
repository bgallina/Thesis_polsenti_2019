##################################
##### preprocess to NER code #####
##################################

corpus <- read.csv2("./78302_clear.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)


### remove punct. if some may have been retained ###

corpus_char <- corpus
corpus_char$lemma <- gsub('\\"|\\„|\\”|\\[|\\]|\\-|\\?|\\!|\\;|\\:|\\>|\\<|\\@|\\&|\\#', 
                          replacement = "", x=corpus_char$lemma, perl = T)

corpus_char$lemma_sorsz <- paste(row.names(corpus_char),"_",corpus_char$lemma,sep = "")

z <- gsub("'", replacement = '"', 
          x=paste('curl -K pro.txt --data-urlencode "text=', corpus_char$lemma_sorsz, sep=""))

z <- paste(z, '"', sep="")
z <- paste(z, '>> outfile.txt')
z[1] <- gsub(">>", replacement = ">", x=z[1])
z <- gsub("|", replacement = "", z)
z <- gsub("\\n", replacement = " ", z)
z <- data.frame(z)

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/NER")

### write ###

write.table(z, file = "./finalcode_urlencode_78302.txt",quote = T, sep="|",row.names = F)


#### In outfile fix:
# "curl --> curl
# outfile.txt" --> outfile.txt
# \" --> "
# "[A-Za-z]\"[A-Za-z]" --> ""


### run DBPedia ###