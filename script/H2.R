
##########################
########## H2 ############
##########################

### read ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus <- read.csv2("./corpus_predicted.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

### libraries ### 

library(ggplot2)
library(questionr)
library(reshape2)


### creating variables ###

ellenzeki <- c('24.hu', '444.hu', 'index.hu', 'nepszava.hu', 'alfahir.hu', 'atv.hu')

for (i in 1:nrow(corpus)){
  if (corpus$source.y[i] %in% ellenzeki){
    corpus$side[i] <- 1 # ellenzéki
  }else{
    corpus$side[i] <- 2 # kormánypárti
  }
}
corpus$side <- as.factor(corpus$side)

corpus$period <- ifelse((corpus$date < as.Date('2018-04-09')), 1, 2)

corpus$period <- as.factor(corpus$period)


### subsets by politicians ###

OrbánV <- data.frame(corpus[corpus$OrbánV==1 & corpus$pred!=0,])
GyurcsányF <- data.frame(corpus[corpus$GyurcsányF==1 & corpus$pred!=0,])
KarácsonyG <- data.frame(corpus[corpus$KarácsonyG==1 & corpus$pred!=0,])
SzélB <- data.frame(corpus[corpus$SzélB==1 & corpus$pred!=0,])
VonaG <- data.frame(corpus[corpus$VonaG==1 & corpus$pred!=0,])

### Odds ratio és khi négyzet ###

get_chi <- function(prediction, groupA, groupB, data){
  chi_ell <- chisq.test(table(groupA[groupB==1], prediction[groupB==1]))
  chi_korm <- chisq.test(table(groupA[groupB==2], prediction[groupB==2]))
  x <- melt(data[,25:27])
  #x <- x[,-3]
  x$value <- as.factor(x$value)
  #df <- table(x)
  #CMH <- mantelhaen.test(df)
  x[,5] <- paste(x[,1], x[,2])
  CMH <- chisq.test(table(x[,5], x$value))
  CV <- cramer(x[,4:5])
  print(df)
  result <- list("Ellenzéki chi p value" = chi_ell$p.value, 
                 "Kormánypárti chi p value" = chi_korm$p.value, 
                 "Chi squared test p value" = CMH$p.value, 
                 "Cramer V" = CV)
  return(result)
}

get_oddsr <- function(prediction, groupA, groupB){
  OR_ell <- odds.ratio(table(groupA[groupB==1], prediction[groupB==1]))
  print(table(groupA[groupB==1], prediction[groupB==1]))
  OR_korm <- odds.ratio(table(groupA[groupB==2], prediction[groupB==2]))
  print(table(groupA[groupB==2], prediction[groupB==2]))
  COR <- OR_ell$OR/OR_korm$OR
  result <- list("Ellenzéki OR" = OR_ell$OR, "Kormánypárti OR" = OR_korm$OR, 
                 "Ellenzéki/Kormánypárti COR" = COR)
  return(result)
}


Orban <- c(get_chi(OrbánV$pred, OrbánV$period, OrbánV$side, OrbánV), 
               get_oddsr(OrbánV$pred, OrbánV$period, OrbánV$side))

Szel <- c(get_chi(SzélB$pred, SzélB$period, SzélB$side, SzélB), 
           get_oddsr(SzélB$pred, SzélB$period, SzélB$side))

Gyurcsany <- c(get_chi(GyurcsányF$pred, GyurcsányF$period, GyurcsányF$side, GyurcsányF), 
          get_oddsr(GyurcsányF$pred, GyurcsányF$period, GyurcsányF$side))

Karacsony <- c(get_chi(KarácsonyG$pred, KarácsonyG$period, KarácsonyG$side, KarácsonyG), 
               get_oddsr(KarácsonyG$pred, KarácsonyG$period, KarácsonyG$side))

Vona <- c(get_chi(VonaG$pred, VonaG$period, VonaG$side, VonaG), 
               get_oddsr(VonaG$pred, VonaG$period, VonaG$side))

eredmeny <- t(cbind(unlist(Vona), unlist(Szel), unlist(Orban), unlist(Gyurcsany), 
               unlist(Karacsony)))

row.names(eredmeny) <- c("Vona Gábor", "Szél Bernadett", "Orbán Viktor", 
                         "Gyurcsány Ferenc", "Karácsony Gergely")

### write results ###

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/results")
write.csv2(eredmeny, file = "./H2.csv",quote = T, fileEncoding = "UTF-8", row.names = T)

get_perc <- function(prediction, who, groupA, groupB, data){
  x <- melt(data[who==1,25:27])
  part <- data.frame(table(x$value, x$side, "period" = x$period))
  n_tab <- table("side" = groupA[who==1], groupB[who==1])
  for (i in 1:nrow(part)){
    if (part$period[i] == 1 ){
      if (part$Var2[i] == 1){
        part$N[i] <- n_tab[1]
      }else{
        part$N[i] <- n_tab[2]
      }
    }else{
      if (part$Var2[i] == 1){
        part$N[i] <- n_tab[3]
      }else{
        part$N[i] <- n_tab[4]
      }
    }
  }
  part$prop <- part$Freq/part$N
  return(part)
}

O_p <- get_perc(corpus$pred, corpus$OrbánV, corpus$side, corpus$period, corpus)
Gy_p <- get_perc(corpus$pred, corpus$GyurcsányF, corpus$side, corpus$period, corpus)
Sz_p <- get_perc(corpus$pred, corpus$SzélB, corpus$side, corpus$period, corpus)
V_p <- get_perc(corpus$pred, corpus$VonaG, corpus$side, corpus$period, corpus)
K_p <- get_perc(corpus$pred, corpus$KarácsonyG, corpus$side, corpus$period, corpus)

### plots ###

o_p_1 <- ggplot(O_p[O_p$Var2==1,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Orbán Viktor - ellenzéki média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

o_p_2 <- ggplot(O_p[O_p$Var2==2,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Orbán Viktor - kormánypárti média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

gy_p_1 <- ggplot(Gy_p[Gy_p$Var2==1,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Gyurcsány Ferenc - ellenzéki média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

gy_p_2 <- ggplot(Gy_p[Gy_p$Var2==2,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Gyurcsány Ferenc - kormánypárti média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

Sz_p_1 <- ggplot(Sz_p[Sz_p$Var2==1,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Szél Bernadett - ellenzéki média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

Sz_p_2 <- ggplot(Sz_p[Sz_p$Var2==2,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Szél Bernadett - kormánypárti média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

V_p_1 <- ggplot(V_p[V_p$Var2==1,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Vona Gábor - ellenzéki média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

V_p_2 <- ggplot(V_p[V_p$Var2==2,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Vona Gábor - kormánypárti média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

K_p_1 <- ggplot(K_p[K_p$Var2==1,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Karácsony Gergely - ellenzéki média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

K_p_2 <- ggplot(K_p[K_p$Var2==2,], aes(x=period, y=prop)) +
  geom_bar(
    aes(fill = Var1),
    stat = "identity",
    position = position_stack(reverse = T))+
  scale_fill_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Karácsony Gergely - kormánypárti média", fill="Szentiment")+ 
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  scale_x_discrete(labels = c("Választások előtt", "Választások után"))

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/script")

source("multiplot_fn.R")

multiplot(o_p_1, gy_p_1, Sz_p_1, K_p_1, V_p_1, o_p_2, gy_p_2, Sz_p_2, K_p_2, V_p_2,
          cols=2)

