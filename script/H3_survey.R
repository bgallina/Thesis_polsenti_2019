#########################
#### H3, survey data ####
#########################


setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")


### read survey data ###

library(openxlsx)
survey <- read.xlsx("./közv_data.xlsx")

### preprocess ###

for (i in 1:nrow(survey)){
  if (survey$party[i] == "Fidesz"){
    survey$sd[i] = 4.3
  }else{
    survey$sd[i] = 4.6
  }
}

nezopont_n <- data.frame(c('január', 'február', 'március', 'április', 'május', 
                           'június', 'július'),
                         c(2000, 2000, 1000, 2000, 2000, 2000, 2000))

colnames(nezopont_n) <- c("time", "N")

publicus_n <- data.frame(c('január', 'február', 'március', 'április', 'május', 
                           'június', 'július'),
                         c(1005, 1002, 1005, 995, 1008, 996, 1010))

colnames(publicus_n) <- c("time", "N")

survey <- merge(nezopont_n, survey, by = "time", all.y = T)

colnames(survey)[2] <- "nezopont_n"

survey <- merge(publicus_n, survey, by = "time", all.y = T)

colnames(survey)[2] <- "publicus_n"

### t test fn ###

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

before <- c('január', 'február', 'március')

survey$when <- ifelse(survey$time %in% before, 1, 0)

### subset ###

fidesz <- survey[survey$party=="Fidesz",]
MSZP <- survey[survey$party=="MSZP",]
DK <- survey[survey$party=="DK",]
LMP <- survey[survey$party=="LMP",]
Jobbik <- survey[survey$party=="Jobbik",]

months <- c('január', 'február', 'március', 'április', 'május', 
            'június', 'július')


### get p values of t test - fn ###

get_p_values <- function(datab, col_m, col_sd, col_n, times){
  dat <- list()
  for (j in 2:length(times)){
    dat[[j]] <- t.test2(datab[datab$time==(times[j-1]), col_m], 
                        datab[datab$time==(times[j]), col_m], 
                      datab[datab$time==(times[j-1]), col_sd], 
                      datab[datab$time==(times[j]), col_sd], 
                      datab[datab$time==(times[j-1]), col_n], 
                      datab[datab$time==(times[j]), col_n])
}
  # return(dat)
  return(lapply(dat, function(x) x[["p-value"]]))

}
library(ggplot2)

### create continous variable from months ###

for (i in 1:nrow(survey)){
  if (survey$time[i] == "január"){
    survey$months[i] <- 1
  } else if (survey$time[i] == "február"){
    survey$months[i] <- 2
  } else if (survey$time[i] == "március"){
    survey$months[i] <- 3
  }else if (survey$time[i] == "április"){
    survey$months[i] <- 4
  }else if ( survey$time[i] == "május"){
    survey$months[i] <- 5
  } else if (survey$time[i] == 'június'){
    survey$months[i] <- 6
  } else{
    survey$months[i] <- 7
  }
}

### plot survey results ###

p1 <- ggplot(survey, aes(x=months, y=nezopont, color=party, group = 1)) +
  geom_point()+ 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),
    labels=c('január', 'február', 'március', 'április', 'május', 
                                          'június', 'július'))+
  scale_color_manual(values = c("Fidesz" = "#ff8d00", 
                                "DK" = "#008080", 
                                "Jobbik" = "#065535",
                                "LMP" = "#00ff7f",
                                "MSZP" = "#990000"))+
  labs(title = "Pártpreferenciák - Nézőpont", fill="Pártok")+
  ylab('')+
  xlab('Hónapok')+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  theme(legend.position = "top")

p2 <- ggplot(survey, aes(x=months, y=publicus, color=party, group = 1)) +
  geom_point()+ 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),
                     labels=c('január', 'február', 'március', 'április', 'május', 
                              'június', 'július'))+
  scale_color_manual(values = c("Fidesz" = "#ff8d00", 
                                "DK" = "#008080", 
                                "Jobbik" = "#065535",
                                "LMP" = "#00ff7f",
                                "MSZP" = "#990000"))+
  labs(title = "Pártpreferenciák - Publicus", fill="Pártok")+
  ylab('')+
  xlab('Hónapok')+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  theme(legend.position = "top")


multiplot(p1, p2)

### get significant changes ###

get_p_values(fidesz, 'nezopont', 'sd', 'nezopont_n', months)
get_p_values(fidesz, 'publicus', 'sd', 'publicus_n', months)

get_p_values(MSZP, 'nezopont', 'sd', 'nezopont_n', months)
get_p_values(MSZP, 'publicus', 'sd', 'publicus_n', months)

get_p_values(LMP, 'nezopont', 'sd', 'nezopont_n', months)
get_p_values(LMP, 'publicus', 'sd', 'publicus_n', months)

get_p_values(DK, 'nezopont', 'sd', 'nezopont_n', months)
get_p_values(DK, 'publicus', 'sd', 'publicus_n', months)

get_p_values(Jobbik, 'nezopont', 'sd', 'nezopont_n', months)
get_p_values(Jobbik, 'publicus', 'sd', 'publicus_n', months)

###

fidesz2 <- aggregate(fidesz, 
          by=list(Category=fidesz$when), FUN=mean)

MSZP2 <- aggregate(MSZP, 
                     by=list(Category=MSZP$when), FUN=mean)

DK2 <- aggregate(DK, 
                     by=list(Category=DK$when), FUN=mean)
LMP2 <- aggregate(LMP, 
                     by=list(Category=LMP$when), FUN=mean)

Jobbik2 <- aggregate(Jobbik, 
                     by=list(Category=Jobbik$when), FUN=mean)

t.test2(fidesz2$nezopont[1], fidesz2$nezopont[2], fidesz2$sd[1], fidesz2$sd[2],
        fidesz2$nezopont_n[1], fidesz2$nezopont_n[2])

t.test2(MSZP2$nezopont[1], MSZP2$nezopont[2], MSZP2$sd[1], MSZP2$sd[2],
        MSZP2$nezopont_n[1], MSZP2$nezopont_n[2])


t.test2(MSZP2$publicus[1], MSZP2$publicus[2], MSZP2$sd[1], MSZP2$sd[2],
        MSZP2$publicus_n[1], MSZP2$publicus_n[2])


t.test2(Jobbik2$nezopont[1], Jobbik2$nezopont[2], Jobbik2$sd[1], Jobbik2$sd[2],
        Jobbik2$nezopont_n[1], Jobbik2$nezopont_n[2])
