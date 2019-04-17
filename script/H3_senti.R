########################
#### H3, sentiments ####
########################


setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src")

corpus <- read.csv2("./corpus_predicted.csv", header = T, sep = ";", 
                    fileEncoding = 'UTF-8', stringsAsFactors = F)

### libraries ###

#library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

### create variables ###

corpus$pred <- as.factor(corpus$pred)

ellenzeki <- c('24.hu', '444.hu', 'index.hu', 'nepszava.hu', 'alfahir.hu', 'atv.hu')

for (i in 1:nrow(corpus)){
  if (corpus$source.y[i] %in% ellenzeki){
    corpus$side[i] <- 1 # ellenzéki
  }else{
    corpus$side[i] <- 2 # kormánypárti
  }
}
corpus$side <- as.factor(corpus$side)

corpus$date <- as.Date(corpus$date, "%Y-%m-%d")
corpus$months <- format(corpus$date,"%m")

corpus$months <- as.numeric(corpus$months)

corpus$side <- as.numeric(corpus$side)

########################
######## Fidesz ########
########################

fidesz <- corpus[corpus$fidesz==1,]

### ord. logit ###

f <- polr(pred ~ months, data = fidesz, Hess=TRUE)
summary(f)

(ctable <- coef(summary(f)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(f))

exp(cbind(OR = coef(f), ci))

### test odds proportional assumption ###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(fidesz, summary(as.numeric(pred) ~ months, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

(p_f1 <- plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4])))

newdat <- data.frame(
  months = rep(seq(from = 1, to = 6, length.out = 100), 4))

newdat <- cbind(newdat, predict(f, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = c("months"),
                variable.name = "Level", value.name="Probability")

p_f2 <- ggplot(lnewdat, aes(x = months, y = Probability, colour = Level)) +
  geom_line() +
  scale_colour_manual(
    values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
    labels = c("Negatív", "Semleges", "Pozitív"))+
  labs(title = "Fidesz", fill="Szentiment")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    hjust = 0.5))+
  theme(legend.position = "top")

######################
######## MSZP ########
######################

MSZP <- corpus[corpus$mszpp == 1 | corpus$MSZP == 1 | corpus$Párbeszéd == 1,]

### ord. logit ###

m <- polr(pred ~ months, data = MSZP, Hess=TRUE)
summary(m)

(ctable <- coef(summary(m)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m))

exp(cbind(OR = coef(m), ci))

### test odds proportional assumption ###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(MSZP, summary(as.numeric(pred) ~ months + side, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

(p_m1 <- plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4])))

newdat <- data.frame(
  months = rep(seq(from = 1, to = 6, length.out = 100), 4))

newdat <- cbind(newdat, predict(m, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = c("months"),
                variable.name = "Level", value.name="Probability")

p_m2 <- ggplot(lnewdat, aes(x = months, y = Probability, colour = Level)) +
    geom_line() +
    scale_colour_manual(
      values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
      labels = c("Negatív", "Semleges", "Pozitív"))+
    labs(title = "MSZP", fill="Szentiment")+
    theme(plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5))+
    theme(legend.position = "top")

#######################
######## LMP ##########
#######################

LMP <- corpus[corpus$lmp==1,]

### ord. logit ###

l <- polr(pred ~ months, data = LMP, Hess=TRUE)
summary(l)

(ctable <- coef(summary(l)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(l))

exp(cbind(OR = coef(l), ci))

### test odds proportional assumption ###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(LMP, summary(as.numeric(pred) ~ months, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

(p_l1 <- plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4])))

newdat <- data.frame(
  months = rep(seq(from = 1, to = 6, length.out = 100), 4))

newdat <- cbind(newdat, predict(l, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = c("months"),
                variable.name = "Level", value.name="Probability")

p_l2 <- ggplot(lnewdat, aes(x = months, y = Probability, colour = Level)) +
    geom_line() +
    scale_colour_manual(
      values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
      labels = c("Negatív", "Semleges", "Pozitív"))+
    labs(title = "LMP", fill="Szentiment")+
    theme(plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5))+
    theme(legend.position = "top")

######################
######### DK #########
######################

DK <- corpus[corpus$DemKo == 1,]

### ord. logit ###

d <- polr(pred ~ months, data = DK, Hess=TRUE)
summary(d)

(ctable <- coef(summary(d)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(d))

exp(cbind(OR = coef(d), ci))

### test odds proportional assumption ###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(DK, summary(as.numeric(pred) ~ months, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

(p_d1 <- plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4])))

newdat <- data.frame(
  months = rep(seq(from = 1, to = 6, length.out = 100), 4))

newdat <- cbind(newdat, predict(d, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = c("months"),
                variable.name = "Level", value.name="Probability")

DK$pred <- as.numeric(DK$pred)

anov1 <- aov(pred ~ months, data = DK)
summary(anov1)

DK$pred <- as.factor(DK$pred)

#####################
###### Jobbik #######
#####################

Jobbik <- corpus[corpus$Jobbik==1,]

### ord. logit ###

j <- polr(pred ~ months, data = Jobbik, Hess=TRUE)
summary(j)

(ctable <- coef(summary(j)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))


(ci <- confint(j))

exp(cbind(OR = coef(j), ci))

### test odds proportional assumption ###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(Jobbik, summary(as.numeric(pred) ~ months, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

(p_j1 <- plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4])))

newdat <- data.frame(
  months = rep(seq(from = 1, to = 6, length.out = 100), 4))

newdat <- cbind(newdat, predict(j, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = c("months"),
                variable.name = "Level", value.name="Probability")


Jobbik$pred <- as.numeric(Jobbik$pred)

anov2 <- aov(pred ~ months, data = Jobbik)
summary(anov2)

Jobbik$pred <- as.factor(Jobbik$pred)

setwd("~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/script")
source("multiplot_fn.R")
multiplot(p_f2, p_m2, p_l2, cols = 3)
