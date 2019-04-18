options(encoding = "UTF-8")

library(shinythemes)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinyjs)
library(lazyeval)

# read corpus:

df <- read.csv("./corpus2.csv", header = T, sep = ";", fileEncoding = 'UTF-8')

#preprocess:


row.names(df) <- 1:nrow(df)

df$date <- gsub("\\/","-", df$publication_time, perl = F)

df$date <- as.Date(df$date, "%Y-%m-%d")

df$pred <- as.factor(df$pred)

df$source <- df$source.y

df$month <- format(df$date,"%m")

df$freq <- 1

df$freq <- as.integer(df$freq)

df$period <- ifelse((df$date < as.Date('2018-04-09')), 1, 2)

df$period <- as.factor(df$period)





m <- list()
for (i in 1:nrow(df)){
  if (df$mszpp[i] == T | df$MSZP[i] == 1 | df$parb[i] == 1 | df$karG[i] == 1){
    m[i] <- 1
  } else {
    m[i] <- 0
  }
}

o <- list()
for (i in 1:nrow(df)){
  if (df$fidesz[i] == 1 | df$KDNP[i] == 1 | df$orbV[i] == 1){
    o[i] <- 1
  } else {
    o[i] <- 0
  }
}


v <- list()
for (i in 1:nrow(df)){
  if (df$Jobbik[i] == 1 | df$VonaG[i] == 1){
    v[i] <- 1
  } else {
    v[i] <- 0
  }
}

l <- list()
for (i in 1:nrow(df)){
  if (df$lmp[i] == 1 | df$szelB[i] == 1){
    l[i] <- 1
  } else {
    l[i] <- 0
  }
}

d <- list()
for (i in 1:nrow(df)){
  if (df$DemKo[i] == 1 | df$gyurcsanyF[i] == 1){
    d[i] <- 1
  } else {
    d[i] <- 0
  }
}

total <- data.frame(cbind(m, o, v, l, d))

total <- data.frame(apply(total, 2, as.numeric))

total$tot <- rowSums(total)

enty <- list()
for (i in 1:nrow(total)){
  if(total$tot[i]== 1){
    if (total$m[i] == 1){
      enty[i] <- "MSZP/Párbeszéd/Karácsony Gergely"
    } else if (total$o[i] ==1) {
      enty[i] <- "Fidesz/KDNP/Orbán Viktor"
    } else if (total$v[i] == 1){
      enty[i] <- "Jobbik/Vona Gábor"
    } else if (total$l[i] == 1){
      enty[i] <- "LMP/Szél Bernadett"
    } else if (total$d[i] == 1){
      enty[i] <- "DK/Gyurcsány Ferenc"
    }
  } else{
    enty[i] <- NA
  }
}

df$enty <- as.character(enty)

df$fidesz <- as.logical(df$fidesz)
df$fidesz <- as.logical(df$lmp)

ellenzeki <<- c('24.hu', '444.hu', 'index.hu', 'nepszava.hu', 'alfahir.hu', 'atv.hu')

for (i in 1:nrow(df)){
  if (df$source[i] %in% ellenzeki){
    df$side[i] <- 1 # ellenzéki
  }else{
    df$side[i] <- 2 # kormánypárti
  }
}
df$side <- as.factor(df$side)

df$p <- as.factor(1)

#print(str(df))
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("Szentiment elemzés online híroldalak cikkein"),
                HTML(paste("Készítette: Gallina Beáta", "2019. április", "<br/>", sep="<br/>")),
                useShinyjs(),
                div(id = "form",
                    sidebarPanel(
                      checkboxGroupInput("source", "Híroldal",
                                         choices = c("24.hu", "444.hu", "index.hu", "origo.hu",
                                                     "alfahir.hu", "atv.hu", "magyaridok.hu",
                                                     "nepszava.hu", "pestisracok.hu")),
                      
                      checkboxGroupInput("enty", "Pártok, politikusok",
                                         choices = c("DK/Gyurcsány Ferenc",
                                                     "Fidesz/KDNP/Orbán Viktor",
                                                     "Jobbik/Vona Gábor",
                                                     "LMP/Szél Bernadett",
                                                     "MSZP/Párbeszéd/Karácsony Gergely")),
                      checkboxGroupInput("side", "Híroldal irányultsága",
                                         choices = c("ellenzéki" = "1", 
                                                     "kormánypárti" = "2"), inline = T),
                      
                      checkboxGroupInput("period", "Időszak",
                                         choices = c("Választások előtt" = "1", 
                                                     "Választások után" = "2"), 
                                         inline = T),
                      checkboxGroupInput("pred", "Szentiment",
                                         choices = c("negatív" = "-1", 
                                                     "semleges" = "0", 
                                                     "pozitív" = "1"), inline = T)
                      
                      
                      
                      
                    ),
                    actionButton("reset_input", "Reset", style="background-color: #00a7b3; border-color: #00a7b3")),
                mainPanel(
                  plotOutput("plot1"),
                  plotOutput("plot2")
                )
                
)
server <- function(input, output) {
  library(ggplot2)
  library(shiny)
  df_subset1 <<- reactive({
    if (is.null(input$source) == TRUE) {
      return(df)
    } else {
      return(filter(df, source %in% input$source))
    }
  })
  df_subset2 <<- reactive({
    if (is.null(input$enty) == TRUE) {
      return(df_subset1())
    } else {
      return(filter(df_subset1(), enty %in% input$enty))
    }
  })
  
  df_subset3 <<- reactive({
    if (is.null(input$side) == TRUE) {
      return(df_subset2())
    } else {
      return(filter(df_subset2(), side %in% input$side))
    }
  })
  
  df_subset4 <<- reactive({
    if (is.null(input$period) == TRUE) {
      return(df_subset3())
    } else {
      return(filter(df_subset3(), period %in% input$period))
    }
  })
  
  df_subset5 <<- reactive({
    if (is.null(input$pred) == TRUE) {
      return(df_subset4())
    } else {
      return(filter(df_subset4(), pred %in% input$pred))
    }
  })
  df_subset <<- reactive({
    if (nrow(df_subset5())==0){
      return(data.frame("date" = as.Date("2018-01-01", "%Y-%m-%d"), "freq" = 0, "pred" =NA))
    }else{
      return(df_subset5())
    }
  })
  
  observeEvent(input$reset_input, {
    reset("form")
  })
  
  df_help <<- reactive({
    return(df)
  })
  
  output$plot1 <- renderPlot({
    p1 <- ggplot(df_subset(), aes(x=date, y=freq)) +
      geom_point(
        aes(colour = pred),
        stat = "identity",
        size = 4,
        position = position_stack(reverse = T)) +
      theme(legend.position = "top") + guides(colour = guide_legend(nrow = 1)) +
      theme(axis.ticks.x = element_blank()) +
      theme(legend.key  = element_blank()) +
      theme(axis.ticks.y  = element_blank())+
      labs(title = "Szentimentek időbeli alakulása", colour="Szentiment")+
      ylab("Szövegek száma") +
      xlab("Hónap (2018)") + 
      theme(plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5)) + 
      scale_colour_manual(
        values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
        labels = c("Negatív", "Semleges", "Pozitív"))+
      scale_x_date(date_breaks = "months" , date_labels = "%m")
    print(p1)
  }, width = 860 )
  
  output$plot2 <- renderPlot({
    pp <- df_subset()['pred']
    dd <- data.frame(table(pp))
    if (nrow(df_subset())==1){
      p2 <- ggplot(data.frame("a" = c("-1","0","1"), "b" = c(0,0,0)), aes(a, b))+
        geom_point()+
        ylab(" ")+
        xlab(" ")+
        theme(axis.text.y = element_blank())+
        theme(axis.ticks.y = element_blank())+
        scale_x_discrete(labels=c("Negatív", "Semleges", "Pozitív"))+
        xlab("Szentimentek")+
        labs(title = "Szentiment orientáció")+ 
        theme(plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5))
    }else{
      p2 <- ggplot(dd ,aes(x=as.factor(pp), y=1, size=ifelse(Freq == 0, NA, Freq))) +
        geom_point(position = position_dodge(1), 
                   aes(color=as.factor(pp)))+
        scale_size(range = c(20, 40))+
        guides(size=FALSE)+
        scale_x_discrete(labels=c("Negatív", "Semleges", "Pozitív"))+
        xlab("Szentimentek")+
        ylab(" ")+
        theme(axis.text.y = element_blank())+
        theme(axis.ticks.y = element_blank())+
        geom_text(aes(label = Freq), size = 4)+
        scale_color_manual(
          values = c("-1" = "#fa8072", "0" = "#999999", "1" = "#8edba3"),
          labels = c("Negatív", "Semleges", "Pozitív"))+
        theme(legend.position = "top") + 
        guides(colour = guide_legend(override.aes = list(size=5), nrow = 1))+
        theme(legend.key  = element_blank())+
        labs(title = "Szentiment orientáció", color="Szentiment")+ 
        theme(plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5))
    }
    print(p2)
  }, height = 255, width = 860 )
  
}
shinyApp(ui = ui, server = server)



