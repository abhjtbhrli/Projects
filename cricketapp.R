#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("mosaic")
# install.packages("shinythemes")
library(magrittr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggrepel)
iplruns<-read.csv("IPL 2020 runs.csv")
iplruns<-iplruns%>%
    mutate(BF.perc=ntile(BF,100))
iplruns<-iplruns%>%
    mutate(SR.perc=ntile(SR,100))
iplruns<-iplruns%>%
    mutate(Avg.perc=ntile(Avg,100))
iplruns<-iplruns%>%
    mutate(X4per100BF=X4s*100/BF)
iplruns<-iplruns%>%
    mutate(X6per100B=X6s*100/BF)
iplruns<-iplruns%>%
    mutate(four.perc=ntile(X4per100BF,100))
iplruns<-iplruns%>%
    mutate(six.perc=ntile(X6per100B,100))
iplruns<-iplruns%>%
    mutate(runs.perc=ntile(Runs,100))
iplshiny<-iplruns%>%
    select(PLAYER,BF.perc,SR.perc,Avg.perc,four.perc,six.perc,runs.perc)
str(iplshiny)
colnames(iplshiny)<-c("Player","Balls","Strike rate","Average","4s/100 balls",
                      "6s/100 balls","Runs")
iplmelt100<-melt(iplshiny%>%filter(Balls>=50),id="Player")
levelplayer<-factor(iplmelt100$Player)

library(shiny)
library(bslib)
# library(shinythemes)
# library(mosaic)

# Define UI for application that draws a histogram
ui <- fillPage(
    theme = bs_theme(
        bg = "#0b3d91", fg = "white", primary = "#FFFFFF",
        base_font = font_google("Space Mono"),
        code_font = font_google("Space Mono")),
    column(8,offset = 4,titlePanel(strong("IPL COMPARISON TOOL"),windowTitle = "IPL cricket comparison app")),
    sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput("b1","Select a player:",levels(levelplayer)),
                     selectInput("b2","Select player to compare:",levels(levelplayer)),
                     actionButton("do","Compare players"),
                     br(),
                     br(),
                     "Comparison chart of IPL 2020 batting stats.",
                     br(),
                     br(),
                     "Data collated from iplt20.com, the official website of 
                     the Indian Premier League.",
                     br(),
                     br(),
                     "Batsmen who have faced at least 100 balls are considered.",
                     br(),
                     br(),
                     ),
        mainPanel(width = 10,
                  plotOutput("polar",height = "100%")
                  )
    )
)

server<-function(input,output,session){
    x<-eventReactive(input$do,{
        iplmelt100%>%filter(Player%in%c(input$b1,input$b2))})
    output$polar<-renderPlot({
        ggplot(data = x(),
               aes(x=variable,y=value, alpha=value))+
            geom_col(aes(fill=Player),alpha=0.8,width=1,show.legend = F)+
            #geom_bar(stat = "identity",show.legend = F, aes(fill=Player))+
            scale_fill_manual(values = c("gold","gray100"))+
            geom_hline(yintercept = seq(0,100,by = 10),colour="black",size=0.05,show.legend = F)+
            geom_vline(xintercept = seq(0.5,16.5,by=1),colour="black",size=0.05,show.legend = F)+
            coord_polar()+
            facet_wrap(~Player,nrow = 2,ncol = 2)+
            geom_label_repel(data = x(),
                             aes(label=value),force = 2,size=3,box.padding = 0.05,alpha=0.8)+
            theme(
                rect = element_rect(fill = "#0b3d91"),
                text = element_text(face = "bold", colour = "white",family = "mono"),
                panel.grid = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y.left = element_blank(),
                strip.background = element_rect(fill = "#0b3d91"), 
                plot.background = element_rect(fill = "#0b3d91"),
                panel.background = element_rect(fill = "#0b3d91"),
                strip.text = element_text(colour = "white",size = 14),
                plot.title = element_text(hjust = 0.5,face = "bold",size=20),
                plot.subtitle = element_text(hjust = 0.5,size = 10),
                axis.text.x = element_text(size=8,face = "bold",angle = 30,colour = "white")
            )+
            labs(
                title = "PLAYER COMPARISON",
                subtitle = "IPL 2020",
                caption = "@abhibharali"
            )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
