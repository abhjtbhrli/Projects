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
library(shinythemes)
# library(mosaic)

# temp <- (360/(length(iplmelt100$Player))/2)
# myAng <- seq(-temp, -360+temp, length.out = length(iplmelt100$Player))
# ang<-ifelse(myAng < -90, myAng+180, myAng)
# ang<-ifelse(ang < -90, ang+180, ang)
# angle =360/(2*pi)*rev(seq( pi/14, 2*pi-pi/14, len=14))

iplmelt100$variable <- gsub(" ","\n",iplmelt100$variable)

# temp <- (360/(length(iplmelt100$Player))/2)
# myAng <- seq(-temp, -360+temp, length.out = length(iplmelt100$Player))
# ang<-ifelse(myAng < -90, myAng+180, myAng)
# ang<-ifelse(ang < -90, ang+180, ang)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #column(12,titlePanel(strong(tags$p("visualising PASSES")))),
    #h3("H3 is fine without tags and so is code here"),
    # tags$head(tags$style(HTML('* {font-family: "Courier"};'))),
    # theme = shinytheme("cyborg"),
    theme = bs_theme(
        bg = "#ffffff", fg = "#666666", primary = "#FFFFFF",
        base_font = font_google("Arial Black"),
        code_font = font_google("Arial Black")),
    #theme = shinytheme("simplex"),
    column(8,offset = 0,titlePanel(strong("batter COMPARISON..."),windowTitle = "IPL cricket comparison app")),
    sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput("b1","Select a player:",levels(levelplayer)),
                     selectInput("b2","Select player to compare:",levels(levelplayer)),
                     actionButton("do","Compare players"),
                     br(),
                     br(),
                     "Comparison sonars of batters in IPL 2020 based on percentile ranks across 6 different metrics.",
                     br(),
                     br(),
                     "Minimum 100 balls faced.",
                     br(),
                     br(),
                     "Data: iplt20.com",
                     br(),
                     br(),
                     img(src="ipl.png",height="50%",width="100%"),
                     ),
        mainPanel(width = 10,
                  plotOutput("polar",height = "750px")
                  )
    )
)

server<-function(input,output,session){
    x<-eventReactive(input$do,{
        iplmelt100%>%filter(Player%in%c(input$b1,input$b2))})
    output$polar<-renderPlot({
        ggplot(data = x(),
               aes(x=variable,y=value, alpha=value))+
            geom_bar(aes(y=100,fill=Player),stat="identity",width=1,colour="white",                 #make the whole pizza first
                     alpha=0.3,show.legend = F)+
            geom_col(aes(fill=Player),width=1,show.legend = F,colour="#ffffff")+
            #geom_bar(stat = "identity",show.legend = F, aes(fill=Player))+
            #geom_bar(stat="identity",width=1,aes(fill=stat),colour="white")+
            scale_fill_manual(values = c("#D70232","#1A78CF"))+
            scale_y_continuous(limits = c(-10,100))+
            #geom_hline(yintercept = seq(0,100,by = 10),colour="black",size=0.05,show.legend = F)+
            #geom_vline(xintercept = seq(0.5,16.5,by=1),colour="black",size=0.05,show.legend = F)+
            coord_polar()+
            facet_wrap(~Player,nrow = 2,ncol = 2)+
            geom_label_repel(data = x(),
                             aes(label=value),force = 2,size=4,box.padding = 0.05,alpha=0.9)+
            #theme_minimal()+
            theme(
                rect = element_rect(fill = "#ffffff"),
                text = element_text(face = "bold", colour = "#d42626",family = "Arial Black"),
                panel.grid = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y.left = element_blank(),
                strip.background = element_rect(fill = "#ffffff"), 
                plot.background = element_rect(fill = "#ffffff"),
                panel.background = element_rect(fill = "#ffffff"),
                strip.text = element_text(colour = "#000000",size = 14),
                plot.title = element_text(hjust = 0.5,face = "bold",size=20,colour = "gray14",
                                          margin =  margin(0, 1, 0, 1, "cm")),
                plot.subtitle = element_text(hjust = 0.5,size = 10,colour = "gray60"),
                plot.caption = element_text(hjust = 0.5,colour = "gray45"),
                axis.text.x = element_text(size=10,face = "plain",
                                           angle = 360/(2*pi)*rev(seq(pi/6, 2*pi-pi/6, len=6)),
                                           colour = "gray60")
            )+
            labs(
                title = " ",
                subtitle = " ",
                caption = "app by @abhibharali, viz ideas from @RobinWilhelmus"
            )
    })
}

#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
