#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("bslib")
# install.packages("thematic")
library(shiny)
#library(plotly)
library(shinythemes)
library(magrittr)
library(ggplot2)
library(ggsoccer)
library(extrafont)
library(dplyr)
library(tidyverse)
# library(bslib)
# library(thematic)

#thematic::thematic_shiny(font = "auto")

#mapping the instat pitch#
pitch_custom <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.3,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.3,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

#importing the dataset#
islpassultimate<-read.csv("ISL passes.csv")
islpassultimate<-islpassultimate%>%filter(xend!=0,yend!=0)

'%ni%'<-Negate('%in%')

#Defining the functions to use#
kmeansfunc<-function(playername,szn){
  soc1<-islpassultimate%>%filter(str_detect(player,playername),season==szn,
                                 x!=105,y%ni%c(0,68),xend!=105,yend!=68)
  soc1km<-soc1%>%select(id,team,season,event,x,y,xend,yend)
  kmeansoc1<-kmeans(soc1km[,5:8],centers = 6,nstart = 25,iter.max = 30)
  soc1km$clust<-kmeansoc1$cluster
  playerkmeans<-soc1km
  playerkmeans$clustername<-factor(playerkmeans$clust,
                                   levels = c(1,2,3,4,5,6),
                                   labels = c("Cluster 1","Cluster 2","Cluster 3",
                                              "Cluster 4","Cluster 5","Cluster 6"))
  return(playerkmeans)
}

clustermeanfunc1<-function(playerdf){
  socc1<-playerdf[1:6,]
  for (i in 1:nrow(socc1)) {
    socc1$id[i]=0
    socc1$team[i]=paste0("cluster means",i)
    socc1$season[i]="placeholder season"
    socc1$event[i]="placeholder event"
    socc1$x[i]=mean((playerdf%>%filter(clust==i))$x)
    socc1$y[i]=mean((playerdf%>%filter(clust==i))$y)
    socc1$xend[i]=mean((playerdf%>%filter(clust==i))$xend)
    socc1$yend[i]=mean((playerdf%>%filter(clust==i))$yend)
    socc1$clust[i]=i
    socc1$clustername[i]=paste0("Cluster ",i)
  }
  meankmeans<-socc1
  return(meankmeans)
}


ui <- fluidPage(
  #h1("Title without tags$"),
  column(12,titlePanel(strong(tags$p("visualising PASSES")))),
  #h3("H3 is fine without tags and so is code here"),
  tags$head(tags$style(HTML('* {font-family: "Courier"};'))),
  theme = shinytheme("cyborg"),
  
  # App title ----
  #titlePanel("Passing Styles"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      selectizeInput("b1","SELECT PLAYER (or SEARCH):",
                     levels(factor(levels(factor(((islpassultimate%>%group_by(player)%>%summarise(nPass=n()))%>%filter(nPass>100))$player)))),
                     options = list(maxItems = 1,placeholder="Search player")),
      #selectizeInput("b2","Select season:",levels(factor(islpassultimate$season))),
      uiOutput("secondSelection"),
      actionButton("do","SHOW PASS MAPS"),
      br(),
      br(),
      tags$b("This application maps all successful passes made in ISL history, searchable by player and season."),
      br(),
      br(),
      tags$b("",tags$em("PASS CLUSTERS")," tab shows similar passes of a selected player clustered by a ML algorithm, while ",tags$em("PROGRESSIVE PASSES")," tab shows a map of progressive passes made by that player."),
      br(),
      br(),
      tags$b("ISL data sourced from InStat."),
      br(),
      br(),
      img(src="instat logo.png",height="70%",width="100%"),
      
      # Input: Select the random distribution type ----
      # radioButtons("dist", "Distribution type:",
      #              c("Normal" = "norm",
      #                "Uniform" = "unif",
      #                "Log-normal" = "lnorm",
      #                "Exponential" = "exp")),
      # 
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      # sliderInput("n",
      #             "Number of observations:",
      #             value = 500,
      #             min = 1,
      #             max = 1000)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("PASS CLUSTERS", plotOutput("plot1",height = "498px")),
                  tabPanel("PROGRESSIVE PASSES", plotOutput("plot2",height = "498px"))
       )
      #plotOutput("plot1",height = "498px")
      
    )
  )
)

server <- function(input, output, session) {
  
  x<-eventReactive(input$do,{
    #islpassultimate%>%filter(player==input$b1,season==input$b2)
    kmeansfunc(playername = input$b1,szn = input$b2)
  }
    
  )
  
  y<-eventReactive(input$do,{
    #kmeans(x()%>%select(x,y,xend,yend),centers=4,nstart=25,iter.max = 30)
    clustermeanfunc1(x())
  }
    
  )
  
  z<-eventReactive(input$do,{
    islpassultimate%>%mutate(xprog=xend-x)%>%
      filter(player==input$b1,season==input$b2,xprog>=25,x>=16.5,x!=105,y%ni%c(0,68),xend!=105,yend!=68)
  }
    
  )
  
  output$secondSelection<-renderUI({
    selectInput("b2","SEASON:",levels(factor((islpassultimate%>%filter(player==input$b1))$season)))
  })
  
  output$plot1<-renderPlot(({
    ggplot(x())+
      annotate_pitch(dimensions = pitch_custom,colour = "white",fill = "#38383b")+
      theme_pitch()+
      geom_segment(aes(x=x,y=y,xend=xend,yend=yend),lineend = "butt",
                   linejoin = "mitre",alpha=0.5,colour="#74c69d",
                   arrow = arrow(ends = "last",length = unit(0.05,"cm"),
                                 type = "closed"))+
      facet_wrap(~clustername)+
      geom_segment(data = y(),aes(x=x,y=y,xend=xend,yend=yend),lineend = "butt",
                   linejoin = "mitre",
                   arrow = arrow(ends = "last",length = unit(0.20,"cm"),
                                 type = "closed"),
                   colour="red",size=0.8,alpha=0.9)+
      labs(title = "PASS CLUSTERS",subtitle = paste0(input$b1,", ",input$b2),
           caption = "@abhibharali")+
      geom_segment(aes(x=0,y=-3,xend=30,yend=-3),size=1,lineend = "butt",
                   linejoin = "mitre",
                   arrow = arrow(type = "closed",length = unit(0.07,"inches")),
                   colour="azure4")+
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        #strip.background = element_rect(fill = "white"),
        #strip.text = element_text(family = "Segoe UI Semibold",colour = "black",size = 5),
        text = element_text(family = "Courier",colour = "black",hjust = 0),
        plot.title = element_text(family = "Courier",colour = "black",hjust = 0,
                                  face = "bold",size = 20),
        plot.subtitle = element_text(family = "Courier",colour = "yellow",hjust = 0, 
                                     size = 15,color = "slateblue1",face = "bold"),
        plot.caption = element_text(size = 10,face = "bold"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_blank()
        #plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
  }
    
  ))
  
  output$plot2<-renderPlot(({
    ggplot(z())+
      annotate_pitch(dimensions = pitch_custom,colour = "white",fill = "#38383b")+
      theme_pitch()+
      geom_segment(aes(x=x,y=y,xend=xend,yend=yend),lineend = "butt",
                   linejoin = "mitre",alpha=0.9,colour="skyblue4",
                   arrow = arrow(ends = "last",length = unit(0.20,"cm"),
                                 type = "closed"),size=1)+
      labs(title = "PROGRESSIVE PASSES",subtitle = paste0(input$b1,", ",input$b2),
           caption = "@abhibharali")+
      geom_segment(aes(x=0,y=-3,xend=30,yend=-3),size=1,lineend = "butt",
                   linejoin = "mitre",
                   arrow = arrow(type = "closed",length = unit(0.07,"inches")),
                   colour="azure4")+
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        #strip.background = element_rect(fill = "white"),
        #strip.text = element_text(family = "Segoe UI Semibold",colour = "black",size = 5),
        text = element_text(family = "Courier",colour = "black",hjust = 0.5),
        plot.title = element_text(family = "Courier",colour = "black",hjust = 0.5,
                                  face = "bold",size = 20),
        plot.subtitle = element_text(family = "Courier",hjust = 0.5, 
                                     size = 15,color = "skyblue4",face = "bold"),
        plot.caption = element_text(size = 10,face = "bold"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm")
      )
  }))
    
    
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  # d <- reactive({
  #   dist <- switch(input$b1,
  #                  norm = rnorm,
  #                  unif = runif,
  #                  lnorm = rlnorm,
  #                  exp = rexp,
  #                  rnorm)
  #   
  #   dist(input$n)
  # })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  # output$plot <- renderPlot({
  #   dist <- input$dist
  #   n <- input$n
  #   
  #   hist(d(),
  #        main = paste("r", dist, "(", n, ")", sep = ""),
  #        col = "#75AADB", border = "white")
  # })
  # 
  # Generate a summary of the data ----
  # output$summary <- renderPrint({
  #   summary(d())
  # })
  # 
  # # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   d()
  # })
  
}

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application 
 shinyApp(ui = ui, server = server)
# 
