#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(magrittr)
library(tidyverse)
library(extrafont)
library(ggplot2)
library(ggsoccer)
library(tidyr)
library(cowplot)
library(paletteer)
library(RColorBrewer)
library(reactable)
library(reactablefmtr)
library(lubridate)

# Load datasets

cric<-read.csv("ipl_big.csv")

ipl_res<-cric%>%filter(innings%in%c(1))%>%
    group_by(batting_team,innings,start_date,match_id)%>%
    summarise(runs=sum(runs_off_bat)+sum(extras),balls=n_distinct(ball),
              wickets=n_distinct(player_dismissed)-1)%>%
    arrange(start_date,match_id)%>%
    full_join(cric%>%filter(innings%in%c(2))%>%
                  group_by(batting_team,innings,start_date,match_id)%>%
                  summarise(runs=sum(runs_off_bat)+sum(extras),balls=n_distinct(ball),
                            wickets=n_distinct(player_dismissed)-1)%>%
                  arrange(start_date,match_id),
              by = c('match_id','start_date'))%>%
    as.data.frame()%>%
    arrange(match_id)%>%
    mutate(Winner=ifelse(runs.x>runs.y,batting_team.x,
                         ifelse(runs.x<runs.y,batting_team.y,"Tie")))

knocksdf<-cric%>%filter(is.na(wides))%>%
    group_by(striker,batting_team,match_id,innings,bowling_team)%>%
    summarise(runs=sum(runs_off_bat),
              balls=n_distinct(ball),
              boundaries=sum(runs_off_bat%in%c(4,6)),
              dots=sum(runs_off_bat==0))%>%
    arrange(desc(runs))%>%
    mutate(perc_46=boundaries/balls,
           perc_dot=dots/balls,
           strike_rate=round(runs*100/balls,1))%>%
    #filter(runs>=40)%>%
    as.data.frame()

knocks_full<-knocksdf%>%
    left_join(ipl_res%>%select(match_id,start_date,Winner),by = 'match_id')%>%
    mutate(res=ifelse(batting_team==Winner,"W",
                      ifelse(Winner=="Tie","T","L")))%>%
    filter(innings%in%c(1,2),runs>=40,res%in%c("W","L"))%>%
    mutate(year=lubridate::year(start_date))

knocks_full<-knocks_full%>%
    mutate(desc=paste0(runs," (",balls,") - ",striker," - IPL ",year))

# Write functions

wormplot <- function(desc1,desc2) {
    
    a = knocks_full$desc==desc1
    b = knocks_full$desc==desc2
    
    sd1 = knocks_full$start_date[a]
    str1 = knocks_full$striker[a]
    score1 = knocks_full$runs[a]
    
    sd2 <- knocks_full$start_date[b]
    str2 <- knocks_full$striker[b]
    score2 <- knocks_full$runs[b]
    
    
    
    batsman1 <- cric%>%
        filter(start_date==sd1,
               striker==str1,
               is.na(wides))%>%
        group_by(striker,ball,innings,match_id)%>%
        summarise(runs=cumsum(runs_off_bat))%>%
        ungroup()%>%
        mutate(cum_runs=cumsum(runs),
               inns_balls_faced=seq(from=1,to=length(striker),length.out=length(striker)))%>%
        arrange(innings,ball)%>%
        as.data.frame()
    
    batsman2 <- cric%>%
        filter(start_date==sd2,
               striker==str2,
               is.na(wides))%>%
        group_by(striker,ball,innings,match_id)%>%
        summarise(runs=cumsum(runs_off_bat))%>%
        ungroup()%>%
        mutate(cum_runs=cumsum(runs),
               inns_balls_faced=seq(from=1,to=length(striker),length.out=length(striker)))%>%
        arrange(innings,ball)%>%
        as.data.frame()
    
    batsmen <- rbind(batsman1,batsman2)
    
    batsmen<-batsmen%>%
        left_join(batsmen%>%
                      group_by(striker,match_id)%>%
                      summarise(runx=sum(runs))%>%
                      mutate(uid=paste0(striker," (",runx,")")),
                  by=c('striker','match_id'))
    
    det <- paste0(str1,"'s ",score1," vs ",str2,"'s ",score2)
    
    cricket <- ggplot(data = batsmen)+
        geom_line(aes(x=inns_balls_faced,y=cum_runs,colour=uid),
                  size=1)+
        geom_hline(yintercept = 100,
                   colour="white",
                   linetype=3)+
        scale_color_manual(values = c("#A6AAB0","#FB4447"))+
        labs(title = "Innings Progression",
             subtitle = det)+
        xlab("Innings Balls Faced")+
        ylab("Innings Runs")+
        theme(text = element_text(family = "Helvetica"),
              plot.background = element_rect(fill = "#2A374A"),
              panel.background = element_rect(fill = "#2A374A"),
              panel.grid = element_blank(),
              axis.text = element_text(colour = "white"),
              panel.border = element_rect(colour = "white",fill = NA),
              plot.title = element_text(colour = "white",size = 20,face = "bold"),
              plot.subtitle = element_text(colour = "#A6AAB0",size = 10),
              axis.title = element_text(colour = "white",size = 15),
              legend.background = element_rect(fill = "#2A374A"),
              legend.key = element_rect(fill = "#2A374A"),
              legend.title = element_blank(),
              legend.text = element_text(colour = "white"),
              #legend.position = c(0.1,0.95),
              legend.position = "top",
              legend.direction = "vertical",
              legend.justification = c(1,0),
              legend.margin = margin(0,2,0,1),
              legend.box.margin = margin(0,0,0,0),
              plot.margin = margin(20,40,20,20),
              aspect.ratio = 1)
    
    return(cricket)
}

reactruns<-function(seasons,batsman){
    
    a=cric%>%
        filter(season%in%seasons,
               striker==batsman)%>%
        group_by(striker,bowling_style)%>%
        summarise(total_balls=n())%>%
        left_join(cric%>%
                      filter(season%in%seasons,
                             striker==batsman)%>%
                      group_by(striker,bowling_style)%>%
                      summarise(runs=sum(runs_off_bat)),
                  by = c('striker','bowling_style'))%>%
        mutate(runs_per_over=round(runs*6/total_balls,1))
    
    
    
    c<-reactable(a[,c(2,4,3,5)],
                 defaultSortOrder = "desc",
                 defaultSorted = "runs",
                 columns = list(
                     bowling_style=colDef(name = "Bowling Style"),
                     total_balls=colDef(name = "Balls Faced"),
                     runs=colDef(name = "Runs Scored",
                                 filterable = F,maxWidth = 100),
                     runs_per_over=colDef(name = "Average Runs per Over")
                 ),
                 filterable = F,
                 minRows = 6,
                 defaultPageSize = 6,
                 outlined = T,
                 borderless = F,
                 highlight = T,
                 theme = reactableTheme(
                     headerStyle = list(borderColor = "#555"
                     ),
                     color = "white",
                     borderColor = "hsl(233, 9%, 22%)",
                     backgroundColor = "#2A374A",
                     stripedColor = "white",
                     highlightColor = "hsl(233, 12%, 24%)",
                     cellPadding = "8px 12px",
                     style = list(fontFamily = "Helvetica"),
                     searchInputStyle = list(width = "100%")
                 ))%>%
        add_title("Runs vs. Bowling Styles",
                  align = "center",
                  font_color = "#A6AAB0",
                  font_family = "Helvetica",
                  font_size = 25,
                  font_style = "normal",
                  font_weight = "bold",
                  text_decoration = NULL,
                  background_color = "#2A374A",
                  margin = 0
        )%>%
        add_subtitle(batsman,align = "center",
                     font_color = "#A6AAB0",
                     font_family = "Helvetica",
                     font_size = 15,
                     font_style = "normal",
                     font_weight = "normal",
                     text_decoration = NULL,
                     background_color = "#2A374A",
                     margin = 0
        )
    
    return(c)
    
}

reactwickets<-function(seasons,batsman){
    
    a=cric%>%
        filter(season%in%c(seasons),
               player_dismissed==batsman)%>%
        group_by(player_dismissed,bowling_style)%>%
        summarise(dism=sum(player_dismissed==batsman))%>%
        #filter(player_dismissed!="")%>%
        arrange(desc(dism))%>%
        left_join(cric%>%
                      filter(season%in%c(seasons),
                             striker==batsman)%>%
                      group_by(striker,bowling_style)%>%
                      summarise(total_balls=n()),
                  by = 'bowling_style')%>%
        mutate(balls_per_dismissal=round(total_balls/dism,0))
    
    
    
    c<-reactable(a[,c(2,3,5,6)],
                 defaultSortOrder = "desc",
                 defaultSorted = "dism",
                 columns = list(
                     bowling_style=colDef(name = "Bowling Style"),
                     dism=colDef(name = "Times Dismissed"),
                     total_balls=colDef(name = "Balls Faced"),
                     balls_per_dismissal=colDef(name = "Balls Faced per Dismissal")
                 ),
                 filterable = F,
                 minRows = 6,
                 defaultPageSize = 6,
                 outlined = T,
                 borderless = F,
                 highlight = T,
                 theme = reactableTheme(
                     headerStyle = list(borderColor = "#555"
                     ),
                     color = "white",
                     borderColor = "hsl(233, 9%, 22%)",
                     backgroundColor = "#2A374A",
                     stripedColor = "white",
                     highlightColor = "hsl(233, 12%, 24%)",
                     cellPadding = "8px 12px",
                     style = list(fontFamily = "Helvetica"),
                     searchInputStyle = list(width = "100%")
                 ))%>%
        add_title("Dismissals vs. Bowling Styles",
                  align = "center",
                  font_color = "#A6AAB0",
                  font_family = "Helvetica",
                  font_size = 25,
                  font_style = "normal",
                  font_weight = "bold",
                  text_decoration = NULL,
                  background_color = "#2A374A",
                  margin = 0
        )%>%
        add_subtitle(batsman,align = "center",
                     font_color = "#A6AAB0",
                     font_family = "Helvetica",
                     font_size = 15,
                     font_style = "normal",
                     font_weight = "normal",
                     text_decoration = NULL,
                     background_color = "#2A374A",
                     margin = 0
        )
    
    return(c)
    
}

# Define UI for application 
ui <- fluidPage(
    tags$head(tags$style(HTML('* {font-family: "Helvetica"};'))),
    theme = shinytheme("sandstone"),
    
    navbarPage(
        title = strong("cricket INSIGHTS"),
        id = "main",
        collapsible = T,
        position = "static-top",
        
        
        tabPanel(
            title = strong(tags$p("Home")),
            fluidRow(
                     column(6,
                            imageOutput("landing",hr(),hr(),
                                        width = "100%",
                                        height = "100%",
                                        click = "landing_cl")
                         
                     ),
                     
                     column(6,
                            h1(tags$strong("cricket INSIGHTS")),
                            br(),
                            h5(("This application highlights certain nuances of cricket using data science techniques.")),
                            br(),
                            h5("Two use cases have been developed to analyse ball-by-ball data and generate insights."),
                            br(),
                            h5(tags$b(tags$ol(
                                tags$li("Innings Build Up"),
                                br(),
                                tags$li("Player Match Up")
                            ))),
                            br(),
                            h5("Enter tabs on top left of page to explore the use cases."),
                            br(),
                            h4(tags$b("Acknowledgements, sources, etc.")),
                            br(),
                            h5("Data from the Indian Premier League (IPL) is used for this study."),
                            br(),
                            h5(tags$div(tags$a(href="www.cricsheet.org", "Cricsheet"),"for ball-by-ball data of all IPL matches in history.")),
                            br(),
                            h5(tags$div(tags$a(href="www.espncricinfo.com", "ESPN Cricinfo"),"for extracting metadata of players (batting styles, bowling styles, etc.).")),
                            br(),
                            h5(tags$div("Design elements of the wormplot visualisation used in this tool are inspired by the work of",tags$a(href="https://www.statsperform.com/resource/modelling-cricket-innings-composition/","Stats Perform."))),
                            br(),
                            h4(tags$b("App author")),
                            br(),
                            h5(tags$div("Abhijit Bharali - ",tags$a(href="https://www.abhijitbharali.com", "abhijitbharali.com"),tags$a(href="https://twitter.com/abhibharali", "- Twitter"))),
                            br(),
                         
                     )
            )
                     
        ),
        
        tabPanel(
            title = strong(tags$p("Innings Build Up")),
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    selectInput(inputId = "inn1",
                                choices = unique(knocks_full$desc),
                                label = "Pick an innings:"),
                    br(),
                    selectInput(inputId = "inn2",
                                choices = unique(knocks_full$desc),
                                label = "Pick another one:"),
                    br(),
                    actionButton("compare","Compare!",
                                 style='font-size:100%;
                                    font-weight: bold;
                                    font-family: Helvetica;
                                    text-align: center;
                                    color: white'),
                    br(),
                    br(),
                    h5("Compare the progression of any two 40+ knocks in IPL history."),
                    br(),
                    br(),
                    h5("Visualise how they stack up against each other, in terms of how the two batters paced their innings."),
                    br(),
                    br(),
                    h5(tags$div("Every data point for this analysis has been sourced from",tags$a(href="www.cricsheet.org", "Cricsheet."))),
                
                    
                ),
                mainPanel(
                    width = 9,
                    div(style = "margin:0 ;text-align: left; ",
                        plotOutput(outputId = "wormplot")),
                )
                
                
            )
        ),

        tabPanel(
            title = strong(tags$p("Match Ups")),
            sidebarLayout(
                sidebarPanel(width = 3,
                             selectizeInput(inputId = "batter",
                                            choices = unique(cric$striker),
                                            label = "Winning cause or losing cause"),
                             br(),
                             uiOutput("secondselection"),
                             br(),
                             actionButton("insight","View insights!",
                                          style='font-size:100%;
                                       font-weight: bold;
                                    font-family: Helvetica;
                                    text-align: center;
                                    color: white'),
                             br(),
                             br(),
                             h5("To gain better insight into batsman vs. bowler match ups, we can identify trends from the following tables. Runs scored and dismissal frequency of a batter against particular bowling styles inform match up advantage."),
                             br(),
                             h5(tags$div("Derived metrics like scoring rate and dismissal rate are also included in the visual analysis here. Data sourced and scraped from ",tags$a(href="www.cricsheet.org", "Cricsheet"), "and",tags$a(href="www.espncricinfo.com", "ESPN Cricinfo.")))

                ),
                
                mainPanel(
                    width = 9,
                    div("Runs vs. Bowling Styles", style = "text-align: center; 
                  background-color: #2A374A; color:#A6AAB0; font-size:200%;
                        font-weight: bold"),
                        reactableOutput(outputId = "runs"),
                    hr(),
                    div("Dismissals vs. Bowling Styles", style = "text-align: center; 
                  background-color: #2A374A; color:#A6AAB0; font-size:200%;
                        font-weight: bold"),
                        reactableOutput(outputId = "dismissals"),
                    
                )
                
                
            )
            
        )
        
        
    )
)


# Define server logic required
server <- function(input, output, session) {
    
    x<-eventReactive(input$insight,{
        reactruns(input$season,input$batter)
    })
    
    y <- eventReactive(input$insight,{
        reactwickets(input$season,input$batter)
    })
    
    z <- eventReactive(input$compare,{
        wormplot(input$inn1,input$inn2)
    }
    
    )
    
    output$secondselection<-renderUI({
        checkboxGroupInput("season",
                           "Select seasons:",
                           unique((cric%>%filter(striker==input$batter))$season))
    })
    
    output$landing <- renderImage(
        list(src = "www/cricapp.jpeg", width = "100%"), 
        deleteFile = F)
    

    output$wormplot <- renderPlot({
        z()
    },width = 1000,height = 750,res=96)
    
    output$runs<-renderReactable({
        x()
    })
    
    output$dismissals<-renderReactable({
        y()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
