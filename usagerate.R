#p2 <- ggplot(islbig%>%filter(season=="19-20",position!="G")%>%top_n(20,usage_rate),aes(x=usage_rate,y=reorder(player_name,usage_rate),label=usage_rate))
#islbig <- read_excel("C:/Users/AB/Documents/R/testshiny/www/ISL data all.xlsx",sheet = "bigdata")
#islbig <- read_csv("C:/Users/AB/Documents/R/testshiny/islbig.csv")
#library(utils)
#islbig <- read_csv("islbig.csv")
#library(readxl)
#islbig <- read_excel("islbig.xlsx")

ui <- fluidPage(
       theme = shinythemes::shinytheme("simplex"),
       titlePanel(strong("Indian Super League")),
       #p(strong("bold font "), em("italic font")),
       #h2(id="big-heading", "INDIAN SUPER LEAGUE"),
       #tags$style(HTML("#big-heading{color: steelblue;}")),
       #titlePanel( div(HTML("Old <em>Faithful Geyser</em> Data"))),
       sidebarLayout(
             sidebarPanel(
                   selectInput("season","Choose season:",c(islbig$season)),
                   selectInput("position","Select player position:",c(islbig$position)),
                   actionButton("do","Show data"),
                   br(),
                   br(),
                   br(),
                   "ISL data sourced from sofascore.com",
                   br(),
                   br(),
                   img(src="sofascore.png", height=70,width=200),
                   br()
               ),
            mainPanel(
                  plotOutput("bar")
              )
        ),
       sidebarLayout(
         sidebarPanel(
           "Usage Rate is a concept borrowed from basketball analysis. According to Martin Hawkes-Teeter (@HawkesTeeter, the inspiration for this analysis), Usage Rate is an attempt to measure the influence each player has in a team's attack by looking at the percentage of a team's possessions he uses, or has the final action of a possession, weighted by minutes played.",
           br(),
           br(),
           "Success Rate is a by-product of Usage Rate, wherein a player's actual influence on his team's attack is measured, and how much success he has had in turning a possession sequence into a completed attacking move.",
           br(),
           br(),
           "Both Usage Rate and Success Rate are percentage values. The labels next to the bars are percentages of a team's possession used by the player.",
           br(),
           br(),
           "All data are from the three 10-team seasons of the Indian Super League. Pardon me for the Comic Sans charts."
         ),
         mainPanel(
           plotOutput("bar2")
         )
       )
   )

server <- function(input,output,session){
       x <- eventReactive(input$do,{
             islbig%>%filter(islbig$season==input$season,islbig$position==input$position)%>%top_n(15,usage_rate) })
       output$bar <- renderPlot({ggplot(x(),aes(x=usage_rate,y=reorder(player_name,usage_rate),label=usage_rate))+geom_bar(stat = "identity",colour="dark blue",fill="steelblue",alpha=0.5)+geom_text(aes(label=round(usage_rate*100,1),hjust=-0.2,family="Comic Sans MS"),position = position_dodge(width = 0.2))+labs(title = "HIGH USAGE PLAYERS", subtitle = "Most involved players in end actions of possession sequences",x="Usage Rate",y="",caption = "@abhibharali")+theme_minimal()+theme(text = element_text(family = "Comic Sans MS"))
       })
       
       output$bar2 <- renderPlot({
         ggplot(x(),aes(x=positive_possessions/total_possessions,y=reorder(player_name,positive_possessions/total_possessions),label=positive_possessions/total_possessions))+geom_bar(stat = "identity",colour="dark blue",fill="dark blue",alpha=0.5)+geom_text(aes(label=round(positive_possessions*100/total_possessions,1),hjust=-0.2,family="Comic Sans MS"),position = position_dodge(width = 0.2))+labs(title="HIGH EFFICIENCY PLAYERS", subtitle = "Players with high offensive output",x="Success Rate (in possession)",y="",caption = "@abhibharali")+theme_minimal()+theme(text = element_text(family = "Comic Sans MS"))
       
       })
}
       
       
shinyApp(ui,server)
