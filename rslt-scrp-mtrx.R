# Web scrape the data from _transfermarkt.com_ and do a full-stack approach to building the viz.

# I walk through method #2 here as it is computationally more challenging.

# The _transfermarkt.com_ link for the results to visualise is [this](https://www.transfermarkt.co.in/indian-super-league/gesamtspielplan/wettbewerb/IND1?saison_id=2019), a complete list of results in the 2019-20 Indian Super League league phase.

# We use the `rvest` package to scrape text from the page. For visualisation, we use the `ggplot2` package. Other packages in use are `magrittr` for piping, `stringr` for text cleaning and `extrafont` for using custom fonts.

# Scrape the page and store it to a variable (sp in this example)

sp <- read_html("https://www.transfermarkt.co.in/indian-super-league/gesamtspielplan/wettbewerb/IND1?saison_id=2019")

# Generate a dataframe of the scraped data and store it to a variable (league in this example)

league <- sp%>%html_nodes(".hauptlink , .hauptlink .tooltipstered , td.hide-for-small:nth-child(1)")%>%
  html_text()%>%
  str_replace_all("[\t\n]", "")%>%
  as.data.frame()
  
# Clean the league dataframe and convert it to a readable format (table dataframe in this example)

colnames(league)="date"
league$home<-league$date
league$result<-league$home
league$away<-league$home

for (i in 0:89) {
  league$date[4*i+2]=""
  league$date[4*i+3]=""
  league$date[4*i+4]=""
  league$home[4*i+1]=""
  league$home[4*i+3]=""
  league$home[4*i+4]=""
  league$result[4*i+1]=""
  league$result[4*i+2]=""
  league$result[4*i+4]=""
  league$away[4*i+1]=""
  league$away[4*i+2]=""
  league$away[4*i+3]=""
}

table <- data.frame(
  date=(league%>%filter(date!=""))$date,
  home=(league%>%filter(home!=""))$home,
  result=(league%>%filter(result!=""))$result,
  away=(league%>%filter(away!=""))$away
  )


table <- table%>%
  separate(col = date,into = c("day","date"),sep = "     ")

table$result2 <- table$result
table <- table%>%
  separate(result2,c("hgoal","agoal"),sep = ":")


table$home <- gsub('([(]+[0-9]+[.]+[)])','',table$home)
table$away <- gsub('([(]+[0-9]+[.]+[)])','',table$away)

# remove unwanted trailing & leading whitespaces in the table dataframe

table$home <- str_trim(table$home,side = "both")
table$away <- str_trim(table$away,side = "both")
table$date <- str_trim(table$date,side = "both")

# convert hgoal & agoal to numbers (first convert to factors to override NA coercion error)

table$hgoal <- as.factor(table$hgoal)
table$hgoal <- as.numeric(table$hgoal)

table$agoal <- as.factor(table$agoal)
table$agoal <- as.numeric(table$agoal)

table$hgoal=table$hgoal-1
table$agoal=table$agoal-1


glimpse(table)

head(table%>%as_tibble())

# Visualise the data and save the plot to local machine using `ggsave`

ggplot(table,aes(x=away,y=home,fill=hgoal-agoal))+
  geom_tile(alpha=0.5)+
  geom_text(aes(label=paste(hgoal,agoal,sep = "-"),
                family="Helvetica",size=hgoal+agoal),
            colour="black",
            show.legend = F)+
  scale_fill_gradient2(low = "red",high = "green",midpoint = 0,guide = FALSE)+
  scale_x_discrete(limits = levels(table$home), position = "top") + 
  scale_y_discrete(limits = rev(levels(table$away)))+
  labs(title="ISL 19-20 RESULT MATRIX",
       caption="darker green = more emphatic home win\ndarker red = more emphatic away win")+
  xlab("AWAY")+
  ylab("HOME")+
  theme(text = element_text(family = "Helvetica"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 18,hjust = 0,family = "Helvetica",
                                  colour = "midnight blue",face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = 7))+
  ggsave("result matrix",dpi = 300, device = "png",width = 11.27,height = 8.27)
