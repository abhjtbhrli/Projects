# Sports data visualisations, models, applications

[Abhijit Bharali](https://abhijitbharali.com/) | [Twitter: `@abhibharali`](https://twitter.com/abhibharali)

## Visualisations

### Visualise innings progression in cricket matches

This is a recreation of a dataviz used by [Stats Perform](https://www.statsperform.com/resource/modelling-cricket-innings-composition/) in their cricket data science related projects. The visualisation showcases how batters pace their innings in a match, highlighting that not all runs are scored similarly. [View my full project documentation here.](https://github.com/abhjtbhrli/Projects/blob/master/Innings-progression.md)

The viz goes like...

![opta](https://user-images.githubusercontent.com/37649445/126365025-6bcfea49-2d45-4a06-b81f-112a7301c60c.png)

_______________________

### Visualise football match results in a matrix using matchsheet data from Transfermarkt.com

*** **Method 1** ***

[Copy-paste the data from _transfermarkt.com_ in local machine and generate the viz.](https://github.com/abhjtbhrli/Projects/blob/master/resultmatrix.R)

*** **Method 2** *** ⬇️ ⬇️ **tutorial**⬇️ ⬇️

[Web scrape the data from _transfermarkt.com_ and do a full-stack approach to building the viz.](https://github.com/abhjtbhrli/Projects/blob/master/rslt-scrp-mtrx.R)

_______________________ 

I walk through method #2 here as it is computationally more challenging.

The _transfermarkt.com_ link for the results to visualise is [this](https://www.transfermarkt.co.in/indian-super-league/gesamtspielplan/wettbewerb/IND1?saison_id=2019), a complete list of results in the 2019-20 Indian Super League league phase.

The page shows like this...

![Screenshot 2021-07-20 at 6 14 39 PM](https://user-images.githubusercontent.com/37649445/126326223-4af082d6-3ecf-4ef0-ac76-56964bfc3a46.png)

We use the `rvest` package to scrape text from the page. For visualisation, we use the `ggplot2` package. Other packages in use are `magrittr` for piping, `stringr` for text cleaning and `extrafont` for using custom fonts.

```
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

table<-data.frame(
  date=(league%>%filter(date!=""))$date,
  home=(league%>%filter(home!=""))$home,
  result=(league%>%filter(result!=""))$result,
  away=(league%>%filter(away!=""))$away
  )


table<-table%>%
  separate(col = date,into = c("day","date"),sep = "     ")

table$result2<-table$result
table<-table%>%
  separate(result2,c("hgoal","agoal"),sep = ":")


table$home<-gsub('([(]+[0-9]+[.]+[)])','',table$home)
table$away<-gsub('([(]+[0-9]+[.]+[)])','',table$away)

# remove unwanted trailing & leading whitespaces in the table dataframe

table$home<-str_trim(table$home,side = "both")
table$away<-str_trim(table$away,side = "both")
table$date<-str_trim(table$date,side = "both")

# convert hgoal & agoal to numbers (first convert to factors to override NA coercion error)

table$hgoal<-as.factor(table$hgoal)
table$hgoal<-as.numeric(table$hgoal)

table$agoal<-as.factor(table$agoal)
table$agoal<-as.numeric(table$agoal)


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
        plot.title = element_text(size = 15,hjust = 0.5,family = "Helvetica"),
        plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = 7))+
  ggsave("result matrix",dpi = 300, device = "png",width = 11.27,height = 8.27)
```

The final result looks like this...

![result matrix](https://user-images.githubusercontent.com/37649445/126327305-c618059f-f881-4a20-b1d5-9d22034b087d.png)

This is an example of a full-stack sports data visualisation approach using `R`, wherein we extract the data from the web using scraping techniques, clean and wrangle the extracted data to a usable format and then visualise the data to generate insights. 

_______________________

### Lollipops to visualise starting lineup changes of football teams

One of the most important aspects of performance analysis in any sport is opposition analysis. This lollipop chart visualisation provides the starting point for opposition analysis for football teams, highlighting the changes made to a team's starting XI on a per-game basis.

[Here's the source code](https://github.com/abhjtbhrli/Projects/blob/master/Upset%20Plot%20Hack.md) for automating lineup information using the `worldfootballR` package for extracting the required data and `ggplot2` for visual representation and analysis.

The end product looks like this...

![KB](https://user-images.githubusercontent.com/37649445/126359741-beebcf9d-37e3-4dc8-97c1-ea6cbb846169.png)

_______________________

### Comparison viz - Radar Plots/Spider Graphs

A widely used visualisation tool to compare players and teams. Its creation is aided by the `fmsb` package. [View my full project documentation here.](https://github.com/abhjtbhrli/Projects/blob/master/spiderplot.R)

The viz goes like...

![image](https://user-images.githubusercontent.com/37649445/126365363-067ce70d-d2f3-49b4-96ea-6fc4780af7d3.png)

_______________________

## Data Modelling

## App Building

- [Building a shiny app to plot 2 Indian Super League graphs by feeding inputs](https://github.com/abhijitbharalianalyst/basic/blob/master/usagerate.R).

- [Building Spider and Radar plots using the **FMSB package**](https://github.com/abhijitbharalianalyst/basic/blob/master/spiderplot.R).

- [Creating a binned density map of assists in the history of the Indian Super League, sorted by team](https://github.com/abhijitbharalianalyst/basic/blob/master/isl_assists.r).
