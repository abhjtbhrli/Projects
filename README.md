# Sports data visualisations, models, applications

[Abhijit Bharali](https://abhijitbharali.com/) | [Twitter: `@abhibharali`](https://twitter.com/abhibharali)

## Basic Visualisations

**Visualise football match results in a matrix using matchsheet data from Transfermarkt.com**

### **Method #1** 

[Copy-paste the data from _transfermarkt.com_ in local machine and generate the viz.](https://github.com/abhjtbhrli/Projects/blob/master/resultmatrix.R)

### **Method #2** 

Web scrape the data from _transfermarkt.com_ and do a full-stack approach to building the viz.

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

The final result looks something like this...

![result matrix](https://user-images.githubusercontent.com/37649445/126327305-c618059f-f881-4a20-b1d5-9d22034b087d.png)

This is an example of a full-stack sports data visualisation approach using `R`, wherein we extract the data from the web using scraping techniques, clean and wrangle the extracted data to a usable format and then visualise the data to generate insights. 

## Advanced Visualisations

## Data Modelling

## App Building

1. [Creating a result matrix of any league in the world, using simple, matchsheet data](https://github.com/abhijitbharalianalyst/basic/blob/master/resultmatrix.R).

2. [Building a shiny app to plot 2 Indian Super League graphs by feeding inputs](https://github.com/abhijitbharalianalyst/basic/blob/master/usagerate.R).

3. [Building Spider and Radar plots using the **FMSB package**](https://github.com/abhijitbharalianalyst/basic/blob/master/spiderplot.R).

4. [Creating a binned density map of assists in the history of the Indian Super League, sorted by team](https://github.com/abhijitbharalianalyst/basic/blob/master/isl_assists.r).
