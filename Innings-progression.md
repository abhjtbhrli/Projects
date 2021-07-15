# Visualising Cricket Data

## Innings Progression

_Under the hood, there is a lot that is going on in the field of cricket analytics. A lot._

_Is all of that stuff open sourced or showcased in the public domain? **I don't think so.**_

One possible reason for cricket analytics and research being relatively private compared to other sports like baseball, soccer, hockey, etc. is the fact that very few teams play elite level competitions and hence there is little incentive to open source intellectual property.

However, in recent times, we have seen websites and blogs of analytics platforms/companies like [ESPNCricinfo](espncricinfo.com), [Stats Perform](https://www.statsperform.com/team-performance/cricket/), [CricViz](cricviz.com), etc. showcase their work, albeit in a limited and non-compete manner.

One such illustration is a visualisation I stumbled upon while reading up stuff on the [Stats Perform website](https://www.statsperform.com/resource/modelling-cricket-innings-composition/). 

Here is that visualisation...

![image](https://user-images.githubusercontent.com/37649445/125837685-41494ee1-96ab-45c1-bd27-8bf64d4b2a96.png)

Having been a fan of the sport of cricket since forever, it has always been akin to a life objective for me to figure out how people do analytics in cricket and get done all the wacky, wonky advanced stuff that we see on TV, internet and on various other media. To that end, I used my limited ideas of programming and went about trying to recreate the exact same plot (as shown above) using `ggplot2` in `R`.

I used data from [cricsheet](cricsheet.org) plus the `dplyr`, `tidyverse`, `magrittr` and `ggplot2` libraries to spit out a replica of the Stats Perform visualisation, with a slightly different colour scheme, as a mark of respect towards the original viz authors. Going by the looks of it, I'm pretty sure the Stats Perform people created their viz using `Python`, so here's the `R` version that creates a similar visual. 

I go in a step-by-step manner for easy comprehension.

Step 1: Load libraries.

```
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
```
Just loading `tidyverse` is enough as it has the functionalities of `dplyr` and `ggplot2` wrapped in it. 

Step2: Download data from cricsheet.

Clicking on this link [https://cricsheet.org/downloads/ntb_male_csv2.zip](https://cricsheet.org/downloads/ntb_male_csv2.zip) downloads the .zip file of the IPL dataset. You can then extract the .zip file and save the .csv file in the working directory of RStudio. 

Step 3: Load data (named "IPL.csv" in my RStudio working directory) into `R`.

```
cric<-read.csv("IPL.csv")
```

Now, let us compare the innings progression of two players from an IPL match. One match of note is the Punjab Kings vs Rajasthan Royals one where there were two 90+ innings from batters from the two teams. We use KL Rahul's 91 (50) and Sanju Samson's 119 (63) as case studies and visualise how they built their innings from the first ball they faced.

Step 4: Create two separate dataframes of the two batters (`batter1` and `batter2`).

```
batter1<-cric%>%
  filter(season=="2021",
         start_date=="2021-04-12",
         batting_team=="Rajasthan Royals",
         striker%in%c("SV Samson"),
         is.na(wides))%>%
  group_by(striker,ball,innings)%>%
  summarise(runs=cumsum(runs_off_bat))%>%
  ungroup()%>%
  mutate(cum_runs=cumsum(runs),
         inns_balls_faced=seq(from=1,to=length(striker),by=1))%>%
  arrange(innings,ball)%>%
  as.data.frame()
  
  batter2<-cric%>%
  filter(season=="2021",
         start_date=="2021-04-12",
         batting_team=="Punjab Kings",
         striker%in%c("KL Rahul"),
         is.na(wides))%>%
  group_by(striker,ball,innings)%>%
  summarise(runs=cumsum(runs_off_bat))%>%
  ungroup()%>%
  mutate(cum_runs=cumsum(runs),
         inns_balls_faced=seq(from=1,to=length(striker),by=1))%>%
  arrange(innings,ball)%>%
  as.data.frame()  
```

Step 5: Combine the two dataframes and create the visualisation. We also hard code a few lines for the plot's subtitle.

```
rbind(batter1,batter2)

team1<-unique((cric%>%
  filter(start_date=="2021-04-12"))$batting_team)[1]
team2<-unique((cric%>%
                 filter(start_date=="2021-04-12"))$batting_team)[2]
details<-paste0("IPL ",unique(cric$season[which(cric$start_date=="2021-04-12")]),
                " | ",team1," vs ",team2," | 12.Apr.2021")

ggplot(data = rbind(batter1,batter2))+
  geom_line(aes(x=inns_balls_faced,y=cum_runs,colour=striker),
            size=1)+
  geom_hline(yintercept = 100,
             colour="white",
             linetype=3)+
  scale_color_manual(values = c("#A6AAB0","#FB4447"))+
  labs(title = "Runs vs Balls Faced",
       subtitle = details)+
  xlab("Innings Balls Faced")+
  ylab("Innings Runs")+
  theme(text = element_text(family = "Andale Mono"),
        plot.background = element_rect(fill = "#2A374A"),
        panel.background = element_rect(fill = "#2A374A"),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "white"),
        panel.border = element_rect(colour = "white",fill = NA),
        plot.title = element_text(colour = "white",size = 20),
        plot.subtitle = element_text(colour = "white",size = 10),
        axis.title = element_text(colour = "white",size = 15),
        legend.background = element_rect(fill = "#2A374A"),
        legend.key = element_rect(fill = "#2A374A"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "white"),
        legend.position = c(0.158,0.913),
        #legend.box.background = element_rect(fill = NA,colour = "white"),
        legend.direction = "vertical",
        legend.justification = c(1,0),
        legend.margin = margin(0,2,0,1),
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(20,40,20,20),
        aspect.ratio = 1)
  
ggsave("opta",dpi = 300, device = "png",width = 8.15,height = 8.27)
```
And, this is what the finished product looks like...

![opta](https://user-images.githubusercontent.com/37649445/125839993-b7daef88-e4ca-441b-96e8-e9021790198c.png)

Fin.
