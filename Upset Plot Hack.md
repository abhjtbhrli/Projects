# Upset Plot Hack 
### *A Dummy Project*

This is a longer workaround of building an upset plot in R. There are packages like `Complex UpSet`, `UpSetR`, and `ggupset` that can make life easier while create upset plots, but I wanted to build something from scratch, even if it's hacky. 

Here we go!

To start with, I create a dummy dataframe.

```
data <- data.frame(match = as.integer(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
                   vs = c("KER","MUM","JAM","NEU","CHE","BEN","GOA","HYD","EAB","MHB",
                          "KER","MUM","JAM","NEU","CHE","BEN","GOA","HYD","EAB","MHB"),
                   changes = c(0,0,2,1,3,4,1,1,2,0,4,1,1,2,0,4,1,1,2,0))
                 
head(data)

  match  vs changes
1     1 KER       0
2     2 MUM       0
3     3 JAM       2
4     4 NEU       1
5     5 CHE       3
6     6 BEN       4
```
Next, we load some libraries for data analysis and visualisation (install them before loading if not done already). 

I have done some importing of fonts using the `extrafont` package since I am very keen to use the *Comfortaa* font.

```
library(tidyverse)
library(plotly)
library(ggthemes)
library(extrafont)
library(extrafontdb)

extrafont::loadfonts(device = "win")
font_import(paths = "C:/Users/AB/Documents")
```
Now let us get down to doing some visualisation with `ggplot2`.

```
ggplot()+
  geom_point(data = data,
             aes(x = match,y = changes),
             size=2.5)+
  scale_x_continuous(breaks = seq(1,max(data$match),1),position = "bottom")+
  #scale_y_discrete(limits = rev(levels(factor(data$changes))))+
  geom_segment(data = data,
               aes(x = match,y = 0,xend = match,yend = changes,colour = -changes),
               size = 1,
               alpha = 0.7,
               show.legend = F)+
  geom_point(data = data,
             aes(x = match,y = 0),
             size = 2.5)+
  labs(title = "Starting XI changes per match",
       subtitle = "Odisha FC \nISL 21-22")+
  xlab("Match no.")+
  ylab("No. of changes")+
  theme(
    text = element_text(family = "Comfortaa"),
    plot.background = element_rect(fill="#F6FCF8"),
    panel.background = element_rect(fill = "#F6FCF8"),
    panel.grid = element_blank(),
    title = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0,face = "bold"),
    plot.subtitle = element_text(hjust = 0,size = 10)
  )
```
We create a plot like this:

![Upset](https://user-images.githubusercontent.com/37649445/123481688-b7274100-d621-11eb-99c8-ad3e179508fe.png)

We can also make the plot interactive by wrapping it in a `ggplotly` function

```
upset <- ggplot()+
  geom_point(data = data,
             aes(x=match,y=changes),
             size=2.5)+
  scale_x_continuous(breaks = seq(1,max(data$match),1),position = "bottom")+
  #scale_y_discrete(limits=rev(levels(factor(data$changes))))+
  geom_segment(data = data,
               aes(x=match,y=0,xend=match,yend=changes,colour=-changes),
               size=1,
               alpha=0.7,
               show.legend = F)+
  geom_point(data = data,
             aes(x=match,y=0),
             size=2.5)+
  labs(title = "Starting XI changes per match",
       subtitle = "Odisha FC \nISL 21-22")+
  xlab("Match no.")+
  ylab("No. of changes")+
  theme(
    text = element_text(family = "Comfortaa"),
    plot.background = element_rect(fill="#F6FCF8"),
    panel.background = element_rect(fill = "#F6FCF8"),
    panel.grid = element_blank(),
    title = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0,face = "bold"),
    plot.subtitle = element_text(hjust = 0,size = 10)
  )

ggplotly(upset)
```

It's not exactly the actual upset plot but comes quite close as a workaround. We can visualise certain kinds of data using this simple technique with base `ggplot2` geoms.

