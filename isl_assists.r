#Import the dataset to use#
isldatafull<-read.csv("combined_csv.csv")

#Install and load required packages#
install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggsoccer")
library(ggsoccer)
pitch_instat<-list(
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

#Modify the data to use#
islassists<-isldatafull%>%filter(label.1.text=="Assists")

#Plot the data#
ggplot(data = islassists,aes(x=pos_x,y=pos_y))+
  annotate_pitch(dimensions = pitch_instat,fill = "antiquewhite1")+
  theme_pitch()+
  geom_bin2d(binwidth=c(15,13.6),alpha=0.8,show.legend = F)+
  scale_fill_gradient(low = "gray66",high = "darkred")+
  geom_point(colour="black",alpha=0.2,size=0.3)+
  labs(title = "Goal creation zones in ISL",
       subtitle = "1046 assists",
       caption = "Data from 572 ISL matches")+
  theme(
    plot.background = element_rect(fill = "antiquewhite1"),
    panel.background = element_rect(fill = "antiquewhite1"),
    legend.background = element_rect(fill = "antiquewhite1"),
    legend.text = element_text(family = "mono",colour = "black",face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    title = element_text(family = "mono",colour = "black",face = "bold",vjust = 10),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(colour="red",hjust = 0.5),
    text = element_text(family = "mono",face = "bold"),
    strip.background = element_rect(fill = "antiquewhite1")
  )+
  facet_wrap(~label.0.text)
