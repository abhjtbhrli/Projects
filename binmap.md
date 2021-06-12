ggplot()+
  annotate_pitch(dimensions = pitch_custom)+
  theme_pitch()+
  geom_point(data = qat.pass%>%
               filter(team=="Qatar",event=="Passes accurate"),
             aes(x=x2,y=y2),shape=21,size=2.5, colour="black")+
  stat_bin_2d(data = qat.pass%>%
               filter(team=="Qatar",event=="Passes accurate"),
             aes(x=x2,y=y2),
             binwidth=c(17.5,13.6),position = "identity",alpha=0.8,
             colour="black",drop = T,linetype="solid", size=0.6 ,show.legend = F)+
  scale_fill_gradient(low = "#00AFBB", high = "#FF1493")+
  labs(title = "QATAR's halfspace \n domination vs. INDIA",
       subtitle = "Location of all passes received",
       caption = "@progball")+
  theme(text = element_text(family = "Courier New"),
        plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill = "ivory"),
        plot.title = element_text(hjust = 0.5,vjust = 1,face = "bold",size=20),
        plot.subtitle = element_text(hjust = 0.5,vjust = 1,size=14),
        plot.caption = element_text(face = "bold",hjust = 0.5,vjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text  = element_blank())+
  ggplot2::annotate("text",x=105,y=-1,label="India 0-1 Qatar\n 03.Jun.21",
                    family="Courier",size=3,hjust=1,vjust=1)+
  geom_segment(aes(x=0,y=-3,xend=30,yend=-3),size=1,lineend = "butt",
               linejoin = "mitre",
               arrow = arrow(type = "closed",length = unit(0.07,"inches")),
               colour="black")
