# Load datasets from internal storage

data1<-read.csv("I-League Master.csv",check.names = F)
data2<-read.csv("Indian Super League Master.csv",check.names = F)

# Adjust loaded datasets
data1<-data1[,-1]
colnames(data1)[1]<-"id"
data1<-data1%>%mutate(xy1=paste(pos_x,pos_y))
#nrow(data2%>%filter(xy1%in%c("105 68","105 0","0 0","0 68")))

data2<-data2[,-1]
colnames(data2)[1]<-"id"
data2<-data2%>%mutate(xy1=paste(pos_x,pos_y))

# Combine datasets

data<-rbind(data2,data1)
colnames(data)[2]<-"id"
glimpse(data)
data<-data[,-1]

# Load libraries

library(tidyverse)
library(ggsoccer)
library(tidyr)
library(magrittr)
library(extrafont)
library(readr)

# Write functions

'%notin%'<-Negate('%in%')

# clean input dataframe

cleanDF<-function(dataframe){
  dataframe<-na.omit(dataframe)
  dataframe<-dataframe%>%
    select(id,start,end,code,label.0.text,label.1.text,label.2.text,pos_x,pos_y,season,
           matchid,competition)
  
  colnames(dataframe)=c("id","start","end","name","team","event","half","pos_x","pos_y",
                        "season","matchid","competition")
  return(dataframe)
}

data_clean<-cleanDF(data)
data_clean<-data_clean%>%mutate(xy1=paste(pos_x,pos_y))
#unique((data_clean%>%filter(pos_x==105,round(pos_y,0)%notin%c(0,68)))$pos_y)

# paste0(toString(1),toString(2))=='12'
# data_clean1<-data_clean1%>%
#   mutate(xy1=paste0(toString(pos_x),toString(pos_y)))

passRecDatFr<-function(dataframe){
  dataframe<-na.omit(dataframe)
  dataframe<-dataframe%>%
    select(id,start,end,code,label.0.text,label.1.text,label.2.text,pos_x,pos_y,season,
           matchid,competition,xy1)
  
  colnames(dataframe)=c("id","start","end","name","team","event","half","pos_x","pos_y",
                        "season","matchid","competition","xy1")
  
  dataframe$rec<-dataframe$id
  dataframe$x2<-dataframe$id
  dataframe$y2<-dataframe$id
  dataframe$start2<-dataframe$id
  
  colnames(dataframe)=c("id","start","end","name","team","event","half","pos_x","pos_y",
                        "season","matchid","competition","xy1","rec","x2","y2","start2")
  
  dataframe.passRec<-dataframe%>%
    filter(event%in%c("Passes accurate","Positional attacks",
                      "Passes into the penalty box","Passes (inaccurate)",
                      "Challenges (lost)","Lost balls","Assists","Shots"),
           #pos_x%notin%c(0,105),pos_y%notin%c(0,68),
           xy1%notin%c("105 68","105 0","0 0","0 68"))
  
  qat.pass<-dataframe.passRec
  

  
  for (i in 1:nrow(qat.pass)) {
    if ((nrow(qat.pass)-i)>=5){
      if (((qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Positional attacks") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes accurate") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes (inaccurate)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes into the penalty box") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Challenges (lost)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Lost balls") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Assists") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Shots")) &
          qat.pass$name[i]!=qat.pass$name[i+1] &
          qat.pass$team[i]==qat.pass$team[i+1] &
          qat.pass$half[i]==qat.pass$half[i+1] &
          qat.pass$id[i+1]-qat.pass$id[i]<10 &
          qat.pass$matchid[i]==qat.pass$matchid[i+1] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+1]
        qat.pass$x2[i]=qat.pass$pos_x[i+1]
        qat.pass$y2[i]=qat.pass$pos_y[i+1]
        qat.pass$start2[i]=qat.pass$start[i+1]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+2] &
                 qat.pass$team[i]==qat.pass$team[i+2] &
                 qat.pass$half[i]==qat.pass$half[i+2] &
                 qat.pass$id[i+2]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+2] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+2]
        qat.pass$x2[i]=qat.pass$pos_x[i+2]
        qat.pass$y2[i]=qat.pass$pos_y[i+2]
        qat.pass$start2[i]=qat.pass$start[i+2]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+3] &
                 qat.pass$team[i]==qat.pass$team[i+3] &
                 qat.pass$half[i]==qat.pass$half[i+3] &
                 qat.pass$id[i+3]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+3] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+3]
        qat.pass$x2[i]=qat.pass$pos_x[i+3]
        qat.pass$y2[i]=qat.pass$pos_y[i+3]
        qat.pass$start2[i]=qat.pass$start[i+3]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+4] &
                 qat.pass$team[i]==qat.pass$team[i+4] &
                 qat.pass$half[i]==qat.pass$half[i+4] &
                 qat.pass$id[i+4]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+4] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+4]
        qat.pass$x2[i]=qat.pass$pos_x[i+4]
        qat.pass$y2[i]=qat.pass$pos_y[i+4]
        qat.pass$start2[i]=qat.pass$start[i+4]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+5]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+5] &
                 qat.pass$team[i]==qat.pass$team[i+5] &
                 qat.pass$half[i]==qat.pass$half[i+5] &
                 qat.pass$id[i+5]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+5] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+5]
        qat.pass$x2[i]=qat.pass$pos_x[i+5]
        qat.pass$y2[i]=qat.pass$pos_y[i+5]
        qat.pass$start2[i]=qat.pass$start[i+5]
      } 
    } else if ((nrow(qat.pass)-i)<5 & (nrow(qat.pass)-i)>=4){
      if (((qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Positional attacks") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes accurate") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes (inaccurate)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes into the penalty box") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Challenges (lost)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Lost balls") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Assists") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Shots")) &
          qat.pass$name[i]!=qat.pass$name[i+1] &
          qat.pass$team[i]==qat.pass$team[i+1] &
          qat.pass$half[i]==qat.pass$half[i+1] &
          qat.pass$id[i+1]-qat.pass$id[i]<10 &
          qat.pass$matchid[i]==qat.pass$matchid[i+1] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+1]
        qat.pass$x2[i]=qat.pass$pos_x[i+1]
        qat.pass$y2[i]=qat.pass$pos_y[i+1]
        qat.pass$start2[i]=qat.pass$start[i+1]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+2] &
                 qat.pass$team[i]==qat.pass$team[i+2] &
                 qat.pass$half[i]==qat.pass$half[i+2] &
                 qat.pass$id[i+2]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+2] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+2]
        qat.pass$x2[i]=qat.pass$pos_x[i+2]
        qat.pass$y2[i]=qat.pass$pos_y[i+2]
        qat.pass$start2[i]=qat.pass$start[i+2]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+3] &
                 qat.pass$team[i]==qat.pass$team[i+3] &
                 qat.pass$half[i]==qat.pass$half[i+3] &
                 qat.pass$id[i+3]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+3] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+3]
        qat.pass$x2[i]=qat.pass$pos_x[i+3]
        qat.pass$y2[i]=qat.pass$pos_y[i+3]
        qat.pass$start2[i]=qat.pass$start[i+3]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+4]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+4] &
                 qat.pass$team[i]==qat.pass$team[i+4] &
                 qat.pass$half[i]==qat.pass$half[i+4] &
                 qat.pass$id[i+4]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+4] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+4]
        qat.pass$x2[i]=qat.pass$pos_x[i+4]
        qat.pass$y2[i]=qat.pass$pos_y[i+4]
        qat.pass$start2[i]=qat.pass$start[i+4]
      } 
    } else if ((nrow(qat.pass)-i)<4 & (nrow(qat.pass)-i)>=3){
      if (((qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Positional attacks") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes accurate") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes (inaccurate)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes into the penalty box") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Challenges (lost)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Lost balls") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Assists") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Shots")) &
          qat.pass$name[i]!=qat.pass$name[i+1] &
          qat.pass$team[i]==qat.pass$team[i+1] &
          qat.pass$half[i]==qat.pass$half[i+1] &
          qat.pass$id[i+1]-qat.pass$id[i]<10 &
          qat.pass$matchid[i]==qat.pass$matchid[i+1] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+1]
        qat.pass$x2[i]=qat.pass$pos_x[i+1]
        qat.pass$y2[i]=qat.pass$pos_y[i+1]
        qat.pass$start2[i]=qat.pass$start[i+1]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+2] &
                 qat.pass$team[i]==qat.pass$team[i+2] &
                 qat.pass$half[i]==qat.pass$half[i+2] &
                 qat.pass$id[i+2]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+2] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+2]
        qat.pass$x2[i]=qat.pass$pos_x[i+2]
        qat.pass$y2[i]=qat.pass$pos_y[i+2]
        qat.pass$start2[i]=qat.pass$start[i+2]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+3]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+3] &
                 qat.pass$team[i]==qat.pass$team[i+3] &
                 qat.pass$half[i]==qat.pass$half[i+3] &
                 qat.pass$id[i+3]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+3] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+3]
        qat.pass$x2[i]=qat.pass$pos_x[i+3]
        qat.pass$y2[i]=qat.pass$pos_y[i+3]
        qat.pass$start2[i]=qat.pass$start[i+3]
      } 
    } else if ((nrow(qat.pass)-i)<3 & (nrow(qat.pass)-i)>=2){
      if (((qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Positional attacks") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes accurate") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes (inaccurate)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes into the penalty box") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Challenges (lost)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Lost balls") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Assists") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Shots")) &
          qat.pass$name[i]!=qat.pass$name[i+1] &
          qat.pass$team[i]==qat.pass$team[i+1] &
          qat.pass$half[i]==qat.pass$half[i+1] &
          qat.pass$id[i+1]-qat.pass$id[i]<10 &
          qat.pass$matchid[i]==qat.pass$matchid[i+1] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+1]
        qat.pass$x2[i]=qat.pass$pos_x[i+1]
        qat.pass$y2[i]=qat.pass$pos_y[i+1]
        qat.pass$start2[i]=qat.pass$start[i+1]
      } else if (((qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Positional attacks") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes accurate") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes (inaccurate)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Passes into the penalty box") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Challenges (lost)") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Lost balls") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Assists") |
                  (qat.pass$event[i]=="Passes accurate" & 
                   qat.pass$event[i+2]=="Shots")) &
                 qat.pass$name[i]!=qat.pass$name[i+2] &
                 qat.pass$team[i]==qat.pass$team[i+2] &
                 qat.pass$half[i]==qat.pass$half[i+2] &
                 qat.pass$id[i+2]-qat.pass$id[i]<10 &
                 qat.pass$matchid[i]==qat.pass$matchid[i+2] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+2]
        qat.pass$x2[i]=qat.pass$pos_x[i+2]
        qat.pass$y2[i]=qat.pass$pos_y[i+2]
        qat.pass$start2[i]=qat.pass$start[i+2]
      }  
    } else if ((nrow(qat.pass)-i)<2 & (nrow(qat.pass)-i)>=1){
      if (((qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Positional attacks") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes accurate") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes (inaccurate)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Passes into the penalty box") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Challenges (lost)") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Lost balls") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Assists") |
           (qat.pass$event[i]=="Passes accurate" & 
            qat.pass$event[i+1]=="Shots")) &
          qat.pass$name[i]!=qat.pass$name[i+1] &
          qat.pass$team[i]==qat.pass$team[i+1] &
          qat.pass$half[i]==qat.pass$half[i+1] &
          qat.pass$id[i+1]-qat.pass$id[i]<10 &
          qat.pass$matchid[i]==qat.pass$matchid[i+1] 
      ) {
        qat.pass$rec[i]=qat.pass$name[i+1]
        qat.pass$x2[i]=qat.pass$pos_x[i+1]
        qat.pass$y2[i]=qat.pass$pos_y[i+1]
        qat.pass$start2[i]=qat.pass$start[i+1]
      }
    }
  }
  
  passRecDF<-qat.pass%>%
    filter(event=="Passes accurate")
  return(passRecDF)
}


############
##        ##
## passdf ##
##        ##
############
unique(data2$season)
succPass1<-passRecDatFr(data1%>%filter(season=="2019-20"))
succPass2<-passRecDatFr(data1%>%filter(season=="2020-21"))
succPass3<-passRecDatFr(data2%>%filter(season=="2014"))
succPass4<-passRecDatFr(data2%>%filter(season=="2015"))
succPass5<-passRecDatFr(data2%>%filter(season=="2016"))
succPass6<-passRecDatFr(data2%>%filter(season=="2017-18"))
succPass7<-passRecDatFr(data2%>%filter(season=="2018-19"))
succPass8<-passRecDatFr(data2%>%filter(season=="2019-20"))
succPass9<-passRecDatFr(data2%>%filter(season=="2020-21"))

succPass<-rbind(succPass1,succPass2,succPass3,succPass4,succPass5,succPass6,succPass7,
                succPass8,succPass9)

#-------------
#---assist----
#-------------
data.assist<-data_clean%>%filter(event%in%c("Assists","Goals"))

data.assist$rec<-data.assist$id
data.assist$x2<-data.assist$id
data.assist$y2<-data.assist$id
data.assist$start2<-data.assist$id

for (i in 1:nrow(data.assist)) {
  if (data.assist$event[i]=="Assists") {
    data.assist$rec[i]=data.assist$name[i+1]
    data.assist$x2[i]=data.assist$pos_x[i+1]
    data.assist$y2[i]=data.assist$pos_y[i+1]
  }
}

data.assist<-data.assist%>%filter(event=="Assists")

# manually correct an error
which(data.assist$id==2168, data.assist$matchid==2033)
data.assist$x2[which(data.assist$id==2168, data.assist$matchid==2033)]=105-data.assist$x2[which(data.assist$id==2168, data.assist$matchid==2033)]
data.assist$y2[which(data.assist$id==2168, data.assist$matchid==2033)]=68-data.assist$y2[which(data.assist$id==2168, data.assist$matchid==2033)]


data.assistnonCK<-data.assist%>%
  filter(xy1%notin%c("105 68","105 0","0 0","0 68","105 0.1","105 67.9"))
  
 #-------------
#---Inacc. pass----
#-------------

data.unpass<-data_clean%>%
  filter(event%in%c("Passes (inaccurate)","Lost balls","Interceptions","Picking-ups"),
         xy1%notin%c("105 68","105 0","0 0","0 68"))

data.unpass$rec<-data.unpass$id
data.unpass$x2<-data.unpass$id
data.unpass$y2<-data.unpass$id
data.unpass$start2<-data.unpass$id

for (i in 1:nrow(data.unpass)) {
  if (i>2){
    if (data.unpass$event[i]=="Passes (inaccurate)" &
        data.unpass$event[i+1]=="Lost balls" &
        data.unpass$team[i+1]==data.unpass$team[i] &
        data.unpass$name[i+1]==data.unpass$name[i] &
        data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Lost balls" &
               data.unpass$team[i+2]==data.unpass$team[i] &
               data.unpass$name[i+2]==data.unpass$name[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+2]
      data.unpass$y2[i]=data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i-1]=="Lost balls" &
               data.unpass$team[i-1]==data.unpass$team[i] &
               data.unpass$name[i-1]==data.unpass$name[i] &
               data.unpass$matchid[i-1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i-1]
      data.unpass$y2[i]=data.unpass$pos_y[i-1]
      data.unpass$rec[i]=data.unpass$event[i-1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i-2]=="Lost balls" &
               data.unpass$team[i-2]==data.unpass$team[i] &
               data.unpass$name[i-2]==data.unpass$name[i] &
               data.unpass$matchid[i-2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i-2]
      data.unpass$y2[i]=data.unpass$pos_y[i-2]
      data.unpass$rec[i]=data.unpass$event[i-2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Interceptions" &
               data.unpass$team[i+1]!=data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+1]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Interceptions" &
               data.unpass$team[i+2]!=data.unpass$team[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+2]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+3]=="Interceptions" &
               data.unpass$team[i+3]!=data.unpass$team[i] &
               data.unpass$matchid[i+3]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+3]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+3]
      data.unpass$rec[i]=data.unpass$event[i+3]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+4]=="Interceptions" &
               data.unpass$team[i+4]!=data.unpass$team[i] &
               data.unpass$matchid[i+4]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+4]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+4]
      data.unpass$rec[i]=data.unpass$event[i+4]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Picking-ups" &
               data.unpass$team[i+1]!=data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+1]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Picking-ups" &
               data.unpass$team[i+2]!=data.unpass$team[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+2]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+3]=="Picking-ups" &
               data.unpass$team[i+3]!=data.unpass$team[i] &
               data.unpass$matchid[i+3]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+3]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+3]
      data.unpass$rec[i]=data.unpass$event[i+3]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+4]=="Picking-ups" &
               data.unpass$team[i+4]!=data.unpass$team[i] &
               data.unpass$matchid[i+4]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+4]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+4]
      data.unpass$rec[i]=data.unpass$event[i+4]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Picking-ups" &
               data.unpass$team[i+1]==data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Lost balls" &
               data.unpass$team[i+1]==data.unpass$team[i] &
               data.unpass$name[i+1]!=data.unpass$name[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    }
  } else if (i<=2){
    if (data.unpass$event[i]=="Passes (inaccurate)" &
        data.unpass$event[i+1]=="Lost balls" &
        data.unpass$team[i+1]==data.unpass$team[i] &
        data.unpass$name[i+1]==data.unpass$name[i] &
        data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Lost balls" &
               data.unpass$team[i+2]==data.unpass$team[i] &
               data.unpass$name[i+2]==data.unpass$name[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+2]
      data.unpass$y2[i]=data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } 
    # else if (data.unpass$event[i]=="Passes (inaccurate)" &
    #            data.unpass$event[i-1]=="Lost balls" &
    #            data.unpass$team[i-1]==data.unpass$team[i] &
    #            data.unpass$name[i-1]==data.unpass$name[i] &
    #            data.unpass$matchid[i-1]==data.unpass$matchid[i]) {
    #   data.unpass$x2[i]=data.unpass$pos_x[i-1]
    #   data.unpass$y2[i]=data.unpass$pos_y[i-1]
    #   data.unpass$rec[i]=data.unpass$event[i-1]
    # } else if (data.unpass$event[i]=="Passes (inaccurate)" &
    #            data.unpass$event[i-2]=="Lost balls" &
    #            data.unpass$team[i-2]==data.unpass$team[i] &
    #            data.unpass$name[i-2]==data.unpass$name[i] &
    #            data.unpass$matchid[i-2]==data.unpass$matchid[i]) {
    #   data.unpass$x2[i]=data.unpass$pos_x[i-2]
    #   data.unpass$y2[i]=data.unpass$pos_y[i-2]
    #   data.unpass$rec[i]=data.unpass$event[i-2]
    # } 
    else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Interceptions" &
               data.unpass$team[i+1]!=data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+1]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Interceptions" &
               data.unpass$team[i+2]!=data.unpass$team[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+2]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+3]=="Interceptions" &
               data.unpass$team[i+3]!=data.unpass$team[i] &
               data.unpass$matchid[i+3]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+3]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+3]
      data.unpass$rec[i]=data.unpass$event[i+3]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+4]=="Interceptions" &
               data.unpass$team[i+4]!=data.unpass$team[i] &
               data.unpass$matchid[i+4]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+4]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+4]
      data.unpass$rec[i]=data.unpass$event[i+4]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Picking-ups" &
               data.unpass$team[i+1]!=data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+1]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+2]=="Picking-ups" &
               data.unpass$team[i+2]!=data.unpass$team[i] &
               data.unpass$matchid[i+2]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+2]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+2]
      data.unpass$rec[i]=data.unpass$event[i+2]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+3]=="Picking-ups" &
               data.unpass$team[i+3]!=data.unpass$team[i] &
               data.unpass$matchid[i+3]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+3]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+3]
      data.unpass$rec[i]=data.unpass$event[i+3]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+4]=="Picking-ups" &
               data.unpass$team[i+4]!=data.unpass$team[i] &
               data.unpass$matchid[i+4]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=105-data.unpass$pos_x[i+4]
      data.unpass$y2[i]=68-data.unpass$pos_y[i+4]
      data.unpass$rec[i]=data.unpass$event[i+4]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Picking-ups" &
               data.unpass$team[i+1]==data.unpass$team[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    } else if (data.unpass$event[i]=="Passes (inaccurate)" &
               data.unpass$event[i+1]=="Lost balls" &
               data.unpass$team[i+1]==data.unpass$team[i] &
               data.unpass$name[i+1]!=data.unpass$name[i] &
               data.unpass$matchid[i+1]==data.unpass$matchid[i]) {
      data.unpass$x2[i]=data.unpass$pos_x[i+1]
      data.unpass$y2[i]=data.unpass$pos_y[i+1]
      data.unpass$rec[i]=data.unpass$event[i+1]
    }
  }
}

data.unpass<-data.unpass%>%filter(event=="Passes (inaccurate)") 

data.pass<-rbind(succPass,data.assistnonCK,data.unpass)
data.pass<-data.pass%>%arrange(matchid,id)

data.passAll<-data.pass
str(data.passAll)
nrow(data.passAll%>%filter(season!="2020-21"))
data.passAll$eventid<-data.passAll$event
unique(data.passAll$eventid)

data.passAll$eventid[which(data.passAll$eventid=="Assists")]=as.numeric(1)
data.passAll$eventid[which(data.passAll$eventid=="Passes accurate")]=as.numeric(1)
data.passAll$eventid[which(data.passAll$eventid=="Passes (inaccurate)")]=as.numeric(0)

# CHANGE CATEGORICAL DATA TO NUMERICAL DATA
data.passAll$eventid<-as.factor(data.passAll$eventid)
glimpse(data.passAll)

# Add distance & angle features

data.passAll<-data.passAll%>%mutate(
  passdist=round(sqrt((pos_x-x2)**2+(pos_y-y2)**2),1),
  passangle=round(atan((y2-pos_y)/(x2-pos_x)),1)
)


# Separate 2020-21 data from the main dataset (for prediction purpose)
passMod<-data.passAll%>%filter(season!="2020-21")%>%na.omit()
passPred<-data.passAll%>%filter(season=="2020-21")%>%na.omit()


# TIDYMODELS & MODELLING


library(tidymodels)

# Data splitting
split_passMod<-initial_split(passMod,0.75,strata = eventid)
train_data<-training(split_passMod)
test_data<-testing(split_passMod)

train_data %>% 
  count(eventid) %>% 
  mutate(ratio = n/sum(n))

test_data %>% 
  count(eventid) %>% 
  mutate(ratio = n/sum(n))


# RECIPE

xp_recipe <-
  recipe(eventid ~ pos_x + pos_y + x2 + y2 + passdist + passangle, data = train_data)
#%>% 
#   update_role(location_x, location_y, new_role = "ID") 

# Model

lr_model<-logistic_reg(mode = "classification")%>%
  set_engine("glm")

xp_workflow<-workflow()%>%
  add_model(lr_model)%>%
  add_recipe(xp_recipe)

xp_workflow

xp_fit<-xp_workflow%>%fit(data=train_data)


# Yardstick
xp_fit%>%pull_workflow_fit()%>%tidy()

xp_pred_lr<-predict(xp_fit,test_data)%>%
  bind_cols(predict(xp_fit,test_data,type = "prob"))%>%
  bind_cols(test_data%>%select(eventid))

xp_pred_lr%>%roc_auc(truth=eventid,.pred_0)

xp_pred_lr%>%metrics(truth=eventid,.pred_class)

xp_pred_lr%>%roc_curve(truth=eventid,.pred_0)%>%autoplot()

xp_pred_lr

# playing around with model predictions

?crossing
nrow(passPred)-nrow(passPred%>%na.omit())

artificial_pass<-data.frame(pos_x = c(50,55), pos_y = c(20,48),
                          x2 = c(45,100),y2 = c(30,34))%>%
  mutate(
    passdist=round(sqrt((pos_x-x2)**2+(pos_y-y2)**2),1),
    passangle=round(atan((y2-pos_y)/(x2-pos_x)),1)
  )



artificial_pass<-data.frame(pos_x = 50, pos_y = 20,
                            x2 = 45,y2 = 30)%>%
  mutate(
    passdist=round(sqrt((pos_x-x2)**2+(pos_y-y2)**2),1),
    passangle=round(atan((y2-pos_y)/(x2-pos_x)),1)
  )


(predict(xp_fit,artificial_pass)%>%
  bind_cols(predict(xp_fit,artificial_pass,type = "prob")))$.pred_1

artificial_pass$xP=(predict(xp_fit,artificial_pass)%>%
                      bind_cols(predict(xp_fit,artificial_pass,type = "prob")))$.pred_1

# Make predictions on 2020-21 data (passPred dataset)

xp_pred_2021<-predict(xp_fit,passPred)%>%
  bind_cols(predict(xp_fit,passPred,type = "prob"))%>%
  bind_cols(passPred%>%select(eventid))

xp_pred_2021%>%metrics(truth=eventid,.pred_class)

xp_pred_2021

passPred$xPass<-xp_pred_2021$.pred_1

#change/correct a few names manually

passPred$name[which(passPred$name=="5. Noguera")]="5. Alberto Noguera"
passPred$name[which(passPred$name=="1. Gurpreet Sing Sandhu")]="1. Gurpreet Singh Sandhu"
passPred$name[which(passPred$name=="1. Subhashish Roj Chowdhury")]="1. Subhasish Roy Chowdhury"
passPred$name[which(passPred$name=="50. Subhashish Roj Chowdhury")]="50. Subhasish Roy Chowdhury"
passPred$name[which(passPred$name=="27. Subhashish Roj Chowdhury")]="27. Subhasish Roy Chowdhury"
passPred$name[which(passPred$name=="29. Subhashish Roj Chowdhury")]="29. Subhasish Roy Chowdhury"
passPred$name[which(passPred$name=="24. Subhashish Roj Chowdhury")]="24. Subhasish Roy Chowdhury"
passPred$name[which(passPred$name=="19. Benjamin Lambot")]="19. Benjamin Edouardo Lambot"
passPred$name[which(passPred$name=="25. Mourtada Serigne Fall")]="25. Mourtada Fall"
passPred$name[which(passPred$name=="17. Mandar Rao Rao Dessai")]="17. Mandar Rao Dessai"
passPred$name[which(passPred$name=="15. Jeakson Singh Singh")]="15. Jeakson Singh"
passPred$name[which(passPred$name=="32. T.P Rehnesh")]="32. T.P Rehenesh"
passPred$name[which(passPred$name=="18. Seminlen Doungel")]="18. Seiminlen Doungel"


# Create table to calculate xP Rating of players

passValue2021<-passPred%>%
  mutate(pass=as.numeric(passPred$eventid)-1)%>%
  group_by(name,team,competition)%>%
  summarise(xP=sum(xPass),nP=sum(pass))%>%
  mutate(passValue=round(nP/xP,2))%>%
  filter(nP>=100)%>%
  arrange(desc(passValue))%>%
  ungroup()

passValue2021<-passValue2021%>%
  separate(name,c("num","player"),". ",extra = "merge")

head(passValue2021%>%filter(competition=="Indian Super League"),15)



# Visualise the xP Ratings using reactable tables

install.packages("reactable")
library(reactable)
install.packages("reactablefmtr")
library(reactablefmtr)


# ISL 2020-21

reactable((passValue2021%>%filter(competition=="Indian Super League"))[,c(2,3,7)],
          defaultSortOrder = "desc",
          defaultSorted = "passValue",
          columns = list(
            player=colDef(name = "Player"),
            team=colDef(name = "Team"),
            passValue=colDef(name = "xP Rating",
                             filterable = F,maxWidth = 100)
          ),
          filterable = T,
          minRows = 10,
          defaultPageSize = 10,
          outlined = T,
          borderless = T,
          highlight = T,
          theme = reactableTheme(
            color = "hsl(233, 9%, 87%)",
            borderColor = "hsl(233, 9%, 22%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)",
            cellPadding = "8px 12px",
            style = list(fontFamily = "Avenir Next"),
            searchInputStyle = list(width = "100%")
          ))%>%
  add_title("Expected Passing Table (ISL 2020-21)",
            align = "center",
            font_color = "hsl(233, 9%, 87%)",
            font_family = "Avenir Next",
            font_size = 30,
            font_style = "normal",
            font_weight = "normal",
            text_decoration = NULL,
            background_color = "hsl(233, 12%, 24%)",
            margin = 0
  )


# I-League 2020-21

reactable((passValue2021%>%filter(competition=="I-League"))[,c(2,3,7)],
          defaultSortOrder = "desc",
          defaultSorted = "passValue",
          columns = list(
            player=colDef(name = "Player"),
            team=colDef(name = "Team"),
            passValue=colDef(name = "xP Rating",
                             filterable = F,maxWidth = 100)
          ),
          filterable = T,
          minRows = 10,
          defaultPageSize = 10,
          outlined = T,
          borderless = T,
          highlight = T,
          theme = reactableTheme(
            color = "hsl(233, 9%, 87%)",
            borderColor = "hsl(233, 9%, 22%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)",
            cellPadding = "8px 12px",
            style = list(fontFamily = "Avenir Next"),
            searchInputStyle = list(width = "100%")
          ))%>%
  add_title("Expected Passing Table (I-League 2020-21)",
            align = "center",
            font_color = "hsl(233, 9%, 87%)",
            font_family = "Avenir Next",
            font_size = 28,
            font_style = "normal",
            font_weight = "normal",
            text_decoration = NULL,
            background_color = "hsl(233, 12%, 24%)",
            margin = 0
  )

