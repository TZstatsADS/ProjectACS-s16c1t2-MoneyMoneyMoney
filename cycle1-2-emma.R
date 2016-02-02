setwd("~/GitHub/project1")
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)

colsToKeep <- c("ST","PINCP","OCCP","COW")

#load data from set A and B
populDataA <- fread("ss13pusa.csv",select=colsToKeep)
populDataB <- fread("ss13pusb.csv",select=colsToKeep)

#concat data to one
populData <- rbind(populDataA, populDataB)

populData <- tbl_df(populData) 
ds <-  populData %>%  
  na.omit() %>%
  #filter(populData,PINCP!='bbbbbb') %>% #exclude no income person or N/A
  group_by(COW) #group by class of work 
ds<-filter(ds,PINCP!='bbbbbb') #exclude no income N/A

mean_cow<-summarise(ds, mean=mean(PINCP))
mean_cow<-arrange(mean_cow, desc(mean))
mean_cow
#boxplot(mean_cow$mean~mean_cow$COW,outline=TRUE)


ggplot(data=mean_cow, aes( x=factor(COW), y=mean,fill=factor(COW))) +
  geom_bar(colour="black",stat="identity")+
  xlab("class of work") + ylab("mean of total person's income ") +
  ggtitle("Average Income of Different Classes of Work")+
  scale_fill_hue(c=40, l=75)

ds5<-filter(populData,COW==5)%>%
    na.omit()%>%
    filter(PINCP!='bbbbbb')%>%
    group_by(OCCP)%>%
    summarise(mean=mean(PINCP))%>%
    arrange(desc(mean))
  
ds5head<-head(ds5)
ds5tail<-tail(ds5)


ds7<-filter(populData,COW==7)%>%
  na.omit()%>%
  filter(PINCP!='bbbbbb')%>%
  group_by(OCCP)%>%
  summarise(mean=mean(PINCP))%>%
  arrange(desc(mean))

ds7head<-head(ds7)
ds7tail<-tail(ds7)
