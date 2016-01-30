library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

setwd("~/Desktop/studying/w4249 applied data models")

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c("PINCP", "CIT")
  popDataA <- fread("2013-american-community-survey/pums/ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("2013-american-community-survey/pums/ss13pusb.csv", select=colsToKeep )
  populData <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(populData, file="populData.RData")
}else{
  load("populData.RData")
}

attach(populData)
CIT <- as.factor(CIT)
levels(CIT) <- c("Born in the U.S.","Born in Puerto Rico, Guam, the U.S. Virgin Islands.or the Northern Marianas","Born abroad of American parent(s)","U.S. citizen by naturalization", "Not a citizen of the U.S.")
da <- na.omit(populData)
da <- tbl_df(populData)
da <- group_by(CIT)
ggplot(filter(da) , aes(x=CIT, y=PINCP)) + 
  geom_boxplot(aes(fill=CIT), size=0.3)
