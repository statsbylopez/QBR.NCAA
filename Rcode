library(MASS)
library(sm)
library(gplots)
library(XML)
library(stringr)


lengths<-c(27,27,29,31,31,31,31,31,33,33,33)

urls<-NULL

for (j in 1:11){
  for (i in 1:lengths[j]){
    year<-j+2003
    url.temp<-paste("http://espn.go.com/ncf/qbr/_/year/",year,"/type/player-game/page/",i,sep="")
    urls<-c(urls,url.temp)
  }
}

ncaa<-NULL
for (i in 1:length(urls)){
  tables <- readHTMLTable(urls[i])
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  temp<-tables[[which.max(n.rows)]]
  ncaa<-rbind(ncaa,temp)
}

###START HERE


library(stringr)
library(sm)
library(ggplot2)
require("extrafont")
require(scales)
library(MASS)
library(gplots)
library(XML)
library(stringr)
require(grid)


names(ncaa)[11]<-"QBR"
ncaa1<-ncaa[ncaa$RK!="RK",]
ncaa1$name<-str_split_fixed(as.character(ncaa1$PLAYER), ",",2)[,1]
ncaa1$QBR<-as.numeric(as.character(ncaa1$QBR))
ncaa1<-ncaa1[ncaa1$name!="PLAYER",]

ncaa1$Outcome<-substr(ncaa1$RESULT,1,1)=="W"
ncaa1$Decile<-with(ncaa1, cut(QBR, breaks=quantile(QBR, probs=seq(0,1, by=0.1)), 
                              include.lowest=TRUE))
temp<-"@"
ncaa1$Road<-grepl(temp,ncaa1$RESULT)
ncaa1$Location<-0
ncaa1[ncaa1$Road=="TRUE",]$Location<-"Away"
ncaa1[ncaa1$Road=="FALSE",]$Location<-"Home"
careerstarts<-aggregate(QBR~name,data=ncaa1,FUN="length")
careerstarts<-careerstarts[careerstarts$QBR>=20,]  #Minimum number of starts

#Put whatever players to want in here

dat.T2<-ncaa1[ncaa1$name%in%c("Matt Leinart","Vince Young"),]
temp<-dat.T2
ggplot(temp) + geom_line(aes(x = QBR,colour=name),lwd=2.2,adjust=1.1, stat="density")+
  theme(axis.title.x = element_text(face="bold", colour="black", size=20))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(size=12),legend.title=element_blank(),
        legend.text = element_text(size = 16, face = "bold"),legend.position="right")

