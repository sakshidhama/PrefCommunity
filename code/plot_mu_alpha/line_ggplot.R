library(readxl)
library(poliscidata)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)

#Read the excel with community sizes
setwd("/Users/sakshidhama/Dropbox/work/community_preference")
mydir<-"/Users/sakshidhama/Dropbox/work/community_preference/all"
linefiles    <- list.files(path =mydir ,pattern = "*.xlsx",full.names = TRUE)
line_table <- linefiles %>% lapply(read_xlsx)
# xlsx files
cc<-c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.70,0.75,0.80)
par(mfrow=c(2,5)) # 3X4 graphs
par(oma=c(0,0,5,0))  # top has 5 lines of space 
par(mar=c(4,4,2,1)+.1)  # margin lines
par(pin=c(1.9,1.9))  # plot areas for graphs
linetype <- c(1:19)
colors1<-rainbow(19)
mulist<-unlist(line_table[[1]][,2][[1]])
dat2<-list()
yrange<-list()
xrange<-list()
for (l in 1:length(line_table)) {
  # get the range for the x and y axis

  ll<-list()
  l<-1
  for (t in 1:19) {
    yt<- line_table[[l]][1:19,3:8][t,]
    tr<-list()
    for ( j in 1:6) {
      tr<-append(tr,yt[j][[1]])
    }
    ll<-append(ll,unlist(tr))
  }
  temp<-as.integer(unlist(ll))
  #yrange[l] <- temp[1:length(temp)]
  xrange[l] <- data.frame(rep(1:6,19))
  dat2<-data.frame(x=rep(1:6,19),y=temp,linecol=rep(colors1,each=6),typeline=rep(1:19,each=6),mu=rep(mulist,each=6))  
  g1<-ggplot(dat2,mapping = aes(x=x,y=y,group = typeline,colour=linecol))+
    geom_line(aes(color=linecol))+
    geom_point(size=1)+
    scale_color_discrete(name = "Mu value", labels = as.character(mulist) ) +
  labs( title= paste0("Alpha value = ",cc[l]), y="Community Size", x = "Number of Communities")
}
 


  