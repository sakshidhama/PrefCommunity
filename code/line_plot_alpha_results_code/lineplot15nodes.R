library(readxl)
library(dplyr)
library(poliscidata)
library(ggplot2)
#Read the excel with community sizes
setwd("/community_preference")
mydir<-"/line_plot_alpha_results_code"
linefiles    <- list.files(path =mydir ,pattern = "*.xlsx",full.names = TRUE)
line_table <- linefiles %>% lapply(read_xlsx)
# xlsx files
cc<-c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.70,0.75,0.80)
par(mfrow=c(3,4)) # 3X4 graphs
par(oma=c(0,0,5,0))  # top has 5 lines of space 
par(mar=c(4,4,2,1)+.1)  # margin lines
par(pin=c(1.9,1.9,1.9,1.9))  # plot areas for graphs

for (l in 1:length(line_table)) {
# get the range for the x and y axis
xrange <- rep(1:6,19)
ll<-list()
for (t in 1:19) {
  yt<- line_table[[l]][1:19,3:8][t,]
  tr<-list()
  for ( j in 1:6) {
    tr<-append(tr,yt[j][[1]])
  }
ll<-append(ll,unlist(tr))
}
yrange <- as.integer(unlist(ll))
x1<-1
y1<-1
x2<-6
y2<-6
# set up the plot
colors <- rainbow(19)
linetype <- c(1:19)
plotchar <- seq(6,6+19,1)
plot(xrange[x1:x2], yrange[x1:x2],xlim=c(1,6), ylim=c(1,15), type="b", xlab="Community",ylab="Size of Community" ,pch=plotchar[1],col=colors[1])
# add lines
  for (i in 1:19) {
    x1<-x1+6
    x2<-x2+6
    y1<-y1+6
    y2<-y2+6
    lines(xrange[x1:x2],yrange[y1:y2], type="b", lwd=1.5,lty=i, col=colors[i+1], pch =plotchar[i+1])
  }

# add a title and subtitle
title(paste0("Alpha value = ",cc[l]), "")
}
# add a legend
plot(xrange[x1:x2], yrange[x1:x2],xlim=c(1,6), ylim=c(1,15), type="b", xlab="Community",ylab="Size of Community" ,pch=plotchar[1],col=colors[1])

legend("right",legend = unlist(line_table[[1]][,2][[1]]), cex=2.4, col=colors[1:19],
    pch=plotchar[1:19], lty=1:19, title="mu Values")
