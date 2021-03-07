library(igraph)
library(ggplot2)
library(xlsx)
library(readxl)
library(readr)
library(grid)
library(gridExtra)
library(data.table)
violin_data<-list()
setwd("/Users/sakshidhama/Dropbox/work/community_preference/data_setgraph")
mydir<-"violinfiles"
community_files<-list.files(path=mydir,pattern = "*.xlsx",full.names = TRUE)
file_table<-lapply(community_files, read_excel)
xlist<-list()
xvalues<-list()
yvalues<-list()
zvalues<-list()
col<-list()
zchar<-"a"
for (i in 1:14) {
  xlist[i]<-file_table[[2]][i+1,2]
  xvalues[[i]]<-rep(xlist[i],201)
  yvalues[[i]]<-file_table[[1]][i+1,3:19]
  zvalues[[i]]<-paste0(xvalues[[i]])
  col[[i]]<-rep(i,1,14)
}

graphs_dataframe<-data.table(x=unlist(xvalues),y=unlist(yvalues),z=unlist(zvalues))
png(file = "violin2.png",width = 1600 ,height = 500 ,res =120)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0))

#newdp2 <- ggplot(graphs_dataframe,aes(x=graphs_dataframe$z,y=graphs_dataframe$y)) +xlab(expression(paste(nu))) + ylab("Community size") + labs(fill=expression(paste(nu," values")))+
 # geom_violin(aes(fill=graphs_dataframe$x),draw_quantiles = TRUE)+ scale_y_log10() + geom_boxplot(varwidth = TRUE, alpha=0.2)+theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"),
 #                                                                                                                                   axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
 #                                                                                                                                   axis.title.x = element_text(color = "grey20", size = 28, angle = 0, hjust = .5, vjust = 0, face = "plain"),
 #                                                                                                                                   axis.title.y = element_text(color = "grey20", size = 28, angle = 90, hjust = .5, vjust = .5, face = "plain"),
 #                                                                                                                                   legend.title = element_text(color = "black", size = 22),
 #                                                                                                                                 legend.text = element_text(color = "black", size = 20))

newdp2 <- ggplot(graphs_dataframe,aes(x=graphs_dataframe$z,y=graphs_dataframe$y))+ scale_y_log10() +xlab(expression(paste(nu))) + ylab("Community size") + labs(fill=expression(paste(nu," values")))+
geom_violin(aes(),draw_quantiles = TRUE) +geom_jitter(alpha=0.5, aes(color=graphs_dataframe$z),position = position_jitter(width = .2))+
  theme(axis.text.x = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"),axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                                                                                                                                                                                                                             axis.title.x = element_text(color = "grey20", size = 28, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                                                                                                                                                                                                                             axis.title.y = element_text(color = "grey20", size = 28, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                                                                                                                                                                                                                             legend.title = element_text(color = "black", size = 20),
                                                                                                                                                                                                                             legend.text = element_text(color = "black", size = 18))
grid.arrange(newdp2)
dev.off()
