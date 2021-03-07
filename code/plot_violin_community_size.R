library(igraph)
library(ggplot2)
library(xlsx)
library(readxl)
library(readr)
library(grid)
library(gridExtra)
library(data.table)
violin_data<-list()
setwd("/Users/sakshidhama/Dropbox/work/community_preference/refrences/paper_results/10percent_seed")
mydir<-"1000_seed_comm"
community_files<-list.files(path=mydir,pattern = "*.csv",full.names = TRUE)
file_table<-lapply(community_files, read_csv)
xlist<-list()
xvalues<-list()
yvalues<-list()
zvalues<-list()
zchar<-"a"
for (i in 1:length(file_table)) {
  
  xlist[[i]]<-file_table[[i]][c(-1,-2)]
  xvalues[[i]]<-seq(1:length(xlist[[i]]))
  yvalues[[i]]<-as.numeric(unlist(xlist[[i]]))
  zvalues[[i]]<-rep(paste0(i," = ",seed_text[i]),length(xlist[[i]]))
}
seed_text<-c("5 %","10 %","20 %","30 %")
graphs_dataframe<-data.table(x=unlist(xvalues),y=unlist(yvalues),z=unlist(zvalues))
png(file = "community_size_1000.png",width = 1600 ,height = 800 ,res =120)
newdp2 <- ggplot(graphs_dataframe,aes(x=graphs_dataframe$z,y=graphs_dataframe$y)) +xlab("Seed Nodes % for Network Size = 1000") + ylab("Community Size ") + labs(fill= "Seed Size ")+
  geom_violin(aes(fill=graphs_dataframe$z),draw_quantiles = TRUE)+ scale_y_log10() + geom_boxplot(varwidth = TRUE, alpha=0.2)+
  scale_color_gradient2()+theme(axis.text.x = element_text(color = "grey20", size = 28, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                                axis.text.y = element_text(color = "grey20", size = 28, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                                axis.title.x = element_text(color = "grey20", size = 28, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                                axis.title.y = element_text(color = "grey20", size = 28, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                                legend.title = element_text(color = "black", size = 28),
                                legend.text = element_text(color = "black", size = 28))
grid.arrange(newdp2)
dev.off()
