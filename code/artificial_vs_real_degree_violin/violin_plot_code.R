library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(plyr)
library(readr)
library(igraph)
library(data.table)
library(RColorBrewer)
library(wesanderson)
library(ggsci)
library(gridExtra)
source("new_setgraph_functions.r")
real_graph_list<-real_data()
#function to read many files together
setwd("/Users/sakshidhama/Dropbox/work/community_preference")
mydir<-"data_setgraph/synthetic_datasets"
myfiles    <- list.files(path = mydir,pattern = "*.dat",full.names = TRUE)
file_table <- myfiles %>% lapply(read.table)
graph_data <- file_table %>% lapply(graph.data.frame)
lfr_graph_list <- graph_data %>% lapply(as.undirected)
#net_list   <- file_table %>% lapply(network,matrix.type="edgelist")
#function to assign each graph object graph number ie g1,g2... gn
myfunc1<-function(g,deglist)
{
  graph_list<-g
  temp<-vector(mode = "list",length = length(deglist))
  for (i in 1:length(graph_list)) {
    graph_name<-""
    temp<-c(temp,rep(paste0(graph_name,vcount(graph_list[[i]])),length(graph_list[[i]][1])))
    print(i)
  }
  temp<-na.omit(unlist(temp))
  return(temp)
}

color_brewer_mycolors <-c( "#edf8fb", "#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#6e016b") 
real_degre_list<- real_graph_list %>% lapply(igraph::degree) #this list contains the degree of all the graphs 
lfr_degre_list<- lfr_graph_list %>% lapply(igraph::degree) #this list contains the degree of all the graphs 
#column_nam <- c("Degree","Graph")
dr1_<- unlist(real_degre_list %>% lapply(unlist))
dr2_<- unlist(lfr_degre_list %>% lapply(unlist))
vg1 <- data.table(dr1_,myfunc1(real_graph_list,real_degre_list))
vg2 <- data.table(dr2_,myfunc1(lfr_graph_list,lfr_degre_list))
#basic command : ggplot(vg1,aes(x=vg1$V2,y=vg1$dr))  + geom_violin() + scale_y_log10()
#Saving the plot in file
png(file = "DataFlair_plot.png",width = 1600 ,height = 800 ,res =120)
dp1 <- ggplot(vg1,aes(x=vg1$V2,y=vg1$dr1_)) +xlab("Real Networks") + ylab("Degree") + labs(fill= "Networks")+
  geom_violin(aes(fill=vg1$V2),draw_quantiles = TRUE)+ scale_y_log10() + geom_boxplot(varwidth = TRUE, alpha=0.2)+
  scale_color_npg()+theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                          axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                          axis.title.x = element_text(color = "grey20", size = 22, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                          axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                          legend.title = element_text(color = "black", size = 20),
                          legend.text = element_text(color = "black", size = 20))
# + scale_color_npg()

dp2 <- ggplot(vg2,aes(x=vg2$V2,y=vg2$dr2_)) +xlab("Aritficial Networks") + ylab("Degree") + labs(fill= "Networks")+
  geom_violin(aes(fill=vg2$V2),draw_quantiles = TRUE)+ scale_y_log10() + geom_boxplot(varwidth = TRUE, alpha=0.2)+
  scale_color_npg()+theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                          axis.text.y = element_text(color = "grey20", size = 18, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                          axis.title.x = element_text(color = "grey20", size = 22, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                          axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                          legend.title = element_text(color = "black", size = 20),
                          legend.text = element_text(color = "black", size = 20))
# + scale_color_npg()
#To arrange graphs dp1 and dp2 in a grid
grid.arrange(dp1,dp2,ncol=2)
#saving file
dev.off()
       
       