library(igraph)
source("/Users/sakshidhama/Dropbox/work/community_preference/new_setgraph_functions.r")
  setwd("/Users/sakshidhama/Dropbox/work/community_preference")
  ###############artificial data##########################
 # mydir<-"data_setgraph/synthetic_datasets"
  #myfiles    <- list.files(path =mydir ,pattern = "*.dat",full.names = TRUE)
  #graph_data <- file_table %>% lapply(graph.data.frame)
  #file_table <- myfiles %>% lapply(read.table)
  #graph_list <- graph_data %>% lapply(as.undirected)
#####################Real Data ###########
real_graph_list<-real_data()
graph_list<-real_graph_list
#write.graph(real_graph_list[[1]],"first_graph.dat",format = "edgelist")
#igraph::set.vertex.attribute(real_graph_list[[1]], "name", value=paste("",1:vcount(real_graph_list[[1]]),sep=""))
########################################
print("Graph statisices : ")
graph_statistics<-list()
graph_statistics$V<-lapply(graph_list, vcount)
graph_statistics$E<-lapply(graph_list, ecount)
graph_statistics$density<-lapply(graph_list, graph.density)
graph_statistics$diameter<-lapply(graph_list, diameter)
graph_statistics$transitivity<-lapply(graph_list, transitivity)
graph_statistics$degree<-lapply(graph_list, igraph::degree)
graph_statistics$maxdegree<-lapply(graph_statistics$degree,max)
graph_statistics$avgdegree<-lapply(graph_statistics$degree,mean)
write.csv(graph_statistics$V,"Vertices.csv")
write.csv(graph_statistics$E,"Edges.csv")
write.csv(graph_statistics$density,"Density.csv")
write.csv(graph_statistics$diameter,"Diameter.csv")
write.csv(graph_statistics$transitivity,"Transitivity.csv")
write.csv(graph_statistics$maxdegree,"Max_degree.csv")
write.csv(graph_statistics$avgdegree,"Avg_degree.csv")
################################################
