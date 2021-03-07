#Final new improved code 11th june 2020
library(igraph)
library(NCmisc)
library(statnet)
library(intergraph)
library(RColorBrewer)
library(poweRlaw)
library(sets)
library(data.table)
library(ggplot2)
library(magrittr)
library(parallel)
library(foreach)
source("new_setgraph_functions.r")

# mu parameter value in algortihm stored in v0_list
#v0_list<-seq(0.25,0.80,by=0.025)
v0_list<-0.75
#Applying Function on one graph with community heads stored in sbgraph 
g<-readmyfiles()
sub_graph <- select_comm_head(g)
#result<- lapply(sub_graph,calculation,v=0.7)# result list holds the number of communities created
result_list<-list()
    foreach (i = 1:length(v0_list)) %do% 
  {
    no_cores <- detectCores() - 1
    base <- 2
    cl <- makeCluster(no_cores)
    # Initiate cluster
    clusterExport(cl,"base")
    clusterEvalQ(cl, {
      library(NCmisc)
      library(statnet)
      library(intergraph)
      library(RColorBrewer)
      library(poweRlaw)
      library(sna)
      library(sets)
      library(data.table)
      library(ggplot2)
      library(magrittr)
      library(parallel)
      library(foreach)
      source("new_setgraph_functions.r")
    })
    result_list[i]<-list(parLapply(cl,sub_graph,calculation,v=v0_list[i]))# result_list holds the number of communities created
  }
    stopCluster(cl)

df<-data.frame()#df datastrucure stores list of communties szies in each graph row by row
for (i in 1:length(v0_list)) {
  df<-c(df,i)
  for (j in 1:length(sub_graph)) {
    df<-c(df,vcount(result_list[[i]][[j]]))
  }
}
#df<-df[-1]
col_names<-c("mu_value",paste("C",1:(length(sub_graph))))
ddf<-matrix(unlist(df),ncol = (length(sub_graph)+1),nrow = length(v0_list),byrow = TRUE)
colnames(ddf)<-col_names
rownames(ddf)<-1:length(v0_list)
ddf[,1]<-v0_list

overlap<-function(result_list)
{
  matrix_list<-list()
  for(r in 1:length(result_list))
  {
    overlap_matrix<-matrix(0,nrow=vcount(g),ncol=length(sub_graph))
    for(j in 1:length(sub_graph))
    {
      vert_ices<-unlist(V(result_list[[r]][[j]]))
      overlap_matrix[c(vert_ices),j]<-1
    }
    print(overlap_matrix)
    matrix_list[[r]]<-overlap_matrix
  }
  community_size<-list()
  overlap_nodes<-list()
  for (i in 1:length(matrix_list)) {
    community_size[[i]]<-colSums(matrix_list[[i]])
    overlap_nodes[[i]]<-rowSums(matrix_list[[i]])
  }
}
