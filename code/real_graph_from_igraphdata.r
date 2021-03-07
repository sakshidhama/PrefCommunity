library(igraph)
library(igraphdata)
#This function reads the real graph data from web and igraphdata and converts it into igraph format
# All the directed graphs are converted into the undirected format for our requirement purpose
read_real_graphs<-function()
{
real_graph<-list()
data("immuno") ## example network
real_graph[[1]] <-immuno
r<-get.adjacency(real_graph[[1]])
r1<-graph_from_adjacency_matrix(r)
r2<-as.undirected(r1)
real_graph[[1]]<-r2
######################
tmp <- tempdir()
url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/UciNet/zachary.dat"
dest <- paste(tmp, sep="/", "k.dat")
download.file(url=url, destfile=dest)
l <- readLines(dest)
l <- l[(grep("^DATA", l)+1):length(l)]
l1 <- matrix(scan(textConnection(paste(l[1:34], collapse="\n"))), nr=34)
l2 <- matrix(scan(textConnection(paste(l[1:34+34], collapse="\n"))), nr=34)
karate <- graph_from_adjacency_matrix(l2, weighted=TRUE, mode="undirected")
real_graph[[2]]<-karate
######################
data("enron")
real_graph[[3]]<-enron
r<-get.adjacency(real_graph[[3]])
r1<-graph_from_adjacency_matrix(r)
r2<-as.undirected(r1)
real_graph[[3]]<-r2
######################3
data("rfid") ## example network
real_graph[[4]]<-rfid
r<-get.adjacency(real_graph[[4]])
r1<-graph_from_adjacency_matrix(r)
r2<-as.undirected(r1)
real_graph[[4]]<-r2
###################
data("kite") ## example network
real_graph[[5]]<-kite
r<-get.adjacency(real_graph[[5]])
r1<-graph_from_adjacency_matrix(r)
r2<-as.undirected(r1)
real_graph[[5]]<-r2
###################
data("Koenigsberg") ## example network
real_graph[[6]]<-Koenigsberg
r<-get.adjacency(real_graph[[6]])
r1<-graph_from_adjacency_matrix(r)
r2<-as.undirected(r1)
real_graph[[6]]<-r2
#####################
return(real_graph)
}