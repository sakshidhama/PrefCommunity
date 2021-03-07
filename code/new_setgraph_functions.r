#Final new improved code 11th June 2020
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
pacakge_list <- c("igraph", "igraphdata")
new_packages <- pacakge_list[!(pacakge_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
library(igraph)
library(igraphdata)
real_data<-function()
{
  setwd("/Users/sakshidhama/Dropbox/work/community_preference")
  mydir<-"data_setgraph/real_dataset"
  real_myfiles    <- list.files(path =mydir ,pattern = "*.txt",full.names = TRUE)
  real_file_table <- real_myfiles %>% lapply(read.table)
  real_graph_data <- real_file_table %>% lapply(graph.data.frame)
  real_graph_list <- real_graph_data %>% lapply(as.undirected)
  return(real_graph_list)
  ##################
}
#Function for rescaling the nodes size 
#plot(net,vertex.cex=rescale(deg,1,6),display.labels=T,label.color="darkblue",main="Adjusted node sizes with rescale function.")
rescale<-function(nchar,low,high)
{
  min_d<-min(nchar)
  max_d<-max(nchar)
  rscl<-((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

#Function to read the files 
readmyfiles<-function(){
setwd("/Users/sakshidhama/Dropbox/work/community_preference")
mydir<-"data_setgraph/synthetic_datasets"
myfiles    <- list.files(path =mydir ,pattern = "*.dat",full.names = TRUE)
file_table <- myfiles %>% lapply(read.table)
graph_data <- file_table %>% lapply(graph.data.frame)
graph_list <- graph_data %>% lapply(as.undirected)
#Set graph parameters here
real_graph_list<-real_data()
g<-real_graph_list[[1]]
set_vertex_attr(g,"kin",value = 0)
set_vertex_attr(g,"kout",value = 0)
set_vertex_attr(g,"comm",value = 0)
V(g)$degree<- igraph::degree(g)
V(g)$kin<-0
V(g)$kout<-0
V(g)$color<-"red"
#Set neighbors for each node in the graph
#foreach (h = 1:vcount(g)) %do% {
# V(g)[h]$neighbor<- list(neighbors(g,V(g)[h]))
#print(neighbors(g,V(g)[h]))  }
get_neighbors<-function(v){
  return(neighbors(g,v))
}
V(g)$neighbor<-lapply(V(g),get_neighbors)
return(g)
}

#Function to take user input of communities
readinteger <- function()
{ 
  n <- readline(prompt=paste("Enter a number for communties to be created on  graph of size:  ",vcount(g)," : "))
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  
  return(n)
}

#Fitness function
fit_fun<-function(kin,kout)
{
  alpha<-1
  p<-kin/(kin+kout)
  #print(p)
  return(p)
}
#Pereference Function
preference_fun<-function(x0,y0,v0)
{
  alp<-1
  x<-x0
  y<-y0
  v<-v0
  print(paste0("x ix ",x,"  y is ",y))
  #print("h")
  #print(y)
  pref_val<-1/(1+(((1-v)/v)*(((1-y)/y)*(x/(1-x)))^alp))
  print(pref_val)
  return(pref_val)
}
#Function to select the number of communties to be created
select_comm_head<-function(g)
{
  g<-g
  ### Here we ask user to input the number of communities to be created on given graph
  nc<-readinteger() ## This variable holds the number of communites
  #Randomly choosing the head for communties
  head_in<-list()
  adsum<-floor((as.numeric(vcount(g)))/(as.numeric(nc)))
  head_in<-V(g)[1]
  for (tc in 1:nc) 
  {
    ad_index<-(adsum*tc)
    head_in<-append(head_in,V(g)[ad_index])
  }
  read_comm<-function(h){
    node_list<-unlist(h)
    return(induced_subgraph(g,h))
  }
  #Comunities with one node each
  sbgraph <- head_in %>%lapply(read_comm)
  return(sbgraph)
}
#calculation of community function
calculation<-function(sbgraph,v)
{
  g<-readmyfiles()
  s<-sbgraph
  v0<-v
  chk<-1
  sprev<-s
  kin<-0
  kout<-0
  while(((igraph::vcount(s))>(igraph::vcount(sprev)) && (igraph::vcount(g)>vcount(s))) || chk==1){
    sprev<-s
    kin<-(igraph::ecount(s))#Kin for the subgraph community Gn#dont move thi line after if clause
    if(chk==1)
    {
      kin<-1
      kout<-1
      ##print("inside if")
      preference_list<-list()
      fitness_list<-list()
    }
    preference_list<-0
    fitness_list<-0
    templist<-0 
    kout<-sum(V(g)[V(s)$name]$degree)-kin #Kout for the subgraph community
    x<-fit_fun(kin,kout)#Fitness of subgraph
    templist1 <- unique(do.call(c, V(s)$neighbor))#flatten the list of list neighbor list in one
    #templist1<-NCmisc::Unlist(neighborlist[[1]])#flatten the list of list neighbor list in one
    templist<-setdiff(templist1,V(s)$name)#excluding nodes which are already in s subgraph
    foreach (i = 1:length(templist)) %do% {
      q<-unique(c(V(g)[c(templist[i])],V(g)[V(s)$name]))#nodes ther in new subgraph Gn+1
      w<-induced.subgraph(g,V(g)[q])
      kin_new<-(igraph::ecount(w))#edges inside Gn+1
      kout_new<-sum(V(g)[V(w)$name]$degree)-kin_new#Edges outside Gn+1
      f<-fit_fun(kin_new,kout_new) #Fintess for Gn+1 after adding node m
      fitness_list[i]<-f # Storing this fitness list which has fitness of all neigbors nodes if they are in Gn+1
      preference_list[i]<-preference_fun(x,fitness_list[i],v0)  #Calc Preference Realtion 
    }
    
    #d<-v0
    initial_discount<-0.24#Initial relaxation given to first member of the community by lowering the threshold value
    comm_number<-1
    delta_threshold_list<-seq(0.55,0.95,by=0.05)
    node_index<-list()
    #if list gives initial discount to start the community
    if(chk==1)
    {
      node_index<-which(preference_list>=initial_discount)
    }
    if(chk>1) #else loop calculates the node index of neighbors stored in templist whose prefrence is greater than v0
    {
      delta<-delta_threshold_list[chk]
      node_index<-which(preference_list>delta)
    }
    nodelist<-V(s)$name
    s<-induced.subgraph(g,unique(c(nodelist,templist[node_index])))
    kin<-igraph::ecount(s)#Kin for the subgraph community Gn
    kout<-sum(V(g)[V(s)$name]$degree)-kin #Kout for the subgraph community
    chk<-chk+1
  }
  print(paste0(" The check value is ",chk))
  
  return(s)
}
