library("readxl")
library(xlsx)
library(caret)
library(data.table)
library(ggplot2)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
setwd("/Users/sakshidhama/Dropbox/work/community_preference")
mydir<-"/Users//sakshidhama//Dropbox//work//community_preference//all_overlap_matrix"
myfiles    <- list.files(path =mydir ,pattern = "*.xlsx",full.names = TRUE)
A<-list()
membership<-list()
member<-list()
zvalues<-list()
maxlist<-list()
minlist<-list()
communitysize<-list()
normalizemembership<-list()
A<- lapply(myfiles,read_excel)
for (i in 1:length(myfiles)) {
  A[[i]]<-A[[i]][-1]
  A[[i]]<-as.matrix(A[[i]])
  membership[[i]]<-data.frame(as.numeric(unlist(rowSums(A[[i]]))))[[1]]
  normalizemembership[[i]]<-range01(membership[[i]])
  member[[i]]<-rep(i,length(A[[i]]))
  zvalues[[i]]<-rep(paste0(i),length(A[[i]]))
  communitysize[[i]]<-data.frame(as.numeric(unlist(colSums(A[[i]]))))[[1]]
  maxlist[[i]]<-max(communitysize[[i]])
  minlist[[i]]<-min(communitysize[[i]])
}
#B <- as.matrix(A)
#membership<-data.frame(as.numeric(unlist(rowSums(B))))[[1]]
#normalizedmembership<-range01(membership)
normalizedata<-data.table(unlist(member),unlist(normalizemembership),zvalues)
png(file = "Membership.png",width = 1600 ,height = 800 ,res =120)
plotmember<- ggplot(normalizedata,aes(x=normalizedata$zvalues,y=normalizedata$V2)) +xlab("Normalized membership of Networks") + ylab("Membership of Nodes") + labs(fill= "Membership")+
  geom_violin(aes(fill=normalizedata$V1),draw_quantiles = TRUE)+ scale_y_log10() + geom_boxplot(varwidth = TRUE, alpha=0.2)+
  scale_color_npg()
 grid.arrange(plotmember)
 dev.off()
 