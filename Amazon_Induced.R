rm(list=ls())

getwd()
infile<-"C:\\Users\\sushaanth\\Documents\\IDS_564_notes\\project"
setwd(infile)
amazon<- read.csv("amazon_ind.csv")

library(igraph)

amazon_graph<-graph.data.frame(amazon, directed = FALSE)
plot(degree.distribution(amazon_graph),main = "Amazon Degree Distribution", xlap = "deg", ylab = "AmazonProd")
# The graph shows as preferntial graph

#Adjacency Matrix
as_adjacency_matrix(amazon_graph)

# edge betweeneness community detection algorithm
amazon_edge<-edge.betweenness.community(amazon_graph)
amazon_edge$bridges
write.csv(table(amazon_edge$bridges), file = "bridge.csv")

amcom<-communities(amazon_edge)
len<- do.call(rbind,lapply(amcom,length))
sort(len)


big_comm<-induced.subgraph(amazon_graph,c(amcom$`1`,amcom$`17`))
big_comm2<-induced.subgraph(amazon_graph,c(amcom$`1`,amcom$`17`,amcom$`26`,amcom$`13`))

summary(big_comm2)

write.graph(big_comm, file = "big_comm.graphml",format = "graphml")
write.graph(big_comm2,file = "big_comm2.graphml",format = "graphml")



# modularity of nodes
mods <- sapply(0:ecount(amazon_graph), function(i) {
  g2 <- delete.edges(amazon_graph, amazon_edge$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(amazon_graph, cl)
})


#function for deg distibution of nodes in amazon network
count.deg.distribution <- function (amazon_graph, cumulative = FALSE) 
{
  if (!is.igraph(amazon_graph)) {
    stop("Not a graph object")
  }
  cs <- degree(amazon_graph)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$count
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  res
}

ag<- count.deg.distribution(amazon_graph)
tab<-table(ag)
write.csv(tab,file = "dd.csv")

# community to membership
com<-cutat(amazon_edge, steps = which.max(amazon_edge$modularity)-1)
modularity(amazon_graph,com)
# modularity 0.8637767, means communities are very well defined


#clustering coefficient
transitivity(amazon_graph)
# 0.2178612 

# List of Communities
amazon_comm<- communities(amazon_edge)
# total 27 communities
sizes(amazon_edge)
#largest node = 1 with 186 number of nodes linked to it

#division of vertices into communities
membership(amazon_edge)


#clique
larclique<-largest.cliques(amazon_graph)
maxclique<- maximal.cliques(amazon_graph)

diameter(amazon_graph)
# 24

is.connected(amazon_graph, mode = "strong")


ecount(amazon_graph)
vcount(amazon_graph)
is.weighted(amazon_graph)

is.simple(amazon_graph)


#largest component

g.components <- clusters(amazon_graph)
h <- which.max(g.components$csize)
amazon_large <- induced.subgraph(amazon_graph,which(g.components$membership==h))
deg <- degree(amazon_large,v=V(amazon_large))
sort(deg)
length(nop)
nop <- hist(deg, -1:max(deg),plot=FALSE)$counts
deg1 <- seq.int(0,length(nop)-1,1)
amazon_deg <- cbind.data.frame(deg1,nop)

tab<-table(sizes(amazon_edge))
write.csv(tab,file = "commSize.csv")

# Amazon netork is a hierarchical structure, plotting dendogram 
amazon_dend<-as.dendrogram(amazon_edge,use.modularity = TRUE, hang = -1)
plot(amazon_dend,main = "Cluster dendogram", xlab = NULL)

#plotting graph in gephi to identify influential nodes and graph features
write.graph(amazon_graph, format = "graphml", file = "AmazonGephi.graphml")

#Preferential network - Equilibrium alpha values

alpha0<-c()
alpha1<-c()

tot_nop<-sum(amazon_deg$nop)
tot_deg<-sum(amazon_deg$degr*amazon_deg$nop)
amazon_deg$fd<-amazon_deg$nop/tot_nop
amazon_deg$Fd=cumsum(amazon_deg$fd)
average_deg<-tot_deg/tot_nop
m=average_deg/2
amazon_deg$y=log(1-amazon_deg$Fd)

#Taking Alpha0 value to find equilibrium solution for amazon network

#0.11 is the equilibrium solution for amazon network with (alpha0 = 0.11, alpha1 = 0.111547), closest to 0, that explains it is preferential network

alpha_0 = 0.11
amazon_deg$x_alpha0.11=c()

for(j in 1:86){
  amazon_deg$x_alpha0.11[j]<-log(amazon_deg$deg[j]+alpha_0*m*2/(1-alpha_0))
}

reg.11<-lm(amazon_deg$y[2:85]~amazon_deg$x_alpha0.11[2:85])
alpha_1 <- (2/reg.11$coefficients[2])+1
alpha0[2] = 0.1
alpha1[2] = alpha_1

alpha_0 = 0.1
amazon_deg$x_alpha0.1=c()

for(j in 1:86){
  amazon_deg$x_alpha0.1[j]<-log(amazon_deg$deg[j]+alpha_0*m*2/(1-alpha_0))
}

reg.1<-lm(amazon_deg$y[2:85]~amazon_deg$x_alpha0.1[2:85])
alpha_1 <- (2/reg.1$coefficients[2])+1
alpha0[2] = 0.1
alpha1[2] = alpha_1


alpha_0 = 0.2
amazon_deg$x_alpha0.2=c()

for(j in 1:86){
  amazon_deg$x_alpha0.2[j]<-log(amazon_deg$deg[j]+alpha_0*m*2/(1-alpha_0))
}

reg.2<-lm(amazon_deg$y[1:85]~amazon_deg$x_alpha0.2[1:85])
alpha_1 <- (2/reg.2$coefficients[2])+1
alpha0[3] = 0.2
alpha1[3] = alpha_1

alpha_0 = 0.5
amazon_deg$x_alpha0.5=c()
for(j in 1:86){
  amazon_deg$x_alpha0.5[j]<-log(amazon_deg$deg[j]+alpha_0*m*2/(1-alpha_0))
}

reg.5<-lm(amazon_deg$y[1:85]~amazon_deg$x_alpha0.5[1:85])
alpha_1 <- (2/reg.5$coefficients[2])+1
alpha0[4] = 0.5
alpha1[4] = alpha_1

alpha_0 = 0.9
amazon_deg$x_alpha0.9=c()
for(j in 1:86){
  amazon_deg$x_alpha0.9[j]<-log(amazon_deg$deg[j]+alpha_0*m*2/(1-alpha_0))
}

reg.9<-lm(amazon_deg$y[1:85]~amazon_deg$x_alpha0.9[1:85])
alpha_1 <- (2/reg.9$coefficients[2])+1
alpha0[5] = 0.9
alpha1[5] = alpha_1

plot(alpha0,abs(alpha1),main = "Alpha values to find Equilibrium")

