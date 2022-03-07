# networks.R
# Network Creation and analysis #


######### Imports #####################
library(bibliometrix)
library(igraph)
library(RColorBrewer)

#######################################
set.seed(1)
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")
# read data in
MLData <- read.csv("../Data/ML/autoScreenedML.csv")
CTData <- read.csv("../Data/CT/autoScreenedCT.csv")
MLData$search <- 1
CTData$search <- 0
MLData$label <- "ML"
CTData$label <- "CT"
MLData$color <- "gold"
CTData$color <- "blue"



fullData <- merge(MLData, CTData, all = T) # all data together with which search
# now make a network?? 
#igraph::graph_from_data_frame(fullData)

names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
fullData$search <- as.numeric(fullData$search)

nodes <- fullData[c("ID", "title")] # slice of just ID and title
edges <- fullData[c("ID", "references")] # could be that I want cited_by here 
edges <- as.data.frame(table(edges))
edges <- subset(edges, Freq > 0)
# currently this is an undirected shared reference network I think 
# or at least it should be as those are the edges 
#edges <- fullData[c("ID", "cited_by")]
authors <- fullData[c("ID", "author")]
journal <- fullData[c("ID", "source_title")]
year <- fullData[c("ID", "year")]
search <- fullData[c("ID", "search")]
label <- fullData[c("ID", "label")]
color <- fullData[c("ID", "color")]
references <- fullData[c("ID", "references")]



net <- igraph::graph_from_data_frame(edges)
net <- set_vertex_attr(net, "title", value = nodes)
# plot(net)
net <- set_vertex_attr(net, "author", value = authors)
net <- set_vertex_attr(net, "journal", value = journal)
net <- set_vertex_attr(net, "year", value = year)
net <- set_vertex_attr(net, "search", value = search)

#net
#netDf <- as_long_data_frame(net)
#head(netDf)
#colnames(netDf)
# below syntax isnt working, will use the old fashioned way
# V(net)$search <- fullData$search
# V(net)$journal <- fullData$source_title
# V(net)$year <- fullData$year
# V(net)$references <- fullData$references
# V(net)$cited_by <- fullData$cited_by

# a plotting parameter
#layout <-layout.fruchterman.reingold(net)
#plot(net, layout=layout)
# degree(net)
# # that didn't work... ###
# filteredNet <- delete.vertices(simplify(net), degree(net)!=0)
# hist(degree(filteredNet))
# simplify(net)
####### 

# coords <- layout.auto(net)
# plot(simplify(net), layout = coords)
# plot(delete.vertices(simplify(net), degree(net)==0))
# 
# 
# cl<- clusters(net)
# layout <- layout.fruchterman.reingold(net)
# plot(net, layout=layout, vertex.label=NA, vertex.color=cl$membership+1L)
# dg <- decompose.graph(net) # connected components
# length(dg)
# plot(dg[[1]])
# plot(dg[[2]])
# 
# 
# ## for undirected nets ##
# eb <- edge.betweenness.community(net) # community detection
# membership <- cut_at(eb, no = 10)
# plot(net,
#      vertex.color= rainbow(10, .8, .8, alpha=.8)[membership],
#      vertex.size=5, layout=layout,  vertex.label=NA,
#      edge.arrow.size=.2)
# 
# # large graph eg
# layout <- layout.fruchterman.reingold(net)
# plot(net, layout=layout, vertex.size=2,
#      vertex.label=NA, edge.arrow.size=.2)

########## Net0 ############
head(fullData)
colnames(fullData)
edgeInteractions <- fullData[c("ID", "author", "cited_by")] # (edges as interactions?)
nodeAttributes <- fullData[c("ID", "title", "source_title", "year", "search", "references")]
title <- fullData[c("ID", "title")]

net0 <- graph.data.frame(edgeInteractions, directed = F)
net0 <- set_vertex_attr(net0, "title", value = title)
layout <- layout.fruchterman.reingold(net0)
plot(net0,
     layout=layout, 
     vertex.size=2,
     vertex.label=NA)
########### second attempt ###################
## This one works!  
## So references are probably too clunky 
## But using the 'cited_by' field works
##
nodes <- fullData[c("ID", "title")] # slice of just ID and title
edges <- fullData[c("ID", "cited_by")]
#edges <- as.data.frame(table(edges))
#edges <- subset(edges, Freq > 0)
net1 <- igraph::graph_from_data_frame(edges)
net1 <- set_vertex_attr(net1, "title", value = nodes)
# plot(net)
net1 <- set_vertex_attr(net1, "author", value = authors)
net1 <- set_vertex_attr(net1, "journal", value = journal)
net1 <- set_vertex_attr(net1, "year", value = year)
net1 <- set_vertex_attr(net1, "search", value = search)
net1 <- set_vertex_attr(net1, "references", value = references)

V(net1)$color <- ifelse(V(net1)$search == 1, "gold", "blue")
# large graph eg
layout <- layout.fruchterman.reingold(net1)
#pal <- brewer.pal(length(unique(V(net1)$search)), "Set3")
#colors = c("Red", "Blue")
plot(net1, layout=layout, vertex.size=2,
     vertex.label=NA, 
     #vertex.color=pal[as.numeric(as.factor(vertex_attr(net1, "search")))],
     #vertex.color= colors[as.numeric(as.factor(vertex_attr(net1, "search")))],
     vertex.color= fullData$search,
     edge.arrow.size=.2)

# cl<- clusters(net1)
# layout <- layout.fruchterman.reingold(net1)
# plot(net1, layout=layout, vertex.label=NA, vertex.color=cl$membership+1L)
# dg <- decompose.graph(net1) # connected components
# length(dg)
# plot(dg[[1]])

########### Third iteration; co-authors ######
# library(dplyr)
# library(stringr)
# library(statnet)
# library(intergraph)
# library(visNetwork)
# 
# fullData.coauthors = sapply(as.character(fullData$author), strsplit, ", ")
# fullData.coauthors = lapply(fullData.coauthors, trimws)
# fullData.coauthors = unique(unlist(fullData.coauthors))[order(unique(unlist(fullData.coauthors)))]
# full.bipartite.edges = lapply(fullData.coauthors, function(x) {unique(fullData.coauthors) %in% x})
# full.bipartite.edges = do.call("cbind", full.bipartite.edges) # dimension is number of authors x number of papers
# rownames(full.bipartite.edges) = unique(fullData.coauthors)
# fullData.mat = full.bipartite.edges %*% t(full.bipartite.edges) #bipartite to unimode
# mat = fullData.mat[order(rownames(fullData.mat)), order(rownames(fullData.mat))]
# 
# fullData.statnet = as.network(fullData.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
# fullData.statnet # view network summary
# plot(fullData.statnet)
# fullData.igraph = intergraph::asIgraph(fullData.statnet)
# layout <- layout.fruchterman.reingold(fullData.igraph)
# plot(fullData.igraph, layout = layout)
# visIgraph(fullData.igraph)
# 
# oc <- cluster_optimal()
# probably should have nodes as authors? 
# and edges as titles? 
# or is it authors as edges and titles as nodes?
edges <- fullData[c("ID", "title")]
nodes <- fullData[c("ID", "author")]
# nodes <- fullData[c("ID", "title")]
# edges <- fullData[c("ID", "author")]
net2 <- igraph::graph_from_data_frame(edges, directed = FALSE)
net2 <- set_vertex_attr(net2, "author", value = nodes)
#net2 <- set_vertex_attr(net2, "title", value = nodes)
#net2 <- set_vertex_attr(net2, "author", value = authors)
net2 <- set_vertex_attr(net2, "journal", value = journal)
net2 <- set_vertex_attr(net2, "year", value = year)
net2 <- set_vertex_attr(net2, "search", value = search)
net2 <- set_vertex_attr(net2, "references", value = references)
net2 <- set_vertex_attr(net2, "label", value = label)
net2 <- set_vertex_attr(net2, "searchCol", value = color)
# I have no idea what the underlying data structure of igraph looks like so no 
# real way to diagnose or fix any issues
# large graph eg
layout <- layout.fruchterman.reingold(net2)
class(V(net2)$search)


# answers <- read.table(textConnection(
#   "  Player Q1_I1                                                             
#     1      k     1                                                             
#     2      l     0                                                             
#     3      n     1                                                             
#     4      m     0                                                             
# "))
# 
# topology <- read.table(textConnection(
#   "  Node.1 Node.2                                                            
#     1      k      l                                                            
#     2      l      k                                                            
#     3      l      m                                                            
#     4      m      l                                                            
#     5      l      n                                                            
#     6      n      l                                                            
#     7      n      k                                                            
#     8      k      n                                                            
#  "))
# g2 <- graph.data.frame(topology, vertices=answers, directed=FALSE)
# g <- simplify(g2)
# V(g)$color <- ifelse(V(g)$Q1_I1 == 1, "lightblue", "orange")
# plot(g)






V(net2)$color <- ifelse(V(net2)$search == 1, "gold", "blue") #"1"
plot(net2, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

V(net2)$degree <- igraph::degree(net2)
# all attempts at trying to label this graph have failed
# searchCol <- V(net2)$search + 1
# searchCol <- coalesce(searchCol, 3) 
# searchCol <- c("steelblue", "tomato", "white")[searchCol]
# cbind(original = V(net2)$search[1:10],
#       colors   = searchCol[1:10])
net2Df <- as_long_data_frame(net2)
colnames(net2Df)
filteredNet2Df <- subset(net2Df, net2Df$to_degree > 1)
modifiedNet2 <- graph_from_data_frame(filteredNet2Df, directed = FALSE)
# V(modifiedNet2)$search
# plot(modifiedNet2, layout=layout, vertex.size=2,
#      vertex.label=NA, edge.arrow.size=.2)
# head(net2Df$to_degree)
# plot each connected subgraph separately
# searchCol <- V(modifiedNet2)$search + 1
# searchCol <- coalesce(searchCol, 3) 
# searchCol <- c("steelblue", "tomato", "white")[searchCol]

# cbind(original = V(modifiedNet2)$search[1:10],
#       colors   = searchCol[1:10])
dg <- decompose(modifiedNet2)
for (i in 1:length(dg)) {
  plot(dg[[i]],
       #vertex.label= c("CT", "ML"),
       #vertex.color=c("blue", "gold"))
       #vertex.label= )
       vertex.color= searchCol)
}
plot(modifiedNet2, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2, vertex.color = searchCol)

# cl<- clusters(net2)
# layout <- layout.fruchterman.reingold(net2)
# plot(net2, layout=layout, vertex.label=NA, vertex.color=cl$membership+1L)
# dg <- decompose.graph(net2) # connected components
# length(dg)
# plot(dg[[1]])



######### No. 4, titles linked by authors #########
nodes <- fullData[c("ID", "title")]
#edges <- fullData[c("ID", "cited_by")]
edges <- fullData[c("ID", "author")]
references <- fullData[c("ID", "references")]
net3 <- igraph::graph_from_data_frame(edges, directed = FALSE)
#net3 <- igraph::graph_from_data_frame(nodes, directed = FALSE)
#net3 <- set_vertex_attr(net3, "authors", value = edges)
#net3 <- set_vertex_attr(net3, "title", value = nodes)
net3 <- set_vertex_attr(net3, "search", value = search)
net3

#layout=layout.fruchterman.reingold(net3, niter=10000, area=30*vcount(net3)^2)

# Generate colors based on media type:
colrs <- c("blue", "gold")
V(net3)$color <- colrs[V(net3)$search]

layout = layout_with_fr(net3)

plot.igraph(simplify(net3), 
     vertex.color=net3$search,
     vertex.size=5, layout=layout, vertex.label=NA)

# Community detection based on label propagation:
clp <- cluster_label_prop(net3)
class(clp)
plot(clp, net3)



net3 <- set_vertex_attr(net3, "author", value = authors)
net3 <- set_vertex_attr(net3, "journal", value = journal)
net3 <- set_vertex_attr(net3, "year", value = year)
net3 <- set_vertex_attr(net3, "search", value = search)
net3 <- set_vertex_attr(net3, "references", value = references)
## for undirected nets ##
eb <- edge.betweenness.community(net3) # community detection
membership <- cut_at(eb, no = 10)
plot(net3,
     vertex.color= rainbow(10, .8, .8, alpha=.8)[membership],
     vertex.size=5, layout=layout,  vertex.label=NA,
     edge.arrow.size=.2)




############ OK trying again... ###########
edges <- fullData[c("ID", "author")]
edges
verticiesData <- fullData[ , !(names(fullData) %in% c("author"))]
verticiesData
net4 <- igraph::graph_from_data_frame(edges, directed = FALSE)
eb <- edge.betweenness.community(net4) # community detection
membership <- cut_at(eb, no = 10)
plot(net4,
     vertex.color= rainbow(10, .8, .8, alpha=.8)[membership],
     vertex.size=5, layout=layout,  vertex.label=NA,
     edge.arrow.size=.2)

filteredNet <- delete.vertices(simplify(net4), degree(net4) < 1)
hist(degree(filteredNet))
secondPassNet <- delete.vertices(simplify(filteredNet), degree(filteredNet) == 1)
hist(degree(secondPassNet))


########### data generation for below ########
library('dplyr')
library('igraph')
library('RColorBrewer')


set.seed(1)

# generate a couple clusters
nodes_per_cluster <- 30
n <- 10

nvals <- nodes_per_cluster * n

# cluster 1 (increasing) 
cluster1 <- matrix(rep((1:n)/4, nodes_per_cluster) + 
                     rnorm(nvals, sd=1),
                   nrow=nodes_per_cluster, byrow=TRUE)

# cluster 2 (decreasing)
cluster2 <- matrix(rep((n:1)/4, nodes_per_cluster) + 
                     rnorm(nvals, sd=1),
                   nrow=nodes_per_cluster, byrow=TRUE)

# noise cluster
noise <- matrix(sample(1:2, nvals, replace=TRUE) +
                  rnorm(nvals, sd=1.5),
                nrow=nodes_per_cluster, byrow=TRUE)

dat <- rbind(cluster1, cluster2, noise)
colnames(dat) <- paste0('n', 1:n)
rownames(dat) <- c(paste0('cluster1_', 1:nodes_per_cluster), 
                   paste0('cluster2_', 1:nodes_per_cluster),
                   paste0('noise_',    1:nodes_per_cluster))

#dat

### clustering bits ? ###
# create correlation matrix
cor_mat <- cor(t(dat))

# shift to [0,1] to separate positive and negative correlations
adj_mat <- (cor_mat + 1) / 2

# get rid of low correlations and self-loops
adj_mat <- adj_mat^3
adj_mat[adj_mat < 0.5] <- 0
diag(adj_mat) <- 0
# convert to dissimilarity matrix and cluster using hclust
dissim_mat <- 1 - adj_mat

dend <- dissim_mat %>% 
  as.dist %>% 
  hclust

clusters = cutree(dend, h=0.65)

# color the nodes
pal = colorRampPalette(brewer.pal(11,"Spectral"))(length(unique(clusters)))
node_colors <- pal[clusters]
# create graph
g <- graph.adjacency(adj_mat, mode='undirected', weighted=TRUE)

# set node color and plot using a force-directed layout (fruchterman-reingold)
V(g)$color <- node_colors
coords_fr = layout.fruchterman.reingold(g, weights=E(g)$weight)

# igraph plot options
igraph.options(vertex.size=8, edge.width=0.75) 

# plot network
plot(g, layout=coords_fr, vertex.color=V(g)$color)















########################################
######### Bibliometrix ################
# doesnt work lol
#######################################

path <- "../Data/LitSearches/CT/SCOPUSFinal/scopus(31).csv"

M <- convert2df(path, dbsource = "scopus", format = "csv") # not sure we need to bother with this 
# could just read in the file as a csv and make as df since have processed already 


convert2df(MLData, dbsource = "scopus", format = "csv")

#file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

#M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]
DF <- dominance(results, k = 10)
DF
indices <- Hindex(M, field = "author", elements="BORNMANN L", sep = ";", years = 10)

# Bornmann's impact indices:
indices$H
# Bornmann's citations
indices$CitationList
authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)

indices$H
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
## Table: Author's productivity per year
head(topAU$dfAU)
L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd
# Beta coefficient estimate
L$Beta
# Constant
L$C
# Goodness of fit
L$R2
# P-value of K-S two sample test
L$p.value
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
A <- cocMatrix(M, Field = "CR", sep = ".  ")
A <- cocMatrix(M, Field = "AU", sep = ";")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "DE", sep = ";")
A <- cocMatrix(M, Field = "ID", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")


#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# An example of a classical keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)
names(netstat$network)
names(netstat$vertex)
summary(netstat, k=10)
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
# Create a co-citation network

# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
#net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, n=15, size = 10, labelsize=5)
