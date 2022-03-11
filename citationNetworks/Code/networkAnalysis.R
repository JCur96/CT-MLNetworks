# networkAnalysis.R
set.seed(1)
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")
# imports
library(igraph)
library(RColorBrewer)

# read in edgeList data
edges <- read.csv("../Data/edges/edgeList.csv")

# read data in
MLData <- read.csv("../Data/ML/autoScreenedML.csv")
CTData <- read.csv("../Data/CT/autoScreenedCT.csv")
MLData$search <- 2
CTData$search <- 1
MLData$label <- "ML"
CTData$label <- "CT"
MLData$color <- "gold"
CTData$color <- "blue"

fullData <- merge(MLData, CTData, all = T) 

names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
fullData$search <- as.numeric(fullData$search)

# fullData$search
# fullData$label
# fullData$color
# fullData$title

# fullDataOdered <- fullData[order(fullData$search),]


colnames(edges)
edges <- edges[c("CitingID", "CitedID")]
colnames(edges)
# edges <- as.data.frame(table(edges))
# edges <- subset(edges, Freq > 0)
# needs editing to refelect reality below here 

nodes <- fullData[c("ID")] # slice of just ID and title

# currently this is an undirected shared reference network I think 
# or at least it should be as those are the edges 
#edges <- fullData[c("ID", "cited_by")]
titles <- fullData[c("ID", "title")]
authors <- fullData[c("ID", "author")]
journal <- fullData[c("ID", "source_title")]
year <- fullData[c("ID", "year")]
search <- fullData[c("ID", "search")]
label <- fullData[c("ID", "label")]
color <- fullData[c("ID", "color")]
references <- fullData[c("ID", "references")]

class(edges)
edges <- as.matrix(edges)
class(edges)
edges
net <- igraph::graph_from_edgelist(edges, directed = FALSE)
#net <- igraph::graph_from_data_frame(edges, directed = FALSE)
#plot(net)
net <- set_vertex_attr(net, "title", value = nodes)
#plot(net)
net <- set_vertex_attr(net, "author", value = authors)
net <- set_vertex_attr(net, "journal", value = journal)
net <- set_vertex_attr(net, "year", value = year)
net <- set_vertex_attr(net, "search", value = search)

layout <- layout.fruchterman.reingold(net)
plot(simplify(net),
     layout=layout, vertex.size=2,
     vertex.label=NA, 
     vertex.color= fullData$search,
     edge.arrow.size=.2)
plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
     vertex.label=NA, 
     vertex.color= fullData$search,
     edge.arrow.size=.2)
coords = layout_(net, with_mds())
# coords = layout_(net, with_kk())
# png("../Results/Graphics/InitialPlot.png")
plot1 <- plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
     vertex.label=NA, 
     vertex.color= fullData$search,
     edge.arrow.size=.2, layout = coords)

plot2 <- plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
              vertex.label=NA, 
              vertex.color= fullData$color,
              edge.arrow.size=.2, layout = coords)
# the layout functions for this are utter garbage
# edges1 <- subset(edges, Freq > 1)
# net1 <- igraph::graph_from_data_frame(edges1, directed = FALSE)
# plot(net)
# net1 <- set_vertex_attr(net, "title", value = nodes)
#plot(net)
# net1 <- set_vertex_attr(net, "author", value = authors)
# net1 <- set_vertex_attr(net, "journal", value = journal)
# net1 <- set_vertex_attr(net, "year", value = year)
# net1 <- set_vertex_attr(net, "search", value = search)

# layout <- layout.fruchterman.reingold(net1)
# plot(simplify(net1),
#      layout=layout, vertex.size=2,
#      vertex.label=NA, 
#      vertex.color= fullData$search,
#      edge.arrow.size=.2)

# plot(delete.vertices(simplify(net1), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      vertex.color= fullData$search,
#      edge.arrow.size=.2)
# coords = layout_(net1, with_mds())
# coords = layout_(net, with_kk())
# png("../Results/Graphics/SecondPlot.png")
# simpleNet <- simplify(net1)
# simpleNet <- delete.vertices(simpleNet, degree(simpleNet)==0)

# coords = layout_(simpleNet, with_mds())
# plot2 <- plot(simpleNet, vertex.size=2,
#               vertex.label=NA, 
#               vertex.color= fullData$search,
#               edge.arrow.size=.2, layout = coords)
# net1$search <- as.numeric(net1$search)
comStructure <- modularity(net, fullData$search)
comStructure

################ useful ###############
# what search did these appear in? 
# proportion of the two searches in each module 

cluster <- cluster_louvain(net)
cluster
members <- igraph::membership(cluster)
members

length(unique(members))

memberTable <- table(members)
hist(memberTable, breaks = 500)
sort(memberTable)
########################################






# plot2 <- plot(delete.vertices(simplify(net1), degree(net)==0), vertex.size=2,
#               vertex.label=NA, 
#               vertex.color= fullData$search,
#               edge.arrow.size=.2, layout = coords)

plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
     vertex.label=NA, 
     edge.arrow.size=.2, 
     layout = coords,
     vertex.color=rainbow(3, alpha=0.6)[cluster$membership])
centrality <- degree(net, V(net)) # not sure what this is doing 
centrality

plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
     vertex.label=NA, 
     edge.arrow.size=.2, 
     layout = coords,
     vertex.color=rainbow(3, alpha=0.6)[centrality])


########### 

ceb <- cluster_edge_betweenness(net)
dendPlot(ceb, mode="hclust")

plot(ceb, net)

###########
clp <- cluster_label_prop(net)
plot(clp, net)

cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net))

colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)

# assortativity_nominal(net, V(net)$search, directed=F)
net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))
cliques(net.sym) # list of cliques
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes

kc <- coreness(net, mode="all")
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])

assortativity_nominal(net, fullData$search, directed=F)
assortativity(net, fullData$search, directed = F)
assortativity.degree(net, directed=F)


walkComm <- walktrap.community(net)
plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
     vertex.label=NA, 
     edge.arrow.size=.2, 
     layout = coords,
     vertex.color=rainbow(3, alpha=0.6)[walkComm])
        


# proportion of CT citing ML papers
# how are we going to get proportion of ML refs cited out? 
names(fullData)[names(fullData) == "ID"] <- "CitingID"
# need to make a list of each cited ID by citingID
edgeDf <- as.data.frame(edges)
edgeDf <- edgeDf[order(edgeDf$CitingID),] 
citationListsByNode <- edgeDf %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
class(citationListsByNode)
##
dataPlusEdges <- merge(fullData, citationListsByNode, all = T, by = 'CitingID')
MLFull <- dataPlusEdges %>% dplyr::filter(dataPlusEdges$label == 'ML')
CTFull <- dataPlusEdges %>% dplyr::filter(dataPlusEdges$label == 'CT')

# plot proportion CT citing ML against year 
# gephi plot 
