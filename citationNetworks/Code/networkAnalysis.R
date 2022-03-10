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
MLData$search <- 1
CTData$search <- 0
MLData$label <- "ML"
CTData$label <- "CT"
MLData$color <- "gold"
CTData$color <- "blue"

fullData <- merge(MLData, CTData, all = T) 

names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
fullData$search <- as.numeric(fullData$search)

colnames(edges)
edges <- edges[c("CitingID", "CitedID")]
colnames(edges)
edges <- as.data.frame(table(edges))
edges <- subset(edges, Freq > 0)
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



net <- igraph::graph_from_data_frame(edges, directed = FALSE)
plot(net)
net <- set_vertex_attr(net, "title", value = nodes)
plot(net)
net <- set_vertex_attr(net, "author", value = authors)
net <- set_vertex_attr(net, "journal", value = journal)
net <- set_vertex_attr(net, "year", value = year)
net <- set_vertex_attr(net, "search", value = search)

layout <- layout.fruchterman.reingold(net1)
plot(net,
     layout=layout)
