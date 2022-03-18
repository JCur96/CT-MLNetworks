# dataGeneration.R

set.seed(1)
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")
# imports
library(igraph)
library(RColorBrewer)

edges <- read.csv("../Data/edges/edgeList.csv")

# read data in
MLData <- read.csv("../Data/ML/autoScreenedML.csv")
CTData <- read.csv("../Data/CT/autoScreenedCT.csv")
MLData$search <- "ML"
CTData$search <- "CT"
MLData$label <- "ML"
CTData$label <- "CT"
MLData$color <- "gold"
CTData$color <- "blue"

fullData <- merge(MLData, CTData, all = T) 

names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
#fullData$search <- as.numeric(fullData$search)
colnames(edges)
edges <- edges[c("CitingID", "CitedID")]
names(edges)[names(edges) == "CitingID"] <- "citingID"
names(edges)[names(edges) == "CitedID"] <- "citedID"
colnames(edges)


# propsDf <- edgeDf
searchDf <- fullData[c("ID", "search",  "year")]
names(searchDf)[names(searchDf) == "ID"] <- "citedID"
names(searchDf)[names(searchDf) == "year"] <- "citedYear"
names(searchDf)[names(searchDf) == "search"] <- "citedSearch"
edges <- merge(edges, searchDf, all = T, by = 'citedID')

head(edges)

citingDf <- fullData[c("ID", "search",  "year")]
names(citingDf)[names(citingDf) == "ID"] <- "citingID"
names(citingDf)[names(citingDf) == "year"] <- "citingYear"
names(citingDf)[names(citingDf) == "search"] <- "citingSearch"
edges <- merge(edges, citingDf, all = T, by = 'citingID')
head(edges)


read.csv("..Data/proportionsData.csv")

edges <- edges[, c(1, 5, 6, 2, 3, 4)]
head(edges)
write.csv(edges, "../Data/proportionsData.csv")

edgesMinusYear <- edges[c("citingID", "citingSearch", "citedID", "citedSearch")]
head(edgesMinusYear)
write.csv(edgesMinusYear, "../Data/proportionsDataMinusYear.csv")
# CitingID citingSearch CitedID citedSearch

# read.csv("..Data/proportionsData.csv")

# edges <- edges[, c(1, 5, 6, 2, 3, 4)]
# head(edges)
# edgesMinusYear <- edges[c("citingID", "citingSearch", "citedID", "citedSearch")]
# head(edgesMinusYear)

# CitingID citingSearch CitingYear CitedID citedSearch citedYear

