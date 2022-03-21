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
# MLData$search <- 2
# CTData$search <- 1
MLData$search <- 'ML'
CTData$search <- 'CT'
MLData$label <- "ML"
CTData$label <- "CT"
MLData$color <- "gold"
CTData$color <- "blue"

fullData <- merge(MLData, CTData, all = T) 

names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
# fullData$ID <- 0:nrow(fullData) ?
write.csv(fullData, "../Data/dataForPythonModules.csv")
# fullData$search <- as.numeric(fullData$search)
colnames(edges)
edges <- edges[c("CitingID", "CitedID")]
colnames(edges)

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
net <- igraph::graph_from_edgelist(edges, directed = TRUE)
net <- set_vertex_attr(net, "title", value = nodes)
net <- set_vertex_attr(net, "author", value = authors)
net <- set_vertex_attr(net, "journal", value = journal)
net <- set_vertex_attr(net, "year", value = year)
net <- set_vertex_attr(net, "search", value = search)

# layout <- layout.fruchterman.reingold(net)
# plot(simplify(net),
#      layout=layout, vertex.size=2,
#      vertex.label=NA, 
#      vertex.color= fullData$search,
#      edge.arrow.size=.2)
# plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      vertex.color= fullData$search,
#      edge.arrow.size=.2)
# coords = layout_(net, with_mds())
# plot1 <- plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      vertex.color= fullData$search,
#      edge.arrow.size=.2, layout = coords)
# 
# plot2 <- plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#               vertex.label=NA, 
#               vertex.color= fullData$color,
#               edge.arrow.size=.2, layout = coords)

# comStructure <- modularity(net, fullData$search)
# comStructure
# 
# ################ useful ###############
# # what search did these appear in? 
# # proportion of the two searches in each module 
# 
# cluster <- cluster_louvain(net)
# cluster
# # modularity(cluster)
# # modularity(net, membership(cluster))
# members <- igraph::membership(cluster)
# members 
# modularity(net, members)
# 
# class(members)
# membersDf <- as.data.frame(members)
# write.csv(as.numeric(members), "../Data/membership.csv")
# need to get the csv from the python version 

# bind the dfs 
# communities <- read.csv("../Data/membership.csv")
communities <- read.csv("../Data/pythonCommunityData.csv")
communities
# names(communities)[names(communities) == "X"] <- "ID"
# names(communities)[names(communities) == "x"] <- "Community"
names(communities)[names(communities) == "community"] <- "Community"
communities <- communities[c("ID", "Community")]
fullData <- merge(fullData, communities, all = T, by = 'ID')
head(fullData)
# now can see what search each module contains
# ok maybe drop irrelevant stuff 
partData <- fullData[c("ID", "search", "Community")]
groupedData <- partData %>% dplyr::group_split(Community)
groupedData[[7]]
# great, now code for proportion of each comm of each search?
tab <- groupedData[[7]]
counts <- dplyr::count(tab, search)
totRows <- nrow(tab)
counts
# count return col2 value 1 the col 2 value 2 over nrow for proportion
CTProp <- counts[1,2] / totRows
MLProp <- counts[2,2] / totRows
CTProp + MLProp
CTProportionList <- c()
for (i in 1:length(groupedData)) {
        tab <- groupedData[[i]]
        counts <- dplyr::count(tab, search)
        totRows <- nrow(tab)
        CTProp <- counts[1,2] / totRows
        MLProp <- counts[2,2] / totRows
        # CTProp + MLProp
        CTProportionList <- append(CTProportionList, CTProp)
        
}
CTPropDf <- as.data.frame(unlist(CTProportionList))
names(CTPropDf)[names(CTPropDf) == "unlist(CTProportionList)"] <- "proportionCT"
# CTPropDf$Community <- 0:(nrow(CTPropDf)-1) # or 
CTPropDf$Community <- 1:nrow(CTPropDf) 
CTPropDf$proportionCT
meanCTPropPerComm <- mean(CTPropDf$proportionCT)
meanCTPropPerComm
# partData$proportionCT
partData <- merge(partData, CTPropDf, all=T, by='Community')
head(partData)
MLProportionList <- c()
for (i in 1:length(groupedData)) {
        tab <- groupedData[[i]]
        counts <- dplyr::count(tab, search)
        totRows <- nrow(tab)
        CTProp <- counts[1,2] / totRows
        MLProp <- counts[2,2] / totRows
        if (is.na(MLProp)) {
                MLProp <- 0
        }
        # CTProp + MLProp
        MLProportionList <- append(MLProportionList, MLProp)
        
}
MLProportionList
# MLPropDf <- c()
# for (i in 1:length(MLProportionList)) {
#         comm <- i
#         #comm <- MLProportionList[[i]]
#         value <- MLProportionList[[i]][[1]]
#         row <- c(comm, value)
#         MLPropDf <- append(MLPropDf, row)
# }
# MLPropDf <- as.data.frame(MLPropDf)
# MLProportionList <- as.data.frame(MLProportionList)
# MLProportionList <- MLProportionList %>% gather(year, value, -c(Code, Country))
# MLProportionList
# partData <- merge(partData, MLProportionList, by='Community')
# MLPropDf <- as.data.frame(MLPropDf) # ?????????
MLPropDf <- as.data.frame(unlist(MLProportionList))
MLPropDf
colnames(MLPropDf)
names(MLPropDf)[names(MLPropDf) == "unlist(MLProportionList)"] <- "proportionML"
MLPropDf$Community <- 1:nrow(MLPropDf)
MLPropDf$Community
partData <- merge(partData, MLPropDf, all=T, by='Community')
write.csv(partData, "../Data/community.csv")
################

# is modularity different to random?
# erdos.renyi.game(
#         6372, # number of verticies
#         m = 37584, # number of edges (m) 
#         type = "gnm", # gnm as using 'm' for edges
#         directed = FALSE,
#         loops = FALSE
# )
# 
# graph <- sample_gnm(
#         6372,
#         37584,
#         directed = FALSE,
#         loops = FALSE
# )
# 
# graphCluster <- cluster_louvain(graph)
# graphMembers <- igraph::membership(graphCluster)
# modularity(graph, graphMembers)
# graph
# graph <- rewire(net, with = keeping_degseq(niter = vcount(net) * 10))
# graph
# 
# write_graph(
#         graph,
#         "../Data/GraphObj.txt",
#         format = c("edgelist"))
# 
# graphCluster <- edge.betweenness.community(graph, weights = NULL)
# graphCluster
# modularityList <- c()
# for (i in 1:10) { # 10000
#         # graph <- sample_gnm(6372, 37584, directed = FALSE, loops = FALSE) # where is degree in there? 
#         # needs to maintain both in- and out-degree
#         # igraph_rewire() ???
#         # degree.distribution()
#         # graph should be directed!!!
#         graph <- rewire(net, with = keeping_degseq(niter = vcount(net) * 10))
#         # graphCluster <- cluster_louvain(graph)
#         # graphCluster <- edge.betweenness(graph)
#         graphCluster <- edge.betweenness.community(graph)
#         graphMembers <- igraph::membership(graphCluster)
#         graphModularity <- modularity(graph, graphMembers)
#         modularityList <- append(modularityList, graphModularity)
# }
# modularityList
# hist(modularityList)
# 
# g <- make_ring(10)
# g %>%
#         rewire(keeping_degseq(niter = 20)) #%>%
#         #degree()
# print_all(rewire(g, with = keeping_degseq(niter = vcount(g) * 10)))
# plot(g)
# visual inspection shows that modularity of clustered members 
# is outside of 5% tails of hist of random graphs
# (0.53 compared to 0.23 - 0.25 for 10000 graphs)
###
# length(unique(members))
# 
# memberTable <- table(members)
# hist(memberTable, breaks = 500)
# sort(memberTable)
########################################






# plot2 <- plot(delete.vertices(simplify(net1), degree(net)==0), vertex.size=2,
#               vertex.label=NA, 
#               vertex.color= fullData$search,
#               edge.arrow.size=.2, layout = coords)

# plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      edge.arrow.size=.2, 
#      layout = coords,
#      vertex.color=rainbow(3, alpha=0.6)[cluster$membership])
# centrality <- degree(net, V(net)) # not sure what this is doing 
# centrality
# 
# plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      edge.arrow.size=.2, 
#      layout = coords,
#      vertex.color=rainbow(3, alpha=0.6)[centrality])


########### 
# 
# ceb <- cluster_edge_betweenness(net)
# dendPlot(ceb, mode="hclust")
# 
# plot(ceb, net)
# 
# ###########
# clp <- cluster_label_prop(net)
# plot(clp, net)
# 
# cfg <- cluster_fast_greedy(as.undirected(net))
# plot(cfg, as.undirected(net))
# 
# colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
# 
# # assortativity_nominal(net, V(net)$search, directed=F)
# net.sym <- as.undirected(net, mode= "collapse",
#                          edge.attr.comb=list(weight="sum", "ignore"))
# cliques(net.sym) # list of cliques
# sapply(cliques(net.sym), length) # clique sizes
# largest_cliques(net.sym) # cliques with max number of nodes
# 
# kc <- coreness(net, mode="all")
# plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])
# 
# assortativity_nominal(net, fullData$search, directed=F)
# assortativity(net, fullData$search, directed = F)
# assortativity.degree(net, directed=F)
# 
# 
# walkComm <- walktrap.community(net)
# plot(delete.vertices(simplify(net), degree(net)==0), vertex.size=2,
#      vertex.label=NA, 
#      edge.arrow.size=.2, 
#      layout = coords,
#      vertex.color=rainbow(3, alpha=0.6)[walkComm])
        


# proportion of CT citing ML papers
# how are we going to get proportion of ML refs cited out? 
names(fullData)[names(fullData) == "ID"] <- "CitingID"
# need to make a list of each cited ID by citingID
edgeDf <- as.data.frame(edges)
edgeDf <- edgeDf[order(edgeDf$CitingID),] 
citationListsByNode <- edgeDf %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID)) # where cited ID is an ML Citing ID?
class(citationListsByNode)
head(citationListsByNode)

## now do actualy proportions 
head(edgeDf)
propsDf <- edgeDf
searchDf <- fullData[c("CitingID", "search",  "year")]
head(searchDf) # merge( by.x, by.y)
names(searchDf)[names(searchDf) == "CitingID"] <- "CitedID"
names(searchDf)[names(searchDf) == "search"] <- "citedSearch"
propsDf <- merge(propsDf, searchDf, all = T, by = 'CitedID')
propsDf <- propsDf[order(propsDf$CitingID),]
propsDf <- tidyr::drop_na(propsDf)
head(propsDf)
yearDf <- propsDf[c("year", "citedSearch")]
propsDf <- propsDf[!(is.na(propsDf$year) | propsDf$year==" "), ]
propsDf
propsDf <- propsDf[order(propsDf$year),]
propsDf
colnames(propsDf)
# propsDf <- propsDf %>% dplyr::filter(citedSearch == 2)
propsDf <- propsDf %>% dplyr::filter(citedSearch == 'ML')
head(propsDf) # this is all the papers which cite ML papers
# now need to filter to those only in the CT seach
# tmpCTData <- fullData %>% dplyr::filter(search == 1)
tmpCTData <- fullData %>% dplyr::filter(search == 'CT')
CtIDList <- tmpCTData$CitingID
# length(CtIDList)
head(CtIDList)
propsDf <- propsDf %>% dplyr::filter(CitingID %in% CtIDList)
head(propsDf)
propsList <- propsDf %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
# compare length of list of CT IDs to the nrow of this new DF to get proportion
# propsList <- tidyr::drop_na(propsList)
nrow(propsList)
length(CtIDList)
propCTPapersCitingML <- nrow(propsList) / length(CtIDList)
propCTPapersCitingML

##
dataPlusEdges <- merge(fullData, citationListsByNode, all = T, by = 'CitingID')
MLFull <- dataPlusEdges %>% dplyr::filter(dataPlusEdges$label == 'ML')
CTFull <- dataPlusEdges %>% dplyr::filter(dataPlusEdges$label == 'CT')
colnames(dataPlusEdges)

giantConnectedData <- dataPlusEdges[!is.na(dataPlusEdges$CitedID),]

dataPlusEdges <- apply(dataPlusEdges,2,as.character)
write.csv(dataPlusEdges, "../Data/FullData.csv")


# plot proportion CT citing ML against year
propsDf <- edgeDf
searchDf <- fullData[c("CitingID", "search", "year")]
names(searchDf)[names(searchDf) == "CitingID"] <- "CitedID"
names(searchDf)[names(searchDf) == "search"] <- "citedSearch"
propsDf <- merge(propsDf, searchDf, all = T, by = 'CitedID')
# propsDf$CitingID
propsDf <- propsDf[order(propsDf$CitingID),]
propsDf <- tidyr::drop_na(propsDf)
propsDf <- propsDf %>% dplyr::filter(citedSearch == 2)
yearData <- searchDf %>% dplyr::group_split(year)
#length(yearData)
tmpCTData <- searchDf %>% dplyr::filter(citedSearch == 1) # search 1 is CT
# CtIDList <- tmpCTData$CitingID
# yearData[[1]][["year"]][[1]]
propVector <- c()
outerList <- c()
nRows <- c()
lengths <- c()
for (i in 1:length(yearData)) {
        #print(yearData[[i]]['year'])
        # tmpsearchDf <- yearData[[i]]
        tmpPropsDf <- propsDf
        # print(head(tmpPropsDf$CitingID))
        # tmpPropsDf <- merge(propsDf, tmpsearchDf, all = T, by = 'CitedID')
        # tmpPropsDf <- tmpPropsDf[order(tmpPropsDf$CitingID),]
        # tmpPropsDf <- tidyr::drop_na(tmpPropsDf)
        # tmpPropsDf <- tmpPropsDf %>% dplyr::filter(citedSearch == 2)
        currentYear <- yearData[[i]][["year"]][[1]]
        yearPropsDf <- tmpPropsDf %>% dplyr::filter(year == currentYear)
        #print(head(yearPropsDf$year))
        # print(class(yearPropsDf))

        yearCTList <- tmpCTData %>% dplyr::filter(year == currentYear)
        #print(head(tmpCTData$CitedID))
        #print(yearCTList)
        #print(yearCTList$CitingID)
        CtIDList <- yearCTList$CitedID # might need to go back to CitingID
        # print(head(tmpPropsDf))
        yearPropsDf <- yearPropsDf %>% dplyr::filter(CitingID %in% CtIDList)
        # print(class(yearPropsDf))
        #print(head(tmpPropsDf))
        tmpPropsList <- yearPropsDf %>% 
                dplyr::group_by(CitingID) %>% 
                dplyr::summarise(CitedID = list(CitedID))
        row <- nrow(tmpPropsList)
        #print(tmpPropsList)
        nRows <- append(nRows, nrow(tmpPropsList))
        lengths <- append(lengths, length(CtIDList))
        # print(length(CtIDList))
        proportion <- nrow(tmpPropsList) / length(CtIDList)
        #print(proportion*100)
        innerList <- list(currentYear, proportion)
        outerList <- append(outerList, list(innerList))
        propVector <- append(propVector, proportion)
        
}
sum(nRows) # should be 2209
sum(lengths) # should be 4640
## it isnt working and right now I can't work out why, will try again tomorrow
# OK i think I'm jsut too stupid to do it this way, gonna do it the idiots ways

# citing vs cited year???
# need to do math against full data number of CT papers
year2012 <- yearData[[1]]
year2013 <- yearData[[2]]
year2014 <- yearData[[3]]
year2015 <- yearData[[4]]
year2016 <- yearData[[5]]
year2017 <- yearData[[6]]
year2018 <- yearData[[7]]
year2019 <- yearData[[8]]
year2020 <- yearData[[9]]
year2021 <- yearData[[10]]
year2022 <- yearData[[11]]

# CitingID citingSearch CitedID citedSearch

# CitingID citingSearch CitingYear CitedID citedSearch citedYear



propsDf <- edgeDf
searchDf <- fullData[c("CitingID", "search",  "year")]
searchDf
names(searchDf)[names(searchDf) == "CitingID"] <- "CitedID"
names(searchDf)[names(searchDf) == "search"] <- "citedSearch"
propsDf <- merge(propsDf, searchDf, all = T, by = 'CitedID')
propsDf <- propsDf[order(propsDf$CitingID),]
propsDf <- tidyr::drop_na(propsDf)
propsDf <- propsDf[!(is.na(propsDf$year) | propsDf$year==" "), ]
propsDf
propsDf <- propsDf[order(propsDf$year),]
propsDf
colnames(propsDf)
propsDf <- propsDf %>% dplyr::filter(citedSearch == 2)
head(propsDf) # this is all the papers which cite ML papers
# now need to filter to those only in the CT seach
yearDataList <- propsDf %>% dplyr::group_split(year)
year2012 <- yearDataList[[1]]
year2013 <- yearDataList[[2]]
year2014 <- yearDataList[[3]]
year2015 <- yearDataList[[4]]
year2016 <- yearDataList[[5]]
year2017 <- yearDataList[[6]]
year2018 <- yearDataList[[7]]
year2019 <- yearDataList[[8]]
year2020 <- yearDataList[[9]]
year2021 <- yearDataList[[10]]
year2022 <- yearDataList[[11]]

tmpCTData <- fullData %>% dplyr::filter(search == 1)
tmpCTData <- tmpCTData[c("CitingID", "year")]
yearCTData <- tmpCTData %>% dplyr::group_split(year)
ctYear2012 <- yearCTData[[1]]
ctYear2013 <- yearCTData[[2]]
ctYear2014 <- yearCTData[[3]]
ctYear2015 <- yearCTData[[4]]
ctYear2016 <- yearCTData[[5]]
ctYear2017 <- yearCTData[[6]]
ctYear2018 <- yearCTData[[7]]
ctYear2019 <- yearCTData[[8]]
ctYear2020 <- yearCTData[[9]]
ctYear2021 <- yearCTData[[10]]
ctYear2022 <- yearCTData[[11]]


CtIDList2012 <- ctYear2012$CitingID
CtIDList2013 <- ctYear2013$CitingID
CtIDList2014 <- ctYear2014$CitingID
CtIDList2015 <- ctYear2015$CitingID
CtIDList2016 <- ctYear2016$CitingID
CtIDList2017 <- ctYear2017$CitingID
CtIDList2018 <- ctYear2018$CitingID
CtIDList2019 <- ctYear2019$CitingID
CtIDList2020 <- ctYear2020$CitingID
CtIDList2021 <- ctYear2021$CitingID
CtIDList2022 <- ctYear2022$CitingID

length(CtIDList2012) +
length(CtIDList2013) +
length(CtIDList2014) +
length(CtIDList2015) +
length(CtIDList2016) +
length(CtIDList2017) +
length(CtIDList2018) +
length(CtIDList2019) +
length(CtIDList2020) +
length(CtIDList2021) +
length(CtIDList2022) # thats the 4640 one then 


# year2012 <- fullData %>% dplyr::filter(year == 2012)
# year2012 <- year2012[c("CitingID", "search", "year")]
# year2012 <- year2012 %>% dplyr::filter(search == 1)
# length(CtIDList2012)
year2012
CtIDList2012

print(year2012$CitingID %in% CtIDList2012)
year2012 <- year2012 %>% dplyr::filter(CitingID %in% CtIDList2012)
head(year2012)
nrow(year2012)
year2012List <- year2012 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2012 <- nrow(year2012List) / length(CtIDList2012)
row2012 <- nrow(year2012List)

year2013 <- year2013 %>% dplyr::filter(CitingID %in% CtIDList2013)
year2013List <- year2013 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2013 <- nrow(year2013List) / length(CtIDList2013)
row2013 <- nrow(year2013List)

year2014 <- year2014 %>% dplyr::filter(CitingID %in% CtIDList2014)
year2014List <- year2014 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2014 <- nrow(year2014List) / length(CtIDList2014)
row2014 <- nrow(year2014List)

year2015 <- year2015 %>% dplyr::filter(CitingID %in% CtIDList2015)
year2015List <- year2015 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2015 <- nrow(year2015List) / length(CtIDList2015)
row2015 <- nrow(year2015List)

year2016 <- year2016 %>% dplyr::filter(CitingID %in% CtIDList2016)
year2016List <- year2016 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2016 <- nrow(year2016List) / length(CtIDList2016)
row2016 <- nrow(year2016List)

year2017 <- year2017 %>% dplyr::filter(CitingID %in% CtIDList2017)
year2017List <- year2017 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2017 <- nrow(year2017List) / length(CtIDList2017)
row2017 <- nrow(year2017List)

year2018 <- year2018 %>% dplyr::filter(CitingID %in% CtIDList2018)
year2018List <- year2018 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2018 <- nrow(year2018List) / length(CtIDList2018)
row2018 <- nrow(year2018List)

year2019 <- year2019 %>% dplyr::filter(CitingID %in% CtIDList2019)
year2019List <- year2019 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2019 <- nrow(year2019List) / length(CtIDList2019)
row2019 <- nrow(year2019List)

year2020 <- year2020 %>% dplyr::filter(CitingID %in% CtIDList2020)
year2020List <- year2020 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2020 <- nrow(year2020List) / length(CtIDList2020)
row2020 <- nrow(year2020List)

year2021 <- year2021 %>% dplyr::filter(CitingID %in% CtIDList2021)
year2021List <- year2021 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2021 <- nrow(year2021List) / length(CtIDList2021)
row2021 <- nrow(year2021List)

year2022 <- year2022 %>% dplyr::filter(CitingID %in% CtIDList2022)
year2022List <- year2022 %>% 
        dplyr::group_by(CitingID) %>% 
        dplyr::summarise(CitedID = list(CitedID))
prop2022 <- nrow(year2022List) / length(CtIDList2022)
row2022 <- nrow(year2022List)

rows <- list(row2012, row2013, row2014, row2015, row2016, row2017, row2018, row2019, row2020, row2021, row2022)
props <- list(prop2012, prop2013, prop2014, prop2015, prop2016, prop2017, prop2018, prop2019, prop2020, prop2021, prop2022)
Reduce("+", props)
Reduce("+", rows)
years <- list(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

# I cannot see what is wrong with this so must assume this and the for loop have actually worked and somehow my logic 
# (which is the same for the full data set?) is incorrect
p <- plot(years, props)

# gephi plot 
giantConnectedData <- dataPlusEdges[!is.na(dataPlusEdges$CitedID),]
giantConnectedData <- apply(giantConnectedData,2,as.character)
write.csv(giantConnectedData, "../Data/GiantConnected.csv")
# read the above + plus the edgelist in to get only connected nodes 
