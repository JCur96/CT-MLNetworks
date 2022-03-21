# edgeList.R
set.seed(1)
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")
# imports 
library(dplyr)
# library(tidyr)
messyData <- read.csv("../Data/edges/matchedResults.csv")

# drop anything with conf greater than -0.55
# most values above this are consistently correct 
# but not all
# and we loose some otherwise correct matches
# but time and general brutality happen here 
colnames(messyData)
messyData <- messyData %>% filter(conf < -0.55)
nrow(messyData)
nrow(unique(messyData))
# removedNa <- messyData %>% tidyr::drop_na()
# nrow(removedNa)
# make edgelist from remaining data
edgeList <- messyData[, c('CitingID', 'CitedID')]
write.csv(edgeList, "../Data/edges/edgeList.csv")
