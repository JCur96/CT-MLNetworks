## refLists.R
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
fullData <- merge(MLData, CTData, all = T) 
head(fullData$references, 4)
names(fullData)[names(fullData) == "X"] <- "ID"
colnames(fullData)
fullData$ID <- 1:nrow(fullData)
refs <- fullData[c("ID", "references")]


refs1 <- refs$references[27]
grep("(?<![^;]).(?![^;])",refs1, value = FALSE, perl=TRUE)
# grep("(?:^|,)XXX(?:$|,)",df$nms, value = FALSE)

someRef <- grep("^(.+?);", refs1, value = TRUE, perl=TRUE)
someRef
grep("^(.+?);", refs1, value = TRUE, perl=TRUE)
#yourString.split(";")[0]

refList<- strsplit(refs1, ";") ## this one works! need to do that in a loop and attach the ID to each
# also need to get the titles out??
#strsplit(refs1, ".,")
refList[[1]]
firstRef <- refList[[1]]
firstRef
strsplit("UK, USA, Germany", ",(?=[^,]+$)", perl=TRUE)


ref <- '[52] " Zub, K., Borowski, Z., SzafraÅ„ska, P.A., Wieczorek, M., Konarzewski, M., Lower body mass and higher metabolic rate enhance winter survival in root voles, Microtus oeconomus (2014) Biol. J. Linn. Soc., 113, pp. 297-309"'  
strsplit(ref, "(\\.,)(?=[^,]+$)", perl=TRUE)
strsplit(ref,"(\\.,)(?!.*\1)", perl=TRUE) # ok this splits it up quite well? 
# split1 <- strsplit(ref, "(\.,)(?=[^,]+$)", perl=TRUE)
# split1
# class(split1)
# newSplit <- "[52] \" Zub, K., Borowski, Z., SzafraÅ„ska, P.A., Wieczorek, M., Konarzewski, M., Lower body mass and higher metabolic rate enhance winter survival in root voles, Microtus oeconomus (2014) Biol. J. Linn. Soc., 11"
# strsplit(newSplit, "(.,)(?=[^,]+$)", perl=TRUE)
# thirdSplit <- "[52] \" Zub, K., Borowski, Z., SzafraÅ„ska, P.A., Wieczorek, M., Konarzewski, M., Lower body mass and higher metabolic rate enhance winter survival in root voles, Microtus oeconomus (2014) Biol. J. Linn. Soc"
# strsplit(thirdSplit, "(.,)(?=[^,]+$)", perl=TRUE)
# ok might have to split at every occurence of ; 
# put all of those into their own cols? IDK this is looking less and less possible 
# next match the (DATENUMERIC) and pull everything before that ?
newstr <- " Lower body mass and higher metabolic rate enhance winter survival in root voles, Microtus oeconomus (2014) Biol. J. Linn. Soc"
strsplit(newstr, "(\\([0-9]*\\))(?!.*\1)", perl=TRUE) # ok great, can get the titles and with some manual work the everything else
# monday task to automate grabbing the titles and having them associated with original article ID
# I guess also give them their own unique ID? Like maybe paperID_OwnID
# IDK maybe quicker to got to scopus and manually do it all

# 
x[[1]][1] # list one element 1

refList[[1]][1]
newSplit <- strsplit(refList[[1]][1],"(\\.,)(?!.*\1)", perl=TRUE)
newSplit
finalSplit <- strsplit(newSplit[[1]][2], "(\\([0-9]*\\))(?!.*\1)", perl=TRUE)
finalSplit
finalSplit[[1]][1] # ok just need to remember to take the first list element 

refDf <- refs[10:11,]

for (i in 1:nrow(refDf)) {
  ID <- refDf$ID[i] # pull out ID as needed later
  singleIDRefList <- strsplit(refDf$references[i], ";") # pull out all the refs
  # ok might need to append to a list of lists 
  #print(singleIDRefList[[1]][1])
  #print(singleIDRefList)
  #print(nrow(refDf))
  for (j in 1:length(singleIDRefList[[1]])) { # iterator of length outer list 
    # can use [[1]] as the single ID list is always a list of 1
    singleRef <- singleIDRefList[[1]][j]
    #print(singleRef)
    splitSingleRef <- strsplit(singleRef[[1]][j],"(\\.,)(?!.*\1)", perl=TRUE)
    
    # loopSize <- length(splitSingleRef[[1]])
    # for (k in 1:loopSize) { # inner list iterator
    #   #print(length(splitSingleRef[[1]]))
    #   print(splitSingleRef[[1]][k])
    #   #atomisedRef <- strsplit(singleIDRefList[[j]][k],"(\\.,)(?!.*\1)", perl=TRUE)
    #   #print(atomisedRef)
    # }
  }
  
}

# this works for getting the whole thing out in one with the ID of the paper it came from 
IDLinkedRefDf <- data.frame(ID=as.numeric(), Reference=as.character())
for (i in 1:nrow(refDf)) {
  ID <- refDf$ID[i] # pull out ID as needed later
  singleIDRefList <- strsplit(refDf$references[i], ";") # pull out all the refs
  for (j in 1:length(singleIDRefList[[1]])) { # iterator of length outer list 1
    singleRef <- singleIDRefList[[1]][j]
    tmpDf <- data.frame(ID=as.numeric(ID), Reference=as.character(singleRef))
    IDLinkedRefDf <- rbind(IDLinkedRefDf, tmpDf)
    #rbind(IDLinkedRefDf$ID, ID)
    #rbind(IDLinkedRefDf$Reference, singleRef)
    
  }
}
# remove any empty rows 
# refs[!apply(refs == "", 1, all),]
refs <- refs[!(is.na(refs$references) | refs$references==""), ]
IDLinkedRefDf <- data.frame(ID=as.numeric(), Reference=as.character())
for (i in 1:nrow(refs)) {
  ID <- refs$ID[i] # pull out ID as needed later
  singleIDRefList <- strsplit(refs$references[i], ";") # pull out all the refs
  for (j in 1:length(singleIDRefList[[1]])) { # iterator of length outer list 1
    singleRef <- singleIDRefList[[1]][j]
    tmpDf <- data.frame(ID=as.numeric(ID), Reference=as.character(singleRef))
    IDLinkedRefDf <- rbind(IDLinkedRefDf, tmpDf)
    #rbind(IDLinkedRefDf$ID, ID)
    #rbind(IDLinkedRefDf$Reference, singleRef)
    
  }
}

head(IDLinkedRefDf)
