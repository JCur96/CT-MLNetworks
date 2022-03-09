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
refs <- fullData[c("ID", "references", "title")]

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
write.csv(IDLinkedRefDf, "../Data/expandedRefsWithOriginPaperID.csv")

# reset workspace as it was getting very slows
# might be worth trying to pull out just titles still? 
IDLinkedRefDf <- read.csv("../Data/expandedRefsWithOriginPaperID.csv")
partData <- IDLinkedRefDf[c(1:5, 80:85),]
partData[1,]
# the below generally works, might have to accept that there will be some 
# bad data as differing formats cause issues
for (i in 1:nrow(partData)) {
  splitRefList <- strsplit(partData$Reference[i],"(\\.,)(?!.*\1)", perl=TRUE)
  #print(splitRefList)
  for (sublistIter in 1:length(splitRefList[[1]])) {
    #print(sublistIter)
    if (grepl("(\\([0-9]*\\))(?!.*\1)" ,splitRefList[[1]][sublistIter], perl=TRUE)) {
      # print(splitRefList[[1]][sublistIter]) # it works, I am lord of all regex
      titlePlusDate <- splitRefList[[1]][sublistIter]
      # justTitle <- strsplit(titlePlusDate, "(\\([0-9]*\\))(?!.*\1)", perl=TRUE)
      # print(justTitle) # not sure if this will really cut it, too variable.
      # additionally, might get away with the added extra bits in a check_recall?
      partData$title[i] <- titlePlusDate
      #print(titlePlusDate)
    }
  }
}
# now need to make the edge list i.e. match up the papers found in the reference
# list tp those that are of interest (i.e. all papers from the full data)
# do it small first 
library(tm) # to remove numbers i.e. the parts which cause the issue for splitting
x <- removeNumbers(x)

partData <- IDLinkedRefDf[c(1:100000),]
partData <- partData[!(is.na(partData$Reference) | partData$Reference==""), ]
for (i in 1:nrow(partData)) {
  splitRefList <- strsplit(partData$Reference[i],"(\\.,)(?!.*\1)", perl=TRUE)
  #print(splitRefList)
  for (sublistIter in 1:length(splitRefList[[1]])) {
    #print(sublistIter)
    #print(length(splitRefList[[1]]))
    if (length(splitRefList[[1]]) > 0) {
      if (grepl("(\\([0-9]*\\))(?!.*\1)" ,splitRefList[[1]][sublistIter], perl=TRUE) == T) {
        # print(splitRefList[[1]][sublistIter]) # it works, I am lord of all regex
        titlePlusDate <- splitRefList[[1]][sublistIter]
        # justTitle <- strsplit(titlePlusDate, "(\\([0-9]*\\))(?!.*\1)", perl=TRUE)
        # print(justTitle) # not sure if this will really cut it, too variable.
        # additionally, might get away with the added extra bits in a check_recall?
        partData$title[i] <- titlePlusDate
        #print(titlePlusDate)
      }
    } 
    if (length(splitRefList[[1]]) == 0) {
      #print(splitRefList[i])
      print(partData$Reference[i])
    }
  }
}
hits <- litsearchr::check_recall(fullData$title, partData$title)
#head(hits)
#class(hits)
hitsDf <- as.data.frame(hits)
#head(hitsDf)
hitsDf <- hitsDf[order(hitsDf$Similarity, decreasing = TRUE), ]
head(hitsDf) 
# # ok so super slow, and not getting above 0.3 with the 500, but thats likely chance
# # might just have to bite the bullet?
# for (i in 1:nrow(IDLinkedRefDf)) {
#   splitRefList <- strsplit(IDLinkedRefDf$Reference[i],"(\\.,)(?!.*\1)", perl=TRUE)
#   #print(splitRefList)
#   for (sublistIter in 1:length(splitRefList[[1]])) {
#     #print(sublistIter)
#     if (grepl("(\\([0-9]*\\))(?!.*\1)" ,splitRefList[[1]][sublistIter], perl=TRUE)) {
#       # print(splitRefList[[1]][sublistIter]) # it works, I am lord of all regex
#       titlePlusDate <- splitRefList[[1]][sublistIter]
#       # justTitle <- strsplit(titlePlusDate, "(\\([0-9]*\\))(?!.*\1)", perl=TRUE)
#       # print(justTitle) # not sure if this will really cut it, too variable.
#       # additionally, might get away with the added extra bits in a check_recall?
#       IDLinkedRefDf$title[i] <- titlePlusDate
#       #print(titlePlusDate)
#     } else {
#       IDLinkedRefDf$title[i] <- NA
#     }
#   }
# }
# hits <- litsearchr::check_recall(fullData$title, IDLinkedRefDf$title)
# hitsDf <- as.data.frame(hits)
# hitsDf <- hitsDf[order(hitsDf$Similarity, decreasing = TRUE), ]
# head(hitsDf) 
# write.csv(hitsDf, "../Data/First10000Refs.csv")
######################################################

############## Actual work from here on ##############
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
refs <- fullData[c("ID", "references", "title")]

refs <- refs[!(is.na(refs$references) | refs$references==""), ]
IDLinkedRefDf <- data.frame(ID=as.numeric(), Reference=as.character(), Title=as.character(), ref_title=as.character(NA))
for (i in 1:nrow(refs)) {
  ID <- refs$ID[i] # pull out ID as needed later
  Title <- refs$title[i]
  singleIDRefList <- strsplit(refs$references[i], ";") # pull out all the refs
  for (j in 1:length(singleIDRefList[[1]])) { # iterator of length outer list 1
    singleRef <- singleIDRefList[[1]][j]
    tmpDf <- data.frame(ID=as.numeric(ID), Reference=as.character(singleRef), Title=as.character(Title))
    IDLinkedRefDf <- rbind(IDLinkedRefDf, tmpDf)
    #rbind(IDLinkedRefDf$ID, ID)
    #rbind(IDLinkedRefDf$Reference, singleRef)
    
  }
}

write.csv(IDLinkedRefDf, "../Data/expandedRefsWithOriginPaperID.csv")


IDLinkedRefDf <- read.csv("../Data/expandedRefsWithOriginPaperID.csv")
IDLinkedRefDf$ref_title <- NA
refSplitter <- function(df) {
  df <- df[!(is.na(df$Reference) | df$Reference==""), ]
  for (i in 1:nrow(df)) {
    splitRefList <- strsplit(df$Reference[i],"(\\.,)(?!.*\1)", perl=TRUE)
    #print(splitRefList)
    for (sublistIter in 1:length(splitRefList[[1]])) {
      #print(sublistIter)
      #print(length(splitRefList[[1]]))
      if (length(splitRefList[[1]]) > 0) {
        if (grepl("(\\([0-9]*\\))(?!.*\1)" ,splitRefList[[1]][sublistIter], perl=TRUE) == T) {
          # print(splitRefList[[1]][sublistIter]) # it works, I am lord of all regex
          titlePlusDate <- splitRefList[[1]][sublistIter]
          # justTitle <- strsplit(titlePlusDate, "(\\([0-9]*\\))(?!.*\1)", perl=TRUE)
          # print(justTitle) # not sure if this will really cut it, too variable.
          # additionally, might get away with the added extra bits in a check_recall?
          df$ref_title[i] <- titlePlusDate
          #print(titlePlusDate)
        }
      } 
      if (length(splitRefList[[1]]) == 0) {
        #print(splitRefList[i])
        print(df$Reference[i])
      }
    }
  }
  return(df)
}

IDLinkedRefDf <- refSplitter(IDLinkedRefDf)

write.csv(IDLinkedRefDf, "../Data/SplitRefs.csv")
IDLinkedRefDf <-  read.csv("../Data/SplitRefs.csv")
# ok now matching
# I think
# if fuzzy match is above 0.5, assign the ID of the title 
# to the reference in new col like RefID (NA for all those below 0.5)
# once that is done, will have ID column which is from the citing paper
# and RefID which is the ID of the cited paper

# matchTitles <- function (articleTitles, refTitles, ID) {
#   matches <- lapply(tm::removePunctuation(tolower(articleTitles)), synthesisr::fuzzdist, b=tm::removePunctuation(tolower(refTitles)))
#   similarity_table <- cbind(articleTitles, refTitles[unlist(lapply(matches, which.min))], 1-unlist(lapply(matches, min, na.rm=TRUE)), ID)
#   colnames(similarity_table) <- c("ArticleTitle", "BestMatch", "Similarity", "Citing_ID")
#   return(similarity_table)
# }

# no idea why it doesnt like this one? # matchTable <- data.frame(Citing_ID=as.numeric(), Cited_ID=as.numeric(), Citing_Title=as.character(), Cited_Title=as.character(), BestMatch=as.character, Similarity=as.numeric())# not sure if that will work
matchedDf <- data.frame(CitingID=as.numeric(), CitedID=as.numeric(), CitingTitle=as.character(), CitedTitle=as.character(), BestMatch=as.character(), Similarity=as.numeric())
matchTitles <- function(refDf, fullDataDf, outputDf) {
  for (i in 1:nrow(fullDataDf)) {
    articleTitle <- fullDataDf$title[i]
    for (j in 1:nrow(refDf)) {
      refTitle <- refDf$ref_title[j]
      CitingID <- refDf$ID[j]
      match <- synthesisr::fuzzdist(a=tm::removePunctuation(tolower(articleTitle)), b=tm::removePunctuation(tolower(refTitle)))
      bestMatch <- refTitle[unlist(lapply(match, which.min))] # don't think this is working
      #print(match)
      match <- 1-unlist(lapply(match, min, na.rm=TRUE))
      if (!is.na(match)) {
        if (match <= 0.5) { # should actually be match <= 0.5 
          #bestMatch <- refTitle[unlist(lapply(match, which.min))]
          #matchTable <- rbind(matchTable, match)
          #print(class(match))
          #print(bestMatch)
          # x <- 1-unlist(lapply(match, min, na.rm=TRUE))
          #x <- 1-unlist(lapply(match, max, na.rm=TRUE))
          #print(x)
          #print(match)
          #print(class(bestMatch))
          CitedID <- fullDataDf$ID
          # itemList <- data.frame(CitingID, CitedID, articleTitle, refTitle, bestMatch, match)
          itemRow <- data.frame(CitingID, CitedID, articleTitle, refTitle, bestMatch, match)
          # itemRow <- as.data.frame(itemList)
          colnames(itemRow) <- c("CitingID", "CitedID", "CitingTitle", "CitedTitle", "BestMatch", "Similarity")
          #print(colnames(itemRow))
          outputDf <- rbind(outputDf, itemRow)
          #print(outputDf[i])
        }
      }
    }
  }
  return(outputDf)
}

partData <- IDLinkedRefDf[1:1000,]
matchedDf <- matchTitles(partData, fullData, matchedDf)
#matchTable <- matchTitles(fullData$title, partData$ref_title, partData$ID) # nah needs to be the full data in the first instance I think 
# ok so now have titles of each, their closest match and IDs, I think?
matchedDf[1,]

matchedTable <- litsearchr::check_recall(fullData$title, IDLinkedRefDf$ref_title)
matchedTableDf <- as.data.frame(matchedTable)
write.csv(matchedTableDf, "../Data/matchedTableDf.csv")

setwd("E:/GitRepos/CT-MLNetworks/citationNetworks/Code")
numTitles <- length(unique(IDLinkedRefDf$ref_title))
numTitles
titleData <- fullData[,c("ID", "title")]

matchedTable <- litsearchr::check_recall(titleData$title, unique(IDLinkedRefDf$ref_title))
matchedTableDf <- as.data.frame(matchedTable)
write.csv(matchedTableDf, "../Data/matchedTableDf.csv")
