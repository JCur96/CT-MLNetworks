## RelevanceCheck.R ##
## manually verify that all articles present are relevant to the study ##
## and add whether they come from CT or ML searches ## 
###### Imports ##########
library(dplyr)

#########################
setwd("C:/docNonNetwork/RProjects/citationNetworks/Code")

############# Setting the Gold Standard ##############
goldStandardML <-
  c(
    "Automated Image Recognition for Wildlife Camera Traps: Making it Work for You",
    "Zilong: A tool to identify empty images in camera-trap data",
    "ClassifyMe: A Field-Scouting Software for the Identification of Wildlife in Camera Trap Images",
    "EventFinder: a program for screening remotely captured images",
    "A deep active learning system for species identification and counting in camera trap images",
    "Animal Scanner: Software for classifying humans, animals, and empty frames in camera trap images",
    "Camera-trap images segmentation using multi-layer robust principal component analysis",
    "Camera‐trapping version 3.0: current constraints and future priorities for development",
    "Recognition in terra incognita",
    "Identifying animal species in camera trap images using deep learning and citizen science",
    "Automatically identifying, counting, and describing wild animals in camera-trap images with deep learning",
    "Machine learning to classify animal species in camera trap images: Applications in ecology",
    "Towards automatic detection of animals in camera-trap images",
    "Deep learning object detection methods for ecological camera trap data",
    "Past, present and future approaches using computer vision for animal re-identification from camera trap data",
    "Towards automatic wild animal monitoring: Identification of animal species in camera-trap images using very deep convolutional neural networks",
    "AnimalFinder: A semi-automated system for animal detection in time-lapse camera trap images",
    "Deep convolutional neural network based species recognition for wild animal monitoring",
    "Automated identification of animal species in camera trap images",
    "Application of deep learning to camera trap data for ecologists in planning/engineering--Can captivity imagery train a model which generalises to the wild?",
    "Minimizing the Annotation Effort for Detecting Wildlife in Camera Trap Images with Active Learning",
    "Automatic Camera Trap Classification Using Wildlife-Specific Deep Learning in Nilgai Management",
    "Filtering Empty Camera Trap Images in Embedded Systems",
    "A systematic study of the class imbalance problem: Automatically identifying empty camera trap images using convolutional neural networks",
    "An automatic method for removing empty camera trap images using ensemble learning",
    "Train Fast While Reducing False Positives: Improving Animal Classification Performance Using Convolutional Neural Networks",
    "Desert bighorn sheep (Ovis canadensis) recognition from camera traps based on learned features",
    "Iterative Human and Automated Identification of Wildlife Images",
    "Robust ecological analysis of camera trap data labelled by a machine learning model",
    "Automated Location Invariant Animal Detection In Camera Trap Images Using Publicly Available Data Sources",
    "U-Infuse: Democratization of Customizable AI for Object Detection",
    "Wildlife insights: A platform to maximize the potential of camera trap and other passive sensor wildlife data for the planet",
    "Identification of Wild Species in Texas from Camera-trap Images using Deep Neural Network for Conservation Monitoring",
    "Automated Image Recognition for Wildlife Camera Traps: Making it Work for You",
    "Improving the accessibility and transferability of machine learning algorithms for identification of animals in camera trap images: MLWIC2",
    "Automated detection of European wild mammal species in camera trap images with an existing and pre-trained computer vision model",
    "Camera settings and biome influence the accuracy of citizen science approaches to camera trap image classification",
    "Sequence Information Channel Concatenation for Improving Camera Trap Image Burst Classification",
    "Synthetic examples improve generalization for rare classes",
    "Improving the accessibility and transferability of machine learning algorithms for identification of animals in camera trap images: MLWIC2",
    "“How many images do I need?” Understanding how sample size per class affects deep learning model performance metrics for balanced designs in autonomous wildlife monitoring",
    "Three critical factors affecting automated image species recognition performance for camera traps",
    "Dynamic Programming Selection of Object Proposals for Sequence-Level Animal Species Classification in the Wild",
    "Fast human-animal detection from highly cluttered camera-trap images using joint background modeling and deep learning classification",
    "Sorting camera trap images",
    "Finding areas of motion in camera trap images",
    "Individual identification of wild giant pandas from camera trap photos – a systematic and hierarchical approach",
    "Animal identification in low quality camera-trap images using very deep convolutional neural networks and confidence thresholds",
    "Efficient pipeline for camera trap image review"
  )
search_dir_CT <- "../Data/goldStandardCTPapers/"
# think it works fine by just saving as a .txt, even with the xlsx still in dir (but nicer to remove maybe?)
priorCTLit <-
  litsearchr::import_results(directory = search_dir_CT, verbose = TRUE)
priorCTLit <- priorCTLit %>% dplyr::filter(pubyear >= 2012)
nrow(priorCTLit)
priorTitles <- priorCTLit$title
flatTitleList <- c()
for (i in 1:length(priorTitles)) {
  flatTitleList <- append(flatTitleList, priorTitles[i])
  
}
goldStandard <- append(flatTitleList, goldStandardML)
goldStandard <- as.data.frame(goldStandard)
names(goldStandard)[names(goldStandard) == "goldStandard"] <- "title"
########################## ML ##################
MLSCOPUS <-
  litsearchr::import_results(directory = "../Data/LitSearches/ML/SCOPUSFinal/", verbose = TRUE)
nrow(MLSCOPUS)
MLSCOPUS <-
  litsearchr::remove_duplicates(MLSCOPUS, field = "title", method = "string_osa")
nrow(MLSCOPUS)
#foundArticlesML <- litsearchr::check_recall(true_hits = goldStandardML,
#                                            retrieved = MLSCOPUSFinalImport$title)
#nrow(foundArticlesML)
mlDf <- as.data.frame(unique(MLSCOPUS))

# pull out titles and abstracts which have key words in them (very likely to be relevant)
relevantDfML <- mlDf[grep("camera", mlDf$title), ]
relevantDfML2 <- mlDf[grep("camera", mlDf$abstract), ]
relevantDfML3 <- mlDf[grep("computer", mlDf$title), ]
relevantDfML4 <- mlDf[grep("computer", mlDf$abstract), ]
relevantDfML5 <- mlDf[grep("vision", mlDf$title), ]
relevantDfML6 <- mlDf[grep("vision", mlDf$abstract), ]
relevantDfML7 <- mlDf[grep("machine", mlDf$title), ]
relevantDfML8 <- mlDf[grep("machine", mlDf$abstract), ]
relevantDfML9 <- mlDf[grep("learning", mlDf$title), ]
relevantDfML10 <- mlDf[grep("learning", mlDf$abstract), ]
relevantDfML11 <- mlDf[grep("neural", mlDf$title), ]
relevantDfML12 <- mlDf[grep("neural", mlDf$abstract), ]
relevantDfML13 <- mlDf[grep("network", mlDf$title), ]
relevantDfML14 <- mlDf[grep("network", mlDf$abstract), ]
relevantDfML15 <- mlDf[grep("camera", mlDf$author_keywords), ]
relevantDfML16 <- mlDf[grep("computer", mlDf$author_keywords), ]
relevantDfML17 <- mlDf[grep("vision", mlDf$author_keywords), ]
relevantDfML18 <- mlDf[grep("machine", mlDf$author_keywords), ]
relevantDfML19 <- mlDf[grep("learning", mlDf$author_keywords), ]
relevantDfML20 <- mlDf[grep("neural", mlDf$author_keywords), ]
relevantDfML21 <- mlDf[grep("network", mlDf$author_keywords), ]

relevantMLDf <- merge(relevantDfML, relevantDfML2, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML3, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML4, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML5, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML6, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML7, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML8, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML9, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML10, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML11, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML12, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML13, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML14, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML15, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML16, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML17, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML18, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML19, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML20, all = T)
relevantMLDf <- merge(relevantMLDf, relevantDfML21, all = T)
# and add the gold standard to this to prevent any further checks on those
inGoldStandard <- mlDf[mlDf$title %in% goldStandard$title, ]
relevantMLDf <- merge(relevantMLDf, inGoldStandard, all = T)

# 60% of titles automatically screened this way (obvs not acceptable for paper)
percentAutoRelevant <- nrow(relevantMLDf)/nrow(mlDf)*100
toCheckMLDf <- mlDf
toCheckMLDf <- toCheckMLDf[!toCheckMLDf$title %in% relevantMLDf$title, ] 

percentManualCheck <- nrow(toCheckMLDf)/nrow(mlDf)*100
percentAutoRelevant + percentManualCheck # wonderful that worked
write.csv(toCheckMLDf, "../Data/ML/relevanceCheckML.csv")




# ok thats a bit too long to do in one, maybe slice into 100 paper docs? 
nrow(toCheckMLDf)
toCheckMLDf1 <- toCheckMLDf[1:100,]
toCheckMLDf2 <- toCheckMLDf[101:200,]
toCheckMLDf3 <- toCheckMLDf[201:300,]
toCheckMLDf4 <- toCheckMLDf[301:400,]
toCheckMLDf5 <- toCheckMLDf[401:500,]
toCheckMLDf6 <- toCheckMLDf[501:600,]
toCheckMLDf7 <- toCheckMLDf[601:700,]
toCheckMLDf8 <- toCheckMLDf[701:800,]
toCheckMLDf9 <- toCheckMLDf[801:900,]
toCheckMLDf10 <- toCheckMLDf[901:1000,]
toCheckMLDf11 <- toCheckMLDf[1001:1100,]
toCheckMLDf12 <- toCheckMLDf[1101:1200,]
toCheckMLDf13 <- toCheckMLDf[1201:1300,]
toCheckMLDf14 <- toCheckMLDf[1301:1340,]

write.csv(toCheckMLDf1, "../Data/ML/relevanceCheckML1.csv")
write.csv(toCheckMLDf2, "../Data/ML/relevanceCheckML2.csv")
write.csv(toCheckMLDf3, "../Data/ML/relevanceCheckML3.csv")
write.csv(toCheckMLDf4, "../Data/ML/relevanceCheckML4.csv")
write.csv(toCheckMLDf5, "../Data/ML/relevanceCheckML5.csv")
write.csv(toCheckMLDf6, "../Data/ML/relevanceCheckML6.csv")
write.csv(toCheckMLDf7, "../Data/ML/relevanceCheckML7.csv")
write.csv(toCheckMLDf8, "../Data/ML/relevanceCheckML8.csv")
write.csv(toCheckMLDf9, "../Data/ML/relevanceCheckML9.csv")
write.csv(toCheckMLDf10, "../Data/ML/relevanceCheckML10.csv")
write.csv(toCheckMLDf11, "../Data/ML/relevanceCheckML11.csv")
write.csv(toCheckMLDf12, "../Data/ML/relevanceCheckML12.csv")
write.csv(toCheckMLDf13, "../Data/ML/relevanceCheckML13.csv")
write.csv(toCheckMLDf14, "../Data/ML/relevanceCheckML14.csv")

################################## CT #################
CTSCOPUSImport <-
  litsearchr::import_results(directory = "../Data/LitSearches/CT/SCOPUSFinal/", verbose = TRUE)
nrow(CTSCOPUSImport)
CTSCOPUSImport <-
  litsearchr::remove_duplicates(CTSCOPUSImport, field = "title", method = "string_osa")
nrow(CTSCOPUSImport)
ctDf <- as.data.frame(unique(CTSCOPUSImport))
colnames((ctDf))
# If camera terms in title OR abstract, immediately keep as relevant? Filter
#relevantDf <- ctDf %>% filter(title contains "camera" || abstract contains "camera")
relevantDf <- ctDf[grep("camera", ctDf$title), ]
relevantDf2 <- ctDf[grep("camera", ctDf$abstract), ]
relevantDf3 <- ctDf[grep("trap", ctDf$title), ]
relevantDf4 <- ctDf[grep("trap", ctDf$abstract), ]
#not sure what other low hanging fruit there is for this one 
relevantDf5 <- ctDf[grep("camera", ctDf$author_keywords), ]
relevantCTDf <- merge(relevantDf, relevantDf2, all = T)
relevantCTDf <- merge(relevantCTDf, relevantDf3, all = T)
relevantCTDf <- merge(relevantCTDf, relevantDf4, all = T)
relevantCTDf <- merge(relevantCTDf, relevantDf5, all = T)
# get 57% of articles automatically screened.
inGoldStandard <- ctDf[ctDf$title %in% goldStandard$title, ]
relevantCTDf <- merge(relevantCTDf, inGoldStandard, all = T)
percentAutoRelevant <- nrow(relevantCTDf)/nrow(ctDf)*100
toCheckCTDf <- ctDf
toCheckCTDf <- toCheckCTDf[!toCheckCTDf$title %in% relevantCTDf$title, ] 
percentManualCheck <- nrow(toCheckCTDf)/nrow(ctDf)*100
percentAutoRelevant + percentManualCheck # wonderful that worked
write.csv(toCheckCTDf, "../Data/CT/relevanceCheckCT.csv")



# Now pull out the ones which weren't for ML

# Save to csv for manual screen
# And for CT 

# Save to csv for manual screen




# relevantDf3 <- rbind(relevantDf, relevantDf2)
# relevantDf4 <- litsearchr::remove_duplicates(relevantDf3, field = "title", method = "string_osa")
#foundArticlesCT <- litsearchr::check_recall(true_hits = goldStandard,
#                                            retrieved = CTSCOPUSFinalImport$title)
# #nrow(foundArticlesCT)
# View(goldStandard)
# goldStandardDF <- as.data.frame(goldStandard)
# View(goldStandardDF)
# nrow(goldStandardDF)
# nrow(foundArticlesCT)
# badMatchesCT <- foundArticlesCT[foundArticlesCT[,"Similarity"] < 1,]
# #badMatchesCT
# nrow(badMatchesCT)
# nrow(badMatchesCT)/nrow(goldStandardDF)*100
