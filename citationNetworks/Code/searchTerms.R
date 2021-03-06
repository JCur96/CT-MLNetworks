## searchTerms.R
# Lifted demo scripts of use of the two titled packages
# Edited to use a novel data set
######################################################
################## Imports ###########################
library(litsearchr)
library(tidyr)
library(bibliometrix)

######################################################
################ Functions ###########################
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")

generateTerms <- function(inputDir, outFilePath, removedupeMethod = "string_osa", 
                          numStudiesTermOccurs = 10, numTimesTermOccurs = 10, 
                          cutoffMethod = "changepoint", numKnots = 3, 
                          numCumulative = 0.8, cutoffImpl = "strength",
                          scopus = TRUE) 
{
  import <-
    litsearchr::import_results(directory = inputDir, verbose = TRUE)
  results <-
    litsearchr::remove_duplicates(import, field = "title", method = removedupeMethod)
  
  rakedkeywords <-
    litsearchr::extract_terms(
      text = paste(results$title, results$abstract),
      method = "fakerake",
      min_freq = 2,
      ngrams = TRUE,
      min_n = 2,
      language = "English"
    )
  
  # authorKeywords <- tryCatch(litsearchr::extract_terms(
  #   keywords = results$author_keywords,
  #   method = "tagged",
  #   min_freq = 2,
  #   ngrams = TRUE,
  #   min_n = 2,
  #   language = "English"), 
  #   finally = print("No author keywords, trying index key words"))
  # 
  # tryCatch(indexKeywords <- litsearchr::extract_terms(
  #   keywords = results$index_keywords,
  #   method = "tagged",
  #   min_freq = 2,
  #   ngrams = TRUE,
  #   min_n = 2,
  #   language = "English"
  # ), finally = "No index key words, trying tagged keywords")
  # 
  # tryCatch( taggedkeywords <-
  #             litsearchr::extract_terms(
  #               keywords = results$keywords,
  #               method = "tagged",
  #               min_freq = 2,
  #               ngrams = TRUE,
  #               min_n = 2,
  #               language = "English"
  #             ), finally("No tagged key words, likely SCOPUS document"))
  
# differs between scopus and WoS, for WoS use results$keywords, for scopus
#  use both / either of results$index_keywords and results$author_keywords
  if (scopus == TRUE) {
    authorKeywords <- litsearchr::extract_terms(
      keywords = results$author_keywords,
      method = "tagged",
      min_freq = 2,
      ngrams = TRUE,
      min_n = 2,
      language = "English"
    )
    indexKeywords <- litsearchr::extract_terms(
      keywords = results$index_keywords,
      method = "tagged",
      min_freq = 2,
      ngrams = TRUE,
      min_n = 2,
      language = "English"
    )

  } else {

    taggedkeywords <-
      litsearchr::extract_terms(
        keywords = results$keywords,
        method = "tagged",
        min_freq = 2,
        ngrams = TRUE,
        min_n = 2,
        language = "English"
      )
  }
  
  # tryCatch() # need to work out the try catches tbh 
  if (scopus == TRUE) {
    allKeywords <- unique(append(rakedkeywords, authorKeywords))
    allKeywords <- unique(append(all_keywords, indexKeywords))
  } else {
    allKeywords <- unique(append(taggedkeywords, rakedkeywords))
  }
 
  #print("Generating document feature matrix...")
  dfm <-
    litsearchr::create_dfm(
      elements = paste(results$title, results$abstract),
      features = all_keywords
    )
  #print("done")
  graph <-
    litsearchr::create_network(
      search_dfm = dfm,
      min_studies = numStudiesTermOccurs,
      min_occ = numTimesTermOccurs
    )
  if (cutoffMethod == "changepoint") {
    cutoff <-
      litsearchr::find_cutoff(
        graph,
        method = "changepoint",
        knot_num = numKnots,
        imp_method = cutoffImpl
      ) 
  } else {
    cutoff <-
      litsearchr::find_cutoff(
        graph,
        method = "cumulative",
        percent = numCumulative,
        imp_method = cutoffImpl
      )
  }
  
  reducedgraph <-
    litsearchr::reduce_graph(graph, cutoff_strength = cutoff[1])
  
  searchterms <- litsearchr::get_keywords(reducedgraph)
  write.csv(searchterms, outFilePath)
  
}

compareAgainstGold <- function(inputDir, goldStandard)
{
  
  retrievedArticles <-
    litsearchr::import_results(directory = inputDir, verbose = TRUE)
  retrievedArticles <- litsearchr::remove_duplicates(retrievedArticles, field="title", method="string_osa")
  articlesFound <- litsearchr::check_recall(true_hits = goldStandard,
                                             retrieved = retrievedArticles$title)
  
  return(articles_found)
}

######################################################
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
#goldStandard
#title_search <- litsearchr::write_title_search(titles=goldStandard)
#####################################################
################## Code #############################
## Read in the initial search for CT papers
## Compare to the CT gold standard papers 
## I.e. those found in;
## https://www.frontiersin.org/articles/10.3389/fevo.2021.617996/full
## After 2012 
######################################################

################# CT Term Generation ################
CTImportDir <- "../Data/LitSearches/CT/FirstCTSearch/"
articlesFound <- compareAgainstGold(CTImportDir, priorCTLit)
articlesFound
CTImport1 <-
  litsearchr::import_results(directory = CTImportDir, verbose = TRUE)
CTImport1 <-
  litsearchr::remove_duplicates(CTImport1, field = "title", method = "string_osa")
articles_found1 <- litsearchr::check_recall(true_hits = goldStandard,
                                           retrieved = CTImport$title)

nrow(articles_found1)
write.csv(articles_found, "../Results/articlesFound.csv")
m <- articles_found1
badMatches <- m[m[,"Similarity"] < 1,]
badMatches
nrow(badMatches)
write.csv(badMatches, "../Results/articlesFound.csv")
nrow(badMatches)/nrow(goldStandardDF)*100
## that's 14% of articles missed, possibly an issue, might be solved with scopus ##
## ok so WoS doesn't pick up on conference proceedings I think ##
## misses the beery paper Efficient pipeline for camera trap image review ##
## get that in the secondary documents in socpus tho ##
## using scopus ##
CTImport2 <-
  litsearchr::import_results(directory = "../Data/LitSearches/CT/scopus/", verbose = TRUE)
CTImport2 <-
  litsearchr::remove_duplicates(CTImport2, field = "title", method = "string_osa")
nrow(CTImport2)
articles_foundScopus2 <- litsearchr::check_recall(true_hits = goldStandard,
                                           retrieved = CTImport$title)
nrow(goldStandard)
nrow(articles_foundScopus)
badMatches <- m[m[,"Similarity"] < 1,]
badMatches
nrow(badMatches)
nrow(badMatches)/nrow(articles_foundScopus)*100
write.csv(badMatches, "../Results/badMatchesArticlesFound.csv")
### at somewhat of a loss, seems to be the same set of bad matches ##
## will have to look at this with fresh eyes wed afternoon I think ##

## fresh eyes say find the papers on WoS or SCOPUS to check that they exist ##
## or are at least accessible via simple search ## 

## worrying about CT papers now ##
# generate terms using the papers # 
generateTerms("../Data/LitSearches/CT/scopus/", "../Results/searchTerms/CT/ctSearchTerms.csv", scopus = TRUE)
# wow that took like 5 hours to run #
# but it did run! #
# read the grouped terms back in and generate the search #
ctTerms <- read.csv("../Results/searchTerms/CT/ctSearchTermsGrouped.csv")
# extract the camera terms from the csv
termsCamera <- ctTerms$term[grep("camera", ctTerms$group)]
# pull out other relevant terms here
termsML <- ctTerms$term[grep("ml", ctTerms$group)]
termsStudyType <- ctTerms$term[grep("studyType", ctTerms$group)]
termsLocation <- ctTerms$term[grep("location", ctTerms$group)]
termsSpecies <- ctTerms$term[grep("species", ctTerms$group)]
# join together a list of manually generated woodpecker terms with the ones from the csv
termsCTcameras <- unique(append(c("camera-trap","camera-traps", "camera trap", 
                           "infrared triggered camera", "trail camera", 
                           "automatic camera", "photo trap", "remote camera", 
                           "remotely triggered camera"), termsCamera)) ## add the original search terms here
# repeat this for all concept groups e.g. AI/ ML etc. for the real run 
# then merge them into a list, using the code below as an example
# mysearchterms <- list(woodpeckers, fire)
cameraGroup <- list(termsCTcameras)
mysearchterms <- cameraGroup
# now generate search using the terms and the first search together #

################################################################################


# ok so gonna worry about the missing papers for CT later, moving on to ML terms ## 
# used the papers from the gold standard ML themselves as search terms in scopus ##
# now will read the results from that in and see what matches we get # 
# then generate terms based on that # 

## OK so generate terms using all of the results from SCOPUS and WoS ###
## then search again using original plus generated and see how we do ##
generateTerms() # will have to rewrite this fun to try catch for keywords as merging WoS and Scopus



















##################### ML Term Generation #############################

### using scopus ##
MLImport1 <-
  litsearchr::import_results(directory = "../Data/LitSearches/ML/SCOPUSInitial/", verbose = TRUE)
MLImport1 <-
  litsearchr::remove_duplicates(MLImport1, field = "title", method = "string_osa")
nrow(MLImport1)
articles_foundScopusML1 <- litsearchr::check_recall(true_hits = goldStandardML,
                                                 retrieved = MLImport$title)
nrow(articles_foundScopusML1)
# ok perfect it hits all 49 papers, lets generate terms # 
mlTermsInDir <- "../Data/LitSearches/ML/SCOPUSInitial/"
mlOutFilePath <- "../Results/searchTerms/ML/mlSearchTerms.csv"
generateTerms(mlTermsInDir, mlOutFilePath, numStudiesTermOccurs = 3, numTimesTermOccurs = 5, scopus = TRUE)
# manually group ML terms then return to the next line #
termsML <- read.csv("../Results/searchTerms/ML/mlSearchTermsGrouped.csv")
# extract the camera terms from the csv
termsCTML <- termsML$term[grep("camera", termsML$group)]
termsMLML <- termsML$term[grep("ML", termsML$group)]
termsEcologyML <- termsML$term[grep("ecology", termsML$group)]
termsAllML <- list(termsCTML, termsMLML, termsEcologyML)
mlSearch <-
  litsearchr::write_search(
    groupdata = termsAllML,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

write(mlSearch, '../Data/ML/mlSearchTerms.txt')

# second search read in # 
MLImport <-
  litsearchr::import_results(directory = "../Data/LitSearches/ML/SCOPUSSecond/", verbose = TRUE)
nrow(MLImport)
MLImport <-
  litsearchr::remove_duplicates(MLImport, field = "title", method = "string_osa")
nrow(MLImport)
articles_foundScopusML <- litsearchr::check_recall(true_hits = goldStandardML,
                                                   retrieved = MLImport$title)
nrow(articles_foundScopusML)
# ok great, these hit all of the gold standard ml #
# combine both initial and this second and that will probably be sufficient #

#################### Unifying terms across CT and ML searches ##################
termsUnifiedCT <- unique(append(termsCTcameras, termsCTML))
termsUnifiedML <- unique(append(termsML , termsMLML))
# possibly need ecology / study type unified terms for each too?
termsunifiedEcol <- unique(append(termsEcologyML, termsStudyType))
termsAllUnifiedML <- list(termsUnifiedCT, termsUnifiedML, termsunifiedEcol)
searchMLFinal <-
  litsearchr::write_search(
    groupdata = termsAllUnifiedML,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )
write(searchMLFinal, '../Data/ML/searchMLFinal.txt')
termsAllUnifiedCT <- list(termsUnifiedCT, termsunifiedEcol)
searchCTFinal <-
  litsearchr::write_search(
    groupdata = termsAllUnifiedCT,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )
write(searchCTFinal, '../Data/CT/searchCTFinal.txt')


# termsAllUnifiedEcol <- list(termsunifiedEcol)
# searchEcolFinal <-
#   litsearchr::write_search(
#     groupdata = termsAllUnifiedEcol,
#     languages = "English",
#     stemming = TRUE,
#     closure = "none",
#     exactphrase = TRUE,
#     writesearch = FALSE,
#     verbose = TRUE
#   )
# 
# write(searchEcolFinal, '../Data/ML/searchEcolFinal.txt')




################# Comparison to Gold #########################
MLSCOPUSFinalImport <-
  litsearchr::import_results(directory = "../Data/LitSearches/ML/SCOPUSFinal/", verbose = TRUE)
nrow(MLSCOPUSFinalImport)
MLSCOPUSFinalImport <-
  litsearchr::remove_duplicates(MLSCOPUSFinalImport, field = "title", method = "string_osa")
nrow(MLSCOPUSFinalImport)
foundArticlesML <- litsearchr::check_recall(true_hits = goldStandardML,
                                                   retrieved = MLSCOPUSFinalImport$title)
nrow(foundArticlesML)


CTSCOPUSFinalImport <-
  litsearchr::import_results(directory = "../Data/LitSearches/CT/SCOPUSFinal/", verbose = TRUE)
nrow(CTSCOPUSFinalImport)
CTSCOPUSFinalImport <-
  litsearchr::remove_duplicates(CTSCOPUSFinalImport, field = "title", method = "string_osa")
nrow(CTSCOPUSFinalImport)
foundArticlesCT <- litsearchr::check_recall(true_hits = goldStandard,
                                                   retrieved = CTSCOPUSFinalImport$title)
nrow(foundArticlesCT)
View(goldStandard)
goldStandardDF <- as.data.frame(goldStandard)
View(goldStandardDF)
nrow(goldStandardDF)
nrow(foundArticlesCT)
badMatchesCT <- foundArticlesCT[foundArticlesCT[,"Similarity"] < 1,]
#badMatchesCT
nrow(badMatchesCT)
nrow(badMatchesCT)/nrow(goldStandardDF)*100


################################################################################


inputDir <- "../Data/LitSearches/CT/FirstCTSearch/"
outFilePath <- "../Results/searchTerms/CT/OfficialRun/search_terms.csv"
generateTerms(inputDir, outFilePath)

############

######### post-grouping code #############
# manually group terms in the csv # 
# not sure there is any way around the above.
# groups are determined by the researcher, e.g. camera-trap or AI (in the example its Woodpeckers, fire etc.)
# relabel the X col to terms
grouped_terms <- read.csv("../Data/search_terms_grouped.csv")
# extract the camera terms from the csv
camera_terms <- grouped_terms$term[grep("cameras", grouped_terms$group)]
# join together a list of manually generated woodpecker terms with the ones from the csv
cameras <- unique(append(c("camera-trap","camera-traps"), camera_terms))
# repeat this for all concept groups e.g. AI/ ML etc. for the real run 
# then merge them into a list, using the code below as an example
# mysearchterms <- list(woodpeckers, fire)
cameraGroup <- list(cameras)
mysearchterms <- cameraGroup
######
# Note: these search terms are a shortened example of a full search for illustration purposes only
# additionally, this is if you want to do it manually, not via the above? 
# mysearchterms <-
#   list(
#     c(
#       "picoides arcticus",
#       "black-backed woodpecker",
#       "cavity-nesting birds",
#       "picoides tridactylus",
#       "three-toed woodpecker"),
#     c(
#       "wildfire",
#       "burned forest",
#       "post-fire",
#       "postfire salvage logging",
#       "fire severity",
#       "recently burned"
#     )
#   )

my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

# when writing to a plain text file, the extra \ are required to render the * and " properly
# if copying straight from the console, simply find and replace them in a text editor
write(my_search, '../Data/SearchTerms.txt')
# my_search
############# END post grouping code ################



############ Validating search terms ##############
## Run the binary search in WoS, SCOPUS etc.
## Import the results and compare to the gold standard list
## Happiness
######################
retrivedResultsDir <- "../Results/litSearches/CT"
iteratedResultsOutDir <- "../Data/CT/search_terms_iter2.csv"
generateTerms(retrivedResultsDir, iteratedResultsOutDir)
results_directoryCT <- "../Results/litSearches/CT"
articlesFoundCT <- compareAgainstGold(results_directoryCT, goldStandard)
articles_found
write.csv(articles_found, "../Results/articlesFound.csv")
m <- articles_found
badMatches <- m[m[,"Similarity"] < 1,]
badMatches
badMatches <- m[m[,"Similarity"] < 0.5,]
badMatches
nrow(badMatches)
badMatches[,"Title"]



############## ML Initial #######################
mlInDir <- "../Data/LitSearches/ML/" # system.file("extdata", package="litsearchr")
mlOutDir <- "../Data/search_terms_ML.csv"
generateTerms(mlInDir, mlOutDir)
title_search <- litsearchr::write_title_search(titles=goldStandardML)
#title_search
goldStandardML
results_directory <- "../Results/litSearches/ML"
articles_found <- compareAgainstGold(results_directory, goldStandardML)
articles_found
write.csv(articles_found, "../Results/articlesFoundML.csv")
m <- articles_found
badMatches <- m[m[,"Similarity"] < 1,]
badMatches
nrow(badMatches)
badMatches <- m[m[,"Similarity"] < 0.5,]
badMatches
nrow(badMatches)
badMatches[,"Title"]
goodMatches <- m[m[,"Similarity"] == 1,]
nrow(goodMatches)
goodMatches

#####
