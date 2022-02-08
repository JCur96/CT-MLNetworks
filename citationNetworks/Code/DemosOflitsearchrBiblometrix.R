## DemosOflitsearchrBiblometrix.R
# Lifted demo scripts of use of the two titled packages
# Edited to use a novel data set
#################################
###########imports ##############
library(litsearchr)
library(tidyr)
library(bibliometrix)

#################################
setwd("C:/docNonNetwork/RProjects/citationNetworks/Code")

# Note: system.file() is only used to identify where the example datasets are stored
# If litsearchr and its dependencies were successfully installed, this directory exists on your computer

# If you are using your own bibliographic files, you should not use system.file
# You should instead give it the full path (or relative path from your current working directory) to the directory where your files are stored
search_directory <- "../Data/LitSearches/" # system.file("extdata", package="litsearchr")

naiveimport <-
  litsearchr::import_results(directory = search_directory, verbose = TRUE)
#> Reading file /tmp/RtmpGp7mOt/temp_libpath605c6719a3ad/litsearchr/extdata/scopus.ris ... done
#> Reading file /tmp/RtmpGp7mOt/temp_libpath605c6719a3ad/litsearchr/extdata/zoorec.txt ... done

naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 10,
    min_occ = 10
  )
# cutoff <-
#   litsearchr::find_cutoff(
#     naivegraph,
#     method = "cumulative",
#     percent = .80,
#     imp_method = "strength"
#   )
cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "changepoint",
    knot_num = 3,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 20)
######
write.csv(searchterms, "../Data/search_terms.csv")
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
#####
# The gold standard will include known literature from both CVMLCT and CT works
gold_standard_ML <-
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

just_titles <- priorCTLit$title
#just_titles
#gold_standard_CT <- list(priorCTLit$title)
flat_title_list <- c()
for (i in 1:length(just_titles)) {
  flat_title_list <- append(flat_title_list, just_titles[i])
  
}

gold_standard <- append(flat_title_list, gold_standard_ML)
gold_standard
title_search <- litsearchr::write_title_search(titles=gold_standard)
results_directory <- "../Results/litSearches"
############ Validating search terms ##############
## Run the binary search in WoS, SCOPUS etc.
## Import the results and compare to the gold standard list
## Happiness
######################
retrieved_articles <-
  litsearchr::import_results(directory = results_directory, verbose = TRUE)
retrieved_articles <- litsearchr::remove_duplicates(retrieved_articles, field="title", method="string_osa")
articles_found <- litsearchr::check_recall(true_hits = gold_standard,
                                           retrieved = retrieved_articles$title)
articles_found 
m <- articles_found
badMatches <- m[m[,"Similarity"] < 1,]
badMatches
nrow(badMatches)
badMatches[,"Title"]

#articles_found_df <- data.frame(articles_found)
#articles_found_df <- articles_found_df %>% dplyr::filter(Similarity < 1)

#####


######### Bibliometrix ################

#######################################
file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
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
