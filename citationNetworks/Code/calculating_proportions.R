########### ----- This code calculates the proportion of camera trap papers that cite ML papers, overall, and by year
# read in data
set.seed(1)
setwd("C:/docNonNetwork/RProjects/CT-MLNetworks/citationNetworks/Code")
library(ggplot2)
data <- read.csv("../Data/proportionsData.csv")

# total number of CT papers
CT_ids <- unique(data[data$citingSearch == "CT","citingID"]) # vector of CT papers = number of unique citing/cited IDs in the CT search subset
CT_ids <- CT_ids[!is.na(CT_ids)] # remove NA
n_CT <- length(CT_ids) # total number of CT papers in the data (including those with degree = 0)

# total number of CT papers BY YEAR
n_CT_byyear <- sapply(split(data, data$citingYear), function(x){ # split data by year and repeat the above code for year subset
  CT_ids <- unique(x[x$citingSearch == "CT","citingID"]) # vector of CT papers = number of unique citing/cited IDs in the CT search subset
  CT_ids <- CT_ids[!is.na(CT_ids)] # remove NA
  length(CT_ids) # total number of CT papers in the data (including those with degree = 0)
})

# make edge list
edges <- data[complete.cases(data),] # edges = data with rows containing NAs removed
# edges <- m[complete.cases(m),] # edges = data with rows containing NAs removed

# all edges FROM ct papers TO ml papers
edges_ct2ml <- edges[(edges$citingSearch == "CT") & (edges$citedSearch == "ML"),] 

# OVERALL proportion of CT papers that cite ML papers
length(unique(edges_ct2ml$citingID))/n_CT # number of ct papers that cite ML papers = number of unique IDs in the ct2ml edgelist; to get the proportion, divide by total number of CT papers
# 0.4760776

# BY YEAR proportion of CT papers that cite ML papers
years <- unique(edges_ct2ml$citingYear) # years in the edges_ct2ml data
proportion_byyear <- sapply(years, function(x){
  edges_ct2ml_focalyear<- edges_ct2ml[edges_ct2ml$citingYear == x,]
  length(unique(edges_ct2ml_focalyear$citingID))/n_CT_byyear[as.character(x)] # number of ct papers that cite ML papers = number of unique IDs in the ct2ml edgelist; to get the proportion, divide by total number of CT papers
})

plot(as.numeric(names(proportion_byyear)), proportion_byyear)

mean(proportion_byyear)

# ggplot this out 
class(proportion_byyear)
proportion_byyear
prop <- data.frame(keyName=names(proportion_byyear), value=proportion_byyear, row.names=NULL)
names(prop)[names(prop) == c("keyName", "value")] <- c("Year", "Proportion")
captionText <- "Figure 2. Proportion of papers appearing in the camera trap specific search that cite papers appearing 
in the camera trap with machine learning search. The mean yearly proportion is 0.42. "
p <- ggplot(prop, aes(Year, Proportion)) + 
  geom_point(size = 4) +
  xlab("Year") +
  ylab("Proportion") +
  labs(caption = captionText) #+
  #geom_hline(yintercept = mean(prop$proportion), color="blue")
p # maybe add annotations for how many CT papers each year has? 
p + theme(
  # axis.text = element_text(size = 20),
  text = element_text(size = 20),
  plot.caption = element_text(hjust = 0, size = 30, face = 'bold')
)
ggsave("../Graphics/propByYear.png", p, width = 15, height = 12, units = 'cm')

########### ----- Sense check to make sure numbers match up between overall and by year data
length(unique(edges_ct2ml$citingID)) # overall number of CT papers that cite ML papers (from above)

# BY YEAR number of CT papers that cite ML papers (same as above, just removing the n_CT_byyear denominator at the end)
number_byyear <- sapply(years, function(x){
  edges_ct2ml_focalyear<- edges_ct2ml[edges_ct2ml$citingYear == x,]
  length(unique(edges_ct2ml_focalyear$citingID)) # number of ct papers that cite ML papers = number of unique IDs in the ct2ml edgelist; to get the proportion, divide by total number of CT papers
})

sum(number_byyear) == length(unique(edges_ct2ml$citingID)) # TRUE - hooray!

