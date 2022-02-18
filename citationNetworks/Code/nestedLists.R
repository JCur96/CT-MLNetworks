#nested list looping 
l1 <- list(1:5)
l2 <- list(6:10)
l3 <- list(11:15)

nestedList <- list(l1, l2, l3)


for (i in 1:length(nestedList)) {
  sublist <- nestedList[i]
  #print(sublist)
  for (j in 1:length(sublist)) {
   print(sublist[j]) 
  }
}
