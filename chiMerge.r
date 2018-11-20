#IMPLEMENTAION OF THE CHI-MERGE ALGORITHM

#Read data
#install.packages("dplyr")
library(dplyr)
irisData <- read.table("iris.data",sep=",")
irisData <- rename(irisData,sepal_length=V1,sepal_width=V2,petal_length=V3,petal_width=V4,iris_class=V5)

#preprocessing
irisData=irisData[order(-irisData[,1],decreasing=T),]
##creating frequency tab

sepal_length <- table(irisData[,1],irisData[,5])
sepal_width <- table(irisData[,2],irisData[,5])
petal_length <- table(irisData[,3],irisData[,5])
petal_width <- table(irisData[,4],irisData[,5])

class_freq=table(irisData[,5])

chi_function <- function(intervals){
  rows=length(intervals[,1])
  cols=length(intervals[1,])

  rowSum=rowSums(intervals)
  colSum=colSums(intervals)
  
  chiVal=0
  for (i in seq_len(rows)){
    for (j in seq_len(cols)){
      estimate=(rowSum[i]*colSum[j])/(sum(colSum))
      if (estimate!=0){
        chiVal=chiVal+(((intervals[i,j]-estimate)**2)/estimate)
      }
    }
  }
  return(chiVal)
}


max_interval=6

chi_merge <-function(attrTable,max_interval){
  
  ##For each pair of adjacent rows in the frequency table calculate e the expected frequency value
  #for that combination from the product of row and column sums divided by the total number of occurrences in the two rows combined
  num_interval=length(attrTable[,1])
  print (attrTable)
  while(num_interval>max_interval){
    
    num_pair=num_interval-1
    chi_values=c()
    ##calculate the chi value of each neighbor interval
    
    for (x in seq_len(num_pair)){
      intervals=c(attrTable[x,],attrTable[x+1,])
      intervals=matrix(intervals,2,3,byrow = T)
      chi_values=c(chi_values,chi_function(intervals))
    }
    min_chi=min(chi_values)
    for (a in num_pair:1){
      if (chi_values[a]==min_chi){
        attrTable[a,]=attrTable[a,]+attrTable[a+1,]
        attrTable=attrTable[-(a+1),]
      }
    }
    num_interval=length(attrTable[,1])

  }
  return (attrTable)
}

merged_sepal_length=chi_merge(sepal_length,9.24)
merged_petal_length=chi_merge(petal_length,9.24)
print (merged_sepal_length)
print (merged_petal_length)