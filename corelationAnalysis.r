library(lsa)
library(cluster)
library(data.table)

duplicatedIndex<- function(vec, item){
  locs=c()
  for(i in 1:length(vec)){
    if (vec[i]==item){
      locs=c(locs,i)
    }
  }
  return (locs)
}
cars <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data')

for (a in 1:length(cars$V1)){
  if(cars$V1[a]=="vhigh"){
    cars$V1[a]<- (1-1)/(4-1)}
  else if (cars$V1[a]=="high"){
    cars$V1[a]<-(2-1)/(4-1)}
  else if (cars$V1[a]=="med"){
      cars$V1[a]<- (3-1)/(4-1)}
  else if (cars$V1[a]=="low"){
        cars$V1[a]<- (4-1)/(4-1)}
}
for (a in 1:length(cars$V2)){
  if(cars$V2[a]=="vhigh"){
    cars$V2[a]<- (1-1)/(4-1)}
  else if (cars$V2[a]=="high"){
    cars$V2[a]<- (2-1)/(4-1)}
  else if (cars$V2[a]=="med"){
    cars$V2[a]<- (3-1)/(4-1)}
  else if (cars$V2[a]=="low"){
    cars$V2[a]<- (4-1)/(4-1)}
}
#doors
for (a in 1:length(cars$V3)){
  if(cars$V3[a]=="2"){
    cars$V3[a]<- (1-1)/(4-1)}
  else if (cars$V3[a]=="3"){
    cars$V3[a]<- (2-1)/(4-1)}
  else if (cars$V3[a]=="4"){
    cars$V3[a]<- (3-1)/(4-1)}
  else if (cars$V3[a]=="5more"){
    cars$V3[a]<- (4-1)/(4-1)}
}
##persons
for (a in 1:length(cars$V4)){
  if(cars$V4[a]=="2"){
    cars$V4[a]<- (1-1)/(3-1)}
  else if (cars$V4[a]=="4"){
    cars$V4[a]<- (2-1)/(3-1)}
  else if (cars$V4[a]=="more"){
    cars$V4[a]<- (3-1)/(3-1)}
}
#lug_boot
for (a in 1:length(cars$V5)){
  if(cars$V5[a]=="small"){
    cars$V5[a]<- (1-1)/(3-1)}
  else if (cars$V5[a]=="med"){
    cars$V5[a]<- (2-1)/(3-1)}
  else if (cars$V5[a]=="big"){
    cars$V5[a]<- (3-1)/(3-1)}
}

##safety
for (a in 1:length(cars$V6)){
  if(cars$V6[a]=="low"){
    cars$V6[a]<- (1-1)/(3-1)}
  else if (cars$V6[a]=="med"){
    cars$V6[a]<- (2-1)/(3-1)}
  else if (cars$V6[a]=="high"){
    cars$V6[a]<- (3-1)/(3-1)}
}
## class values
for (a in 1:length(cars$V7)){
  if(cars$V7[a]=="unacc"){
    cars$V7[a]<- (1-1)/(4-1)}
  else if (cars$V7[a]=="acc"){
    cars$V7[a]<- (2-1)/(4-1)}
  else if (cars$V7[a]=="good"){
    cars$V7[a]<- (3-1)/(4-1)}
  else if (cars$V7[a]=="vgood"){
    cars$V6[a]<- (4-1)/(4-1)}
}
#dissmilarlty matrix
cars$V1=as.numeric(cars$V1)
cars$V2=as.numeric(cars$V2)
cars$V3=as.numeric(cars$V3)
cars$V4=as.numeric(cars$V4)
cars$V5=as.numeric(cars$V5)
cars$V6=as.numeric(cars$V6)
cars$V7=as.numeric(cars$V7)

c<-as.matrix(daisy(cars))

##getting rid of the duplicated values

notDuplicates=unique(sort(c))

#Q1: most similar cars

similarCars=which(c==notDuplicates[2],arr.ind = TRUE)
write.csv(similarCars,file="similarCars.csv")



##Q2 most dissimilar cars

dSimilar=max(c)
carsDissim=which(c==dSimilar,arr.ind=TRUE)

write.csv(similarCars,file="dissimilarCars.csv")

##Q3 cooreation scatterplot

carsCor=cor(cars)
corD <- unique(sort(carsCor, decreasing = TRUE))
highCor <- which(carsCor == corD[2], arr.ind = TRUE)

write.csv(highCor, file = "highlyCorrelatedAttributes.csv")
plot(cars$V6, cars$V7, xlab = "Safety", ylab = "Class Values", main = "Safety vs Class Values (Coorelation)")

#Q4 
##get "very good" cars from all the cars
vGoodCars <- subset(cars, V7 == 0)
vGoodCarsDist <- as.matrix (daisy(vGoodCars))

write.csv(vGoodCarsDist,file="vGoodCarsDissMAt.csv")
