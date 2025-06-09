setwd("/home/pablesky/Documentos/MD-FIB")
dd<- read.csv("limpio.csv")

# variables numericas
fullVariables<-c(3, 4, 5, 7, 10, 11, 12, 13, 14, 15, 16, 17)
aux<-dd[,fullVariables]

dim(aux)
names(aux)
hist(aux$Rainfall)
mean(aux$Rainfall, na.rm=TRUE)
sum(is.na(aux))

numVariables<-c(3, 4, 5, 7, 10, 11, 12, 13, 14, 15, 16, 17)
for(i in numVariables) {
  hist(dd[,i],main=paste("Histogram off", names(dd)[i]))
  qqnorm(dd[,i],main="Normal")
  qqline(dd[,i])
  
}

dimensiones <- dim(dd)
filas <- dimensiones[1]
columnas <- dimensiones[2]
indexColumnas <- (2:columnas)
for (i in indexColumnas) {
  columnTitle <- names(dd)[i]
  columnaValores <- dd[,i]
  print(columnTitle)
  print(sum(is.na(columnaValores)))
}

library(class)
numVariables<-c(3, 4, 5, 7, 10, 11, 12, 13, 14, 15, 16, 17)
aux<-dd[,numVariables]
noNA <- aux[rowSums(is.na(aux)) == 0,]
dim(dd)
dim(noNA)
perdidas <- dim(dd)[1] - dim(noNA)[1]
print("hemos dropeado este numero de filas: ")
print(perdidas)
names(aux)

for(c in 1:ncol(aux)) {  
  print(mean(aux2[,c], na.rm = TRUE))
}

sol <- aux
c=3
missings <- sol[is.na(sol[,c]),]
mids <- as.numeric(rownames(missings))
train <- aux2[mids,]
pred1 <- knn(train[,-c],noNA[,-c], train[,c],k=10)
sol[is.na(sol[,c]),] <- as.numeric(levels(pred1))

hist(dd[,3],main=paste("Histogram off", names(dd)[3]))
hist(sol[,1],main=paste("Histogram off", names(sol)[3]))
sum(is.na(sol[,4]))
mean(sol[,4])

# knn
aux2 <- aux
for(c in 1:ncol(aux)) {  
  aux2[is.na(aux[,c]),c] <- mean(aux2[,c], na.rm = TRUE)
}
aux3 <- aux
for(c in 1:ncol(aux)) {  
  aux3[is.na(aux[,c]),c] <- median(aux3[,c], na.rm = TRUE)
}
sol <- aux

for(c in 1:ncol(noNA)) {  
  aux3[is.na(aux[,c]),c] <- median(aux3[,c], na.rm = TRUE)
}
par(ask=TRUE)
for (c in 1:ncol(sol)) {
  print(c)
  if (sum(is.na(sol[,c])) > 0) {
    missings <- sol[is.na(sol[,c]),]
    mids <- as.numeric(rownames(missings))
    train <- aux2[mids,]
    pred1 <- knn(train[,-c],noNA[,-c], train[,c],k=10)
    sol[is.na(sol[,c]),] <- as.numeric(levels(pred1))
  }
  else {
    print("No missing values")
  }
}
par(ask=FALSE)
sum(is.na(sol))

# crear una tabla para cada cuidad
cities <- unique(dd$Location)
for (c in cities) {
  print(c)
  paste(c,"Database")
  df2 <- dd[which(dd$Location == c),]
  assign(c,df2)
  
}

mean()

# % de missing por variable y cuidad
city <- Adelaide
for(i in 3:ncol(city)) {
  p <- trunc((sum(is.na(city[,i]))/nrow(city))*100*100)/100
  print(paste(names(city)[i]," -- " , p,"%"))
}


# comparar original, media y mediana
aux2 <- aux
for(c in 1:ncol(aux)) {  
  aux2[is.na(aux[,c]),c] <- mean(aux2[,c], na.rm = TRUE)
}
aux3 <- aux
for(c in 1:ncol(aux)) {  
  aux3[is.na(aux[,c]),c] <- median(aux3[,c], na.rm = TRUE)
}
par(ask=TRUE)
for(c in 1:ncol(aux)) {       
  print(names(aux)[c] )
  p <- trunc((sum(is.na(aux[,c]))/nrow(aux))*100*100)/100
  print(paste(names(aux)[c]," -- " , p,"%"))
  hist(aux[,c],main=paste("Histogram off", names(aux)[c]))
  hist(aux2[,c],main=paste("Mean replacement histogram off", names(aux2)[c]))
  hist(aux3[,c],main=paste("Median replacement histogram off", names(aux3)[c]))
  hist(sol[,c],main=paste("KNN replacement histogram off", names(sol)[c]))
}
par(ask=FALSE)



aux3 <- aux2[rowSums(is.na(aux2)) > 0,]
c=4
aux2[,c] <- aux[,c]
train <- aux2[!is.na(aux2[,c]),]
dim(train)
test <- aux2[is.na(aux2[,c]),]
dim(test)
print(sum(is.na(train[,c])))
#tecnica
for(c in 1:ncol(aux)) {       # for-loop over rows
  t.ids <- createDataPartition(aux2[,c], p=0.5, list=F)
  train <- aux2[t.ids,]
  test <- aux2[-t.ids,]
  print(sum(is.na(train[,-c])))
  print(sum(is.na(test[,-c])))
  pred1 <- knn(train[,-c],test[,-c], train[,c],k=15)
  aux3 <- aux2
  c <- as.numeric(levels(pred1))
}

# create train, test and validation partitions
library(caret)
t.ids <- createDataPartition(aux$Rainfall, p=0.5, list=F)
train <- aux[t.ids,]
temp <- aux[-t.ids,]
v.ids <- createDataPartition(temp$Rainfall, p=0.5, list=F)
val <- temp[v.ids,]
test <-temp[-v.ids,]

pred1 <- knn(train[,-3],val[,-3], train[,3],k=15)
confMat1 <- table(val$Rainfall,pred1, dnn = c("Actual", "Predicted"))
confMat1 <- as.data.frame.matrix(confMat1)

library(ggplot2)
ggplot(confMat1)

for(i in 1:ncol(aux2)) {                                   # Replace NA in all columns  
  if (i!=3) aux2[ , i][is.na(aux2[ , i])] <- mean(aux2[ , i], na.rm = TRUE)
}
colSums(is.na(aux1))
#Find nns for aux2
library(class)
knn.ing <- knn(aux1,aux1,aux1[,3])
q <- table(aux$Rainfall, knn.ing)


#CARE: knn.ing is generated as a factor. 
#Be sure to retrieve the correct values

aux[is.na(aux$Rainfall)] <- as.numeric(levels(knn.ing))