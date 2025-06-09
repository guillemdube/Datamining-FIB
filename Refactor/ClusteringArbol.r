#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned
#setwd("/home/pablesky/Documentos/MD-FIB/Refactor")
dd<- read.csv("sinMissings.csv", stringsAsFactors = TRUE)

set.seed(0)
library("dplyr")
dd<-sample_n(dd,10000)

names(dd)
dim(dd)
summary(dd)

#set a list of numerical variables
names(dd)
dcon <-c()
for (i in 1:ncol(dd)) {
  if(is.numeric(dd[,i])) {
    dcon <- c(dcon, i)
  }
}
print(dcon)
numVars <- dcon
print(dim(dcon))
dcon  <- dd[,dcon]
dim(dcon)
names(dcon)

# HIERARCHICAL CLUSTERING

d  <- dist(dcon)
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 6

c1 <- cutree(h1,nc)

c1[1:20]

tabla <- table(c1)
sumador <- 0
for (i in tabla) {
  sumador <- sumador + i
}
print(sumador)

cdg <- aggregate(as.data.frame(dcon),list(c1),mean)
cdg

#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix

actives<-c(3:ncol(dd))
print(names(dd[,actives]))
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

c2 <- cutree(h1,5)

#class sizes 

tabla2 <- table(c2)
print(tabla2)
sumador <- 0
for (i in tabla2) {
  sumador <- sumador + i
}
print(sumador)

#comparing with other partitions
table(c1,c2)

table(dend)
names(dd)
nombres=names(dd)
#ratiFin
for (i in numVars) {
  name <- names(dd)[i]
  boxplot(dd[,i]~c1, horizontal=TRUE, main=name)
}

cdg <- aggregate(as.data.frame(dcon),list(c2),mean)
cdg

qualVars <- c()
for (i in actives) {
  if(!is.numeric(dd[,i])) {
    qualVars <- c(qualVars, i)
  }
}
print(qualVars)
print(names(dd[,qualVars]))

for (i in qualVars) {
  barplot(table(dd[,i],c1),  beside=TRUE,col=c(1:length(levels(dd[,i]))), main=names(dd)[i])
  legend("topright",levels(dd[,i]),pch=1,cex=0.5, col=c(1:length(levels(dd[,i]))))
}

print(numVars)
for (i in numVars) {
  qqnorm(dd[,i], main=names(dd)[i])
  qqline(dd[,i])
}


