
#PREREQUISITES: 
#factors are properly labelled and reading data makes R to directly recognize them
#Numerical variables do not contain missing values anymore. They have been imputed in preprocessing step

#  READING CREDSCO_BIN
# load("d:/karina/docencia/sreferenciesppt/16.AssociatiusVisualitzacio/MultivariateAnalysis/PracticaR/credscok_bin")
install.packages("dplyr")

setwd("/home/pablesky/Documentos/MD-FIB/Refactor")
dd <- read.csv("sinMissings.csv");
set.seed(0)
library("dplyr")
ddr<-sample_n(dd,10000)

#alternatively 
objects()
attributes(ddr)

# VISUALISATION OF DATA
## PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
### CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

attach(ddr)
names(ddr)

#is R understanding well my factor variables?
sapply(ddr,class)

#set a list of numerical variables (with no missing values)
numeriques <- which(sapply(ddr,is.numeric))
numeriques

dcon <- ddr[, numeriques]
sapply(dcon, class)

#dcon <- data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, RatiFin)

#alternatively
#dim(ddr)
#indexCon<-c(2,4:5,9:16)
#dcon<-ddr[,indexCon]
#names(dcon)

#be sure you don't have missing data in your numerical variables.

#in case of having missing data, select complete rows JUST TO FOLLOW THE CLASS
#ddr<-ddr[!is.na(ddr[,indecCon[1]])& !is.na(ddr[,indecCon[2]]) & !is.na(ddr[,indecCon[3]])& !is.na(ddr[,indecCon[4]]),]
#then preprocess your complete data set to IMPUTE all missing data, and reproduce
#the whole analysis again
# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 6 

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis
eje1<-2
eje1<-1
eje2<-3
eje2<-2

{plot(Psi[,eje1],Psi[,eje2])
  text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")}

#library(rgl)
#plot3d(Psi[,1],Psi[,2],Psi[,3])

#Projection of variables

Phi = cor(dcon,Psi)
View(Phi)

#select your axis
X<-Phi[,eje1]
Y<-Phi[,eje2]

{plot(Psi[,eje1],Psi[,eje2],type="n")
  axis(side=1, pos= 0, labels = F)
  axis(side=3, pos= 0, labels = F)
  axis(side=2, pos= 0, labels = F)
  axis(side=4, pos= 0, labels = F)
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)}


#zooms
{plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
  axis(side=1, pos= 0, labels = F)
  axis(side=3, pos= 0, labels = F)
  axis(side=2, pos= 0, labels = F)
  axis(side=4, pos= 0, labels = F)
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)}

# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

varcat=as.factor(ddr[,1])
plot(Psi[,1],Psi[,2],col = 1:length(levels(varcat)))
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)

#select your qualitative variable
k<-18 #dictamen in credsco

varcat<-ddr[,k]
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)


#Now we project both cdgs of levels of a selected qualitative variable without
#representing the individual anymore

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#select your qualitative variable
k<-18 #dictamen in credsco

#varcat<-ddr[,k]
#fdic1 = tapply(Psi[,eje1],varcat,mean)
#fdic2 = tapply(Psi[,eje2],varcat,mean) 

#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)


#all qualitative together
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#nominal qualitative variables

dcat<-c(1,3,6:7)
#divide categoricals in several graphs if joint representation saturates

#build a palette with as much colors as qualitative variables 

#colors<-c("blue","red","green","orange","darkgreen")
#alternative
colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],ddr[,k],mean)
  fdic2 = tapply(Psi[,eje2],ddr[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(ddr[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(ddr)[dcat],pch=1,col=colors, cex=0.6)

#determine zoom level
#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
#fm = round(max(abs(Psi[,1]))) 
# fm=20



#scale the projected variables
# X<-fm*U[,eje1]
# Y<-fm*U[,eje2]

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
plot(X,Y,type="none",xlim=c(-2,1), ylim=c(-2,2))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#addr projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#addr centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],ddr[,k],mean)
  fdic2 = tapply(Psi[,eje2],ddr[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, abels=levels(ddr[,k]))
  text(fdic1,fdic2,labels=levels(ddr[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(ddr)[dcat],pch=1,col=colors, cex=0.6)



#addr ordinal qualitative variables. Ensure ordering is the correct
dordi<-c(8)


levels(ddr[,dordi[1]])
#reorder modalities: when required
ddr[,dordi[1]] <- factor(ddr[,dordi[1]], ordered=TRUE,  levels= c("WorkingTypeUnknown","altres sit","temporal","fixe","autonom"))
levels(ddr[,dordi[1]])

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],ddr[,k],mean)
  fdic2 = tapply(Psi[,eje2],ddr[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(ddr[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(ddr[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(ddr)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)



# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

varcat=ddr[,1]
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)


# Overproject THE CDG OF  LEVELS OF varcat
fdic1 = tapply(Psi[,1],varcat,mean)
fdic2 = tapply(Psi[,2],varcat,mean) 

text(fdic1,fdic2,labels=levels(varcat),col="cyan", cex=0.75)
