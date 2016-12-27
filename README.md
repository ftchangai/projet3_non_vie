# projet3_non_vie


rm(list=ls())
setwd("C:/Users/Utilisateur/Documents/ENSAE 2016_2017/actuariat assurance non vie/DM3")


library(splines)
library(lsr)
library(MASS)
library(ROCR)
library(pROC)
library(lattice)
library(tm)
library(utils)
library(SnowballC)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(hmeasure)
library(wordcloud)
library(texreg)
library(stargazer)
library(ade4)
library(doBy)
library(xlsx)
library(party)
library(ipred)
library(randomForest)
library(parallel)
library(ParallelPC)
library(ParallelForest)
library(doParallel)
library(dplyr)
library(class)
library(mgcv)
library(AER)
library(pscl)
library(readODS)
library(ChainLadder)




claims<-read.csv("triangle_ensae.csv", header = TRUE, sep=";")
claims1<-claims[,-1]


cum.paid<-claims[,-1]
cum.paid=as.vector(t(as.matrix(cum.paid)))
cum.paid<-cum.paid[!is.na(cum.paid)]


n=14


Claims<-data.frame(originf=factor(rep(2003:2016, n:1)),
                   dev=sequence(n:1),cum.paid)


cum.triangle<-with(Claims, {
  M<-matrix(nrow=n, ncol=n,
            dimnames=list(origin=levels(originf), dev=1:n))
  M[cbind(originf,dev)]<-cum.paid
  M
})

originf=2003:2016
dev=1:n

#===================brouillon

#en remplace les N-1 par des années
#claims1$Années<-2003:2016
#claims2<-claims1

#on met la premire colonne en nom
#claims2 <- data.frame(claims1[,-1], row.names=claims1[,1])
 

#n=nrow(claims)


#originf=2003:2016
#dev=1:n
#claims5=claims[,-1]

#inc.triangle<-data.matrix(claims2, rownames.force = FALSE)



#on renomme ligne et colonnes
colnames(cum.triangle) <-dev 
rownames(cum.triangle)<-originf


colnames(inc.triangle) <-dev 
rownames(inc.triangle)<-originf

claims3<-claims2
claims3$cum.paid<-cum.triangle

#==========================================================

#le triangle des cumulés
#cum.triangle<-t(apply(inc.triangle, 1, cumsum))


 #le triangle des incrément
inc.triangle <- cum2incr(cum.triangle)
#incrément en vecteur
inc.paid=as.vector(t(inc.triangle))
inc.paid<-inc.paid[!is.na(inc.paid)]



# la diagonale
latest.paid<-cum.triangle[row(cum.triangle)==n-col(cum.triangle)+1]

#on ajoute les cumulés
#Claims$cum.paid<-cum.triangle[with(Claims, cbind(originf, dev))]



op<-par(fig=c(0,0.5,0,1),cex=0.8, oma=c(0,0,0,0))
with(Claims,{
     interaction.plot(x.factor=dev, trace.factor = originf, response=inc.paid,
                      fun=sum, type="b", bty='n', legend = FALSE); axis(1,at=1:n)
  par(fig=c(0.45,1,0,1),new=TRUE, cex=0.8, oma=c(0,0,0,0))
  interaction.plot(x.factor=dev, trace.factor = originf, response=cum.paid,
                   fun=sum, type="b", bty='n', legend = FALSE); axis(1,at=1:n)
     })
mtext("Incremental and cumulative claims development",
      side=3, outer=TRUE, line=-3, cex=1.1, font=2)
par(op)


xyplot(cum.paid~dev |originf, data=Claims, t="b", layout=c(4,2),
       as.table=TRUE, main="Cumulative claims development")


MCL<-MackChainLadder(cum.triangle)

plot(MCL, lattice=TRUE)

plot(MCL)

full.triangle<-MCL$FullTriangle

ultimate.paid<-full.triangle[,n]



#chain Ladder algorithm
f<-sapply((n-1):1, function(i){
  sum(cum.triangle[1:i, n-i+1])/sum(cum.triangle[1:i, n-i])
})

#si on considère qu'il n'y a pas de développement après l'année 14
tail<-1
f<-c(f,tail)

full.triangle2<-cum.triangle
for(k in 1:(n-1)){
  full.triangle2[(n-k+1):n,k+1]<-full.triangle2[(n-k+1):n,k]*f[k]
}

full.triangle2
test=full.triangle-full.triangle2
test

#loss development factor 
ldf<-rev(cumprod(rev(f)))
ldf
dev.pattern<-1/ldf

#le montant total des réserves
reserve1<-sum(latest.paid*(ldf-1))
reserve2=sum(ultimate.paid-latest.paid)
