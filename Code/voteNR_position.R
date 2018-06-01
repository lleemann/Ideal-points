rm(list=ls())
library(rstan)
#library(MASS)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)


setwd("~/Dropbox/IRT Tamedia/")
data1 <- read.csv("Data/votes_50_legislatur.csv")


## get different Topics - Garret's script
# reads in raw data
# cleans votes that have only been treated by one commission
# reshapes data to be fit for further use
source("Code/identify_commission_2.R")


YN.vote.NR <- function(x){
  Y.share <- mean(x, na.rm= TRUE)
  output <- 1
  if (Y.share<.5) output <- 0
  return(output)
}

theta.mle <- function(theta, alpha, beta){
  p <- 1/(1+exp(-alphas1 - betas1*theta))
  PP <- sum(log(p))
  return(PP)
}

topics <- c("FK", "WBK", "RK", "WAK", "UREK", "SiK", "SGK", "Bü", "SPK", "APK","KVF", "GPK")
topics <- topics[order(topics)]

thetaNR <- matrix(NA,1000,12)

library(MASS)
for (g in 1:length(topics)){
  # Themen rausnehmen mit 5 und 2 Votes 
  if(topics[g]!="Bü" & topics[g]!="GPK"){
    cL <- g + 7
    data4 <- data3[data3[,cL]==1,]
    data5 <- reshape(data4, idvar=c("CouncillorBioId","CouncillorName", "Geburtsdatum", "Kanton", "Fraktion","FK", "WBK", "Bü", "RK", "WAK", "UREK", "SiK", "SGK",  "SPK", "APK","KVF", "GPK"), timevar="VoteRegistrationNumber", direction="wide")
    mp.block <- data5[,c(1:17)]
    votesL <- data5[,-c(1:17)]
    c <- dim(votesL)[2] 
    #number.of.votes[g] <- c
    for (i in 1:c){
      votesL[,i] <- as.character(votesL[,i])  
    }
    votesL[votesL=="Ja"] <- 1
    votesL[votesL=="Nein"] <- 0
    votesL[votesL=="Hat nicht teilgenommen"] <- NA
    votesL[votesL=="Der Präsident stimmt nicht"] <- NA
    votesL[votesL=="Enthaltung"] <- NA
    votesL[votesL=="Entschuldigt"] <- NA
    
    anz.parl <- length(unique(mp.block$CouncillorName))  
    c <- dim(votesL)[2]
    for (i in 1:c){
      votesL[,i] <- as.numeric(votesL[,i])  
    }
    
    votesNR <- apply(votesL,2,YN.vote.NR)
    save(votesNR, file=paste("Objects/votesNR.",topics[g],".RData",sep=""))
    
    # Position NR as a whole
    load(paste("Objects/alphas.",topics[g],".RData",sep=""))
    load(paste("Objects/betas.",topics[g],".RData",sep=""))
    
    alpha.draws <- mvrnorm(1000, alphas[,1], diag(alphas[,3]))
    beta.draws <- mvrnorm(1000, betas[,1], diag(betas[,3]))
    
    startval <- 0
    
    for (i in 1:1000){
      alphas1 <- alpha.draws[i,]
      betas1 <- beta.draws[i,]
      out <- optim(startval, theta.mle , alpha=alphas1, beta = betas1, hessian = TRUE, 
                   method="BFGS", control=list(fnscale=-1,trace=10,REPORT=1, 
                                               maxit=300))
      thetaNR[i,g] <- out$par
    }
  }
}

par(mfrow=c(5,2))
hist(thetaNR[,1])
hist(thetaNR[,3])
hist(thetaNR[,5])
hist(thetaNR[,6])
hist(thetaNR[,7])
hist(thetaNR[,8])
hist(thetaNR[,9])
hist(thetaNR[,10])
hist(thetaNR[,11])
hist(thetaNR[,12])