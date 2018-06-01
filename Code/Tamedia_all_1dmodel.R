###############################################################
# IRT Tamedia Projekt -- Model Estimation
###############################################################
# GB und LL 
# 16.2.2018
###############################################################

rm(list=ls())
library(rstan)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Dropbox/IRT Tamedia/")
session <- 49
topics <- c("FK", "WBK", "RK", "WAK", "UREK", "SiK", "SGK", "Bü", "SPK", "APK","KVF", "GPK")

data1 <- read.csv(paste0("Data/votes_", session, "_legislatur.csv"))

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


## get different Topics - Garret's script
# reads in raw data
# cleans votes that have only been treated by one commission
# reshapes data to be fit for further use
# session 49: 13.092-N - 13, APK (15.2006) - 1, 12.008 - 98. Fraktion '-' - Poggia Mauro
source("Code/identify_commission_2.R")




#number.of.votes <- rep(NA,length(topics))


for (g in 1:length(topics)){
  # Themen rausnehmen mit 5 und 2 Votes 
  if(topics[g]!="Bü" & topics[g]!="GPK"){
    cL <- g + 7
    data4 <- data3[data3[,cL]==1,]
    data5 <- reshape(data4, idvar = c("CouncillorBioId","CouncillorName", "Geburtsdatum", "Kanton", "Fraktion",
                                    topics), 
                     timevar="VoteRegistrationNumber", direction="wide")
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

    
    # take out NA for Stan:
    vote_seq <- names(votesL)
    votesL <- unlist(votesL)
    nas <- which(is.na(votesL))
    votes <- votesL[-nas]
    
    N <- length(votes)
    j <- rep(1:anz.parl, times = c)
    j <- j[-nas]
    k <- rep(1:c, each = anz.parl)
    k <- k[-nas]
    J <- max(j)
    K <- max(k)
    hSP <- which(mp.block$CouncillorName =="Nordmann Roger")
    hSVP <- which(mp.block$CouncillorName =="Aeschi Thomas")
    mps <- unique(j)
    
    wak_data <- list(N = N, K = K, J = J, j = j, k = k, y = votes, hSP=hSP, hSVP=hSVP)
    
    party <- ifelse(data5$Fraktion == "S", -1,
                    ifelse(data5$Fraktion == "V", 1, 0))
    initF <- function() {
      list(theta = party)
    }
    
    
    start.irt <- Sys.time()
    stan.fit <- stan(file = "Code/1d_model_L.stan",
                     data = wak_data, iter = 800, warmup = 400, chains = 4,
                     init = initF, verbose = TRUE, cores = 4, seed = 1234)
    end.irt <- Sys.time()


    save(stan.fit, file=paste("Objects/stan.fit.",topics[g],".", session, ".RData",sep=""))
    save(mps, file=paste("Objects/mps.",topics[g],".", session, ".RData",sep=""))
    save(mp.block, file=paste("Objects/mp.block.",topics[g],".", session, ".RData",sep=""))
    save(vote_seq, file=paste("Objects/vote.block.",topics[g],".", session, ".RData",sep=""))
    
    print(paste("Finished the ",g,"th run (topic: ",topics[g],")",sep=""))  
    
  }
}




