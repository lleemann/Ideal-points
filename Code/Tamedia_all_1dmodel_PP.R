###############################################################
# IRT Tamedia Projekt -- Post-Processing
###############################################################
# GB und LL 
# 15.3.2018
###############################################################

rm(list=ls())
library(rstan)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)


setwd("~/Dropbox/IRT Tamedia/")
session <- 49
data1 <- read.csv(paste0("Data/votes_",session,"_legislatur.csv"))
topics <- c("FK", "WBK", "RK", "WAK", "UREK", "SiK", "SGK", "Bü", "SPK", "APK","KVF", "GPK")
topics <- topics[order(topics)]

## Re-run this code to get object "data3"
source("Code/identify_commission_2.R")



# read in all model results (across all issues)

stan.draws <- rep(list(NA),11)
mp.block.draws <- rep(list(NA),11)
number.of.votes <- rep(NA,length(topics))

result <- data.frame()
vote.result <- data.frame()
for (g in 1:length(topics)){
  if (topics[g] %in% c("Bü", "GPK")){
    next()
  }
  load(paste0("Objects/mps.", topics[g], ".", session, ".RData"))
  load(paste0("Objects/mp.block.", topics[g], ".", session,".RData"))
  load(paste0("Objects/stan.fit.", topics[g], ".", session,".RData"))
  load(paste0("Objects/vote.block.", topics[g], ".", session,".RData"))
  
  sum <- as.data.frame(summary(stan.fit)[[1]])
  stan.draws[[g]] <- stan.fit
  mp.block.draws[[g]] <- mp.block
  sum$par <- row.names(sum)
  sum$par <- gsub("\\[.*?\\]", "", sum$par)
  thetas <- filter(sum, par == "theta")
  thetas <- cbind(thetas, mp.block)
  thetas$kom <- topics[g]
  
  number.of.votes[g] <- dim(filter(sum, par == "alpha"))[1]
  alphas <- filter(sum, par == "alpha") %>% mutate(vote = vote_seq,
                                                   kom = topics[g])
  betas <- filter(sum, par == "beta") %>% mutate(vote = vote_seq,
                                                 kom = topics[g])

  save(alphas, file=paste("Objects/alphas.",topics[g],".", session,".RData",sep=""))
  save(betas, file=paste("Objects/betas.",topics[g],".", session,".RData",sep=""))

  vote.result <- bind_rows(vote.result, alphas, betas)
  result <- bind_rows(result, thetas)
}

head(result)



################################################################################################
################################################################################################
# Check convergence
# Inspecting the Rhat value across four chains - values should be below 1.1
max(result$Rhat)


max(vote.result$Rhat)

################################################################################################
################################################################################################
# VISUALIZATION

setwd("~/Dropbox/IRT Tamedia/")

if (session == 50){
parties <- c("BD", "C", "G", "GL", "RL", "S", "V")
partiesL <- c("BDP", "CVP", "GPS", "GLP", "FDP", "SPS", "SVP")
} else {
  parties <- c("BD", "CE", "G", "GL", "RL", "S", "V")
  partiesL <- c("BDP", "CVP", "GPS", "GLP", "FDP", "SPS", "SVP")
}
thetaNR <- matrix(NA,1000,12)


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

party.theta <- matrix(NA,3,7)
ff <- 13
#par(mar=c(5.1,6.1,4.1,2.1), xpd=TRUE, family="CMU Serif")
par(mar=c(5.1,6.1,4.1,2.1), xpd=TRUE)

plot(0,0, xlim=c(-(ff+2),ff), ylim=c(-.1,17), type="n", bty="n", ylab="", 
     xlab="",yaxt='n',xaxt='n')
axis(1,at = c(-10,-5,0,5,10))
adjL <- c(1:7)/10 + 18
counter <- 0
counter.vote <- 0
coL <- c(rgb(255,255,0,100,maxColorValue = 255),
         rgb(255,165,0,100,maxColorValue = 255),
         rgb(0,100,0,100,maxColorValue = 255),
         rgb(0,255,0,100,maxColorValue = 255),
         rgb(0,0,255,100,maxColorValue = 255),
         rgb(255,0,0,100,maxColorValue = 255),
         rgb(85,107,47,100,maxColorValue = 255))

coL.strong <- c(rgb(255,255,0,200,maxColorValue = 255),
         rgb(255,165,0,200,maxColorValue = 255),
         rgb(0,100,0,200,maxColorValue = 255),
         rgb(0,255,0,200,maxColorValue = 255),
         rgb(0,0,255,200,maxColorValue = 255),
         rgb(255,0,0,200,maxColorValue = 255),
         rgb(85,107,47,200,maxColorValue = 255))

# Loop over all areas
for (g in 1:length(topics)){
  if(topics[g]!="Bü" & topics[g]!="GPK"){
    #counter.vote <- counter.vote + 1  # used later to pick
    draws <- as.matrix(stan.draws[[g]])
    mpb <- mp.block.draws[[g]]
    n.leg <- dim(mpb)[1]
    NN <- dim(draws)[2]
    draws.theta <- draws[,c((NN-n.leg):(NN-1))]

    Nr.test <- t(apply(draws.theta,2,quantile, prob=c(0.025,0.5,0.975)))
    median.legislator.row <- order(Nr.test[,2])[floor(length(Nr.test[,2])/2)]
    Nr.median  <- Nr.test[median.legislator.row,]
    
    for (j in 1:7){
      xx <- apply(draws.theta[,which(mpb[,5]==parties[j])],MARGIN = 1,mean)
      #plot(density(xx))
      party.theta[,j] <-  sort(xx)[c(length(xx)*0.025,length(xx)*0.5,length(xx)*0.975)]
    }

    ### Normalization: SP at -10 and SVP at +10
    interceptL <-   -party.theta[2,6]  # now SP at exactly 0
    slopeL <- 20/(party.theta[2,7] - party.theta[2,6]) # factor by which we need to expand the difference between S and V to get to 20
    party.theta <- slopeL * (party.theta + interceptL) - 10
    
    # Determine Position of NR (as a whole)
    cL <- g + 7
    data4 <- data3[data3[,cL]==1,]
    data5 <- reshape(data4, idvar=c("CouncillorBioId","CouncillorName", "Geburtsdatum", "Kanton", "Fraktion", topics), timevar="VoteRegistrationNumber", direction="wide")
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
    save(votesNR, file=paste("Objects/votesNR.",topics[g], ".", session, ".RData",sep=""))
    
    # Position NR as a whole
    #load(paste("Objects/alphas.",topics[g],".RData",sep=""))
    #load(paste("Objects/betas.",topics[g],".RData",sep=""))
    
    #alpha.draws <- MASS::mvrnorm(1000, alphas[,1], diag(alphas[,3]))
    #beta.draws <- MASS::mvrnorm(1000, betas[,1], diag(betas[,3]))
    
    #startval <- 0
    
    #for (i in 1:1000){
    #  alphas1 <- alpha.draws[i,]
    #  betas1 <- beta.draws[i,]
    #  out <- optim(startval, theta.mle , alpha=alphas1, beta = betas1, hessian = TRUE, 
    #               method="BFGS", control=list(fnscale=-1,trace=10,REPORT=1, 
    #                                           maxit=300))
    #  thetaNR[i,g] <- out$par
    #  }
    #thetaNR[,g] <- slopeL*(thetaNR[,g]+interceptL) -10
    
    Nr.test <- slopeL*(Nr.test+interceptL) -10  
  
    rect(-13,mean(adjL) - 0.5,11, mean(adjL) + 0.5, col=rgb(190,190,190,50,maxColorValue = 255), border = "NA")
    
    for ( k in 1:length(parties)){
    counter <- counter + 1
    points(party.theta[2,k],adjL[k], col=coL.strong[k], pch=19, cex=1)
    segments(party.theta[1,k],adjL[k],party.theta[3,k],adjL[k], col=coL[k], lwd=4)
    }
    # add NR
    #points(mean(thetaNR[,g]),adjL[7],pch=19,col=rgb(190,190,190,200,maxColorValue = 255))
    #segments(quantile(thetaNR[,g],0.025),adjL[7],quantile(thetaNR[,g],0.975),adjL[7], col=rgb(190,190,190,100,maxColorValue = 255), lwd=4)
    text(-19,mean(adjL),paste(topics[g],"\n (# Abst:", number.of.votes[g],
                            ")"),pos = 4, cex=0.8)
    points(Nr.median[2],adjL[7],pch=19,col=rgb(190,190,190,200,maxColorValue = 255))
    segments(Nr.median[1],adjL[7],Nr.median[3],adjL[7], col=rgb(190,190,190,100,maxColorValue = 255), lwd=4)
    adjL <- adjL - 2
  
    output.numeric <- cbind(party.theta,Nr.median)
    colnames(output.numeric) <- c(parties,"Median")
    output.numeric <- round(output.numeric,2)
    write.csv(output.numeric, file=paste("Objects/output.numeric.",topics[g],".", session,".csv", sep=""))
}}

legend(-14, -1.9,legend=c(partiesL,"Median NR"), col=c(coL.strong,rgb(190,190,190,100,maxColorValue = 255)),pch=19, bty="n", horiz = TRUE)


colnames(party.theta) <- parties
party.theta

#topics <- rev(topics)


################################################################################################
################################################################################################
# DATA RESULT READ-OUT

# Transform the thetas in the oupt object


result <- subset(result, select = -c(se_mean,sd) )
colnames(result)[c(1,2,6)] <- c("Mittelwert", "CI (unten)", "CI (oben)")

result$Name <- NA
setwd("/Users/lleemann/Dropbox/IRT Tamedia/Report")

result.out <- data.frame(matrix(c(NA,NA,NA,NA,NA,NA),1,6))
colnames(result.out) <- c("Mittelwert", "CI (unten)", "CI (oben)", "Name", "Fraktion", "Kommission")

#partiesL <- c("BDP", "CVP", "GPS", "GLP", "FDP", "SVP", "SPS")
counter <- 0

for (g in 1:length(topics)){
  if(topics[g]!="Bü" & topics[g]!="GPK"){
    #counter.vote <- counter.vote + 1  # used later to pick
    draws <- as.matrix(stan.draws[[g]])
    mpb <- mp.block.draws[[g]]
    n.leg <- dim(mpb)[1]
    NN <- dim(draws)[2]
    draws.theta <- draws[,c((NN-n.leg):(NN-1))]
    
    for (j in 1:7){
      xx <- apply(draws.theta[,which(mpb[,5]==parties[j])],MARGIN = 1,mean)
      #plot(density(xx))
      party.theta[,j] <-  sort(xx)[c(length(xx)*0.025,length(xx)*0.5,length(xx)*0.975)]
    }
    
    ### Normalization: SP at -10 and SVP at +10
    interceptL <-   -party.theta[2,6]  # now SP at exactly 0
    slopeL <- 20/(party.theta[2,7] - party.theta[2,6]) # factor by which we need to expand the difference between S and V to get to 20
    party.theta <- slopeL * (party.theta + interceptL) - 10
    # function for rescaling
    rescaleL <- function(x)  slopeL * (x + interceptL) - 10    
    
    aa <- min(which(result$kom==topics[g]))
    bb <- max(which(result$kom==topics[g]))
    
    result[aa:bb,1:6] <- rescaleL(result[aa:bb,1:6])
    # sort by position (left to right)
    ccc <- result[aa:bb,] 
    result[aa:bb,] <- ccc[order(ccc[,1]),] 
    result$Name[aa:bb] <- iconv(as.character(result[aa:bb,11]))
    result$Name[aa:bb]  <- stringi::stri_replace_all_fixed(
      result$Name[aa:bb] , 
      c("ä", "ö", "ü", "Ä", "Ö", "Ü", "è", "é", "ë","î", "ç"), 
      c('\\"a', '\\"o', '\\"u', '\\"A', '\\"O', '\\"U',"\\`e","\\'e",'\\"e',"\\^i","\\c c"), 
      vectorize_all = FALSE
    )
    blockL <- result[aa:bb,c(1,2,6,28,14)]
    blockL$Kommission <- topics[g]
    result.out <- rbind(result.out, blockL)
    
    result$Fraktion1 <- recode(result$Fraktion, BD="BDP", C= "CVP", G="GPS", GL="GLP",RL="FDP",S="SPS",V="SVP")
    
    sink(paste("result.",topics[g], ".", session, ".tex",sep=""))
    print(xtable(result[aa:bb,c(1,2,6,28,29)]),tabular.environment = "longtable",floating=FALSE,
          include.rownames=FALSE, sanitize.text.function = identity, sanitize.colnames.function = identity)
    sink()
    

    
    # Plot per topic
    pdf(paste0("Plot",topics[g], ".", session, ".pdf"),width=12,height=8,paper='special') 
    block <- result[aa:bb,]
    nnn <- dim(block)[1]
    factorL <- 100
    y.pos <- factorL *c(1:nnn)/nnn
    plot(1, type="n", xlab="", ylab="", yaxt='n', bty="n", xlim=c(-20, 20), ylim=c(0, factorL))
    for ( k in 1:length(parties)){
      counter <- counter + 1
      rowsL <- which(block[,14]==parties[k])
      points(block[rowsL,1],  y.pos[rowsL],col=coL.strong[k], pch=19, cex=1)
      segments(block[rowsL,2],y.pos[rowsL],block[rowsL,6],y.pos[rowsL], col=coL[k], lwd=1.9)
    }
    #text(-12,0.9*factorL, paste0("Positionen im Bereich der ", topics[g]), font=2, cex=1.5, pos=4)
    legend(-12,0.9*factorL,legend=partiesL, col=coL.strong,pch=19, bty="n")
    dev.off()
    
  }}    

setwd("/Users/lleemann/Dropbox/IRT Tamedia/")

vote.result.original <- vote.result
vote.result <- select(vote.result.original, vote, par, kom, mean, `2.5%`, `97.5%`) %>% 
  rename(VoteRegistrationNumber = vote,
         Parameter = par,
         Kommission = kom,
         Mittelwert = mean,
         upr = `2.5%`,
         lwr = `97.5%`) %>% 
  mutate(VoteRegistrationNumber = substr(VoteRegistrationNumber, 6, nchar(VoteRegistrationNumber)))

vote.result <- merge(vote.result, (data1 %>% select(VoteRegistrationNumber, VoteSubmissionText) %>% distinct()))

vote.result <- mutate(vote.result,
                      Parameter = ifelse(Parameter == "alpha", "difficulty", 
                                         ifelse(Parameter == "beta", "discrimination", Parameter)),
                      VoteSubmissionText = as.character(VoteSubmissionText))

names(vote.result)[c(1,5:7)] <- c("Vote ID","CI (unten)", "CI (oben)", "Abstimmungstitel")

for (topic in unique(vote.result$Kommission)){
  temp_df <- filter(vote.result, Kommission == topic) %>% dplyr::arrange(Parameter, Mittelwert)
  temp_df[,7]  <- stringi::stri_replace_all_fixed(
    temp_df[,7], 
    c("ä", "ö", "ü", "Ä", "Ö", "Ü", "è", "é", "ë","î", "ç", "«","»","–"), 
    c('\\"a', '\\"o', '\\"u', '\\"A', '\\"O', '\\"U',"\\`e","\\'e",'\\"e',"\\^i","\\c c", '"', '"', "-"), 
    vectorize_all = FALSE
  )
  
  temp_df1 <- temp_df[,-3]
  
  sink(paste("Report/vote.result.disc.",topic, ".", session, ".tex",sep=""))
  print(xtable(temp_df1 %>% filter(Parameter == "discrimination") %>% select(-Parameter), align="lllllp{8cm}"), tabular.environment = "longtable", floating=FALSE,
        include.rownames=FALSE, sanitize.text.function = identity, sanitize.colnames.function = identity)
  sink()
  
  sink(paste("Report/vote.result.diff.",topic, ".", session, ".tex",sep=""))
  print(xtable(temp_df1 %>% filter(Parameter == "difficulty") %>% select(-Parameter), align="lllllp{8cm}"), tabular.environment = "longtable", floating=FALSE,
        include.rownames=FALSE, sanitize.text.function = identity, sanitize.colnames.function = identity)
  sink()

  
  temp_df1 <- temp_df1[,-c(4:5)]
  sink(paste("Report/vote.result.disc.top3.",topic, ".", session, ".tex",sep=""))
  print(xtable(temp_df1 %>% filter(Parameter == "discrimination" & Abstimmungstitel != "") %>% select(-Parameter) %>% top_n(3, abs(Mittelwert)), 
               align="lllp{9cm}"), tabular.environment = "longtable", floating=FALSE,
        include.rownames=FALSE, sanitize.text.function = identity, sanitize.colnames.function = identity)
  sink()

}

freq_com_vote <- data.frame()

for (topic in unique(vote.result$Kommission)){
  temp <- c(topic, sum(match_df[[topic]]), sum(match_df[[topic]][match_df$unique == 1]))
  names(temp) <- c("Kürzel", "Total", "Davon berücksichtigt")
  freq_com_vote <- bind_rows(freq_com_vote, temp)
}
freq_com_vote <- freq_com_vote %>% arrange(Kürzel) %>% 
  mutate(Kommission = c("Aussenpolitische Kommission", 
                        "Finanzkommission",
                        "Kommission für Verkehr und Fernmeldewesen",
                        "Kommission für Rechtsfragen",
                        "Kommission für soziale Sicherheit und Gesundheit",
                        "Sicherheitspolitische Kommission",
                        "Staatspolitische Kommission",
                        "Kommission für Umwelt, Raumplanung und Energie",
                        "Kommission für Wirtschaft und Abgaben",
                        "Kommission für Wissenschaft, Bildung und Kultur")) %>% select(Kommission, Kürzel, Total, `Davon berücksichtigt`)
freq_com_vote$Kommission  <- stringi::stri_replace_all_fixed(
  freq_com_vote$Kommission, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "è", "é", "ë","î", "ç", "«","»","–"), 
  c('\\"a', '\\"o', '\\"u', '\\"A', '\\"O', '\\"U',"\\`e","\\'e",'\\"e',"\\^i","\\c c", '"', '"', "-"), 
  vectorize_all = FALSE
)

colnames(freq_com_vote)  <- stringi::stri_replace_all_fixed(
  colnames(freq_com_vote), 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "è", "é", "ë","î", "ç", "«","»","–"), 
  c('\\"a', '\\"o', '\\"u', '\\"A', '\\"O', '\\"U',"\\`e","\\'e",'\\"e',"\\^i","\\c c", '"', '"', "-"), 
  vectorize_all = FALSE
)

sink(paste("Report/vote.freq.by.com.tex",sep=""))
print(xtable(freq_com_vote, align="p{4cm}lllp{2cm}"), tabular.environment = "longtable", floating=FALSE,
      include.rownames=FALSE, sanitize.text.function = identity, sanitize.colnames.function = identity)
sink()

result.out <- result.out[-1,]
setwd("~/Dropbox/IRT Tamedia/")
write.csv(result.out,file=paste0("Data/write.out.1", ".", session, ".csv"))
write.csv(vote.result,file=paste0("Data/write.out.votes", ".", session, ".csv"))
## Latex write-out for document


match_new <- filter_at(match_df, vars(FK:GPK), any_vars(. == 1))
nrow(match_new)
match_new <- filter(match_new, unique == 1)
nrow(match_new)
match_new <- filter(match_new, GPK != 1)
nrow(match_new)
match_new <- filter(match_new, `16.016` != 1)
nrow(match_new)
#match_new <- filter(match_new, `BÃ¼` != 1)
#match_new
