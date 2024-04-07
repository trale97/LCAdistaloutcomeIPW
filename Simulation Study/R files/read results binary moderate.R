################################################################################
#### Title: Read results from LatentGOLD for binary Z and moderate separation
#### Author: Tra T. Le
#### Last modified: May 31, 2022

rm(list=ls())
setwd("~/binary moderate")
library(dplyr)
ATEN500G1 <- data.frame()
SEN500G1 <- data.frame()
SDN500G1 <- data.frame()
B <- c(1,2,3)
G <- 1
N <- 500
#sample 500 and G 1
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN500G1 <- rbind(ATEN500G1, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN500G1 <- rbind(SEN500G1,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN500G1 <- rbind(SDN500G1, sd)
}

#sample 500 and G 2
ATEN500G2 <- data.frame()
SEN500G2 <- data.frame()
SDN500G2 <- data.frame()
G <-2
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN500G2 <- rbind(ATEN500G2, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN500G2 <- rbind(SEN500G2,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN500G2 <- rbind(SDN500G2, sd)
}

#sample 500 and G 3
ATEN500G3 <- data.frame()
SEN500G3 <- data.frame()
SDN500G3 <- data.frame()
G <-3
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN500G3 <- rbind(ATEN500G3, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN500G3 <- rbind(SEN500G3,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN500G3 <- rbind(SDN500G3, sd)
}

#sample 1000 and G 1
G <- 1
N <- 1000
ATEN1000G1 <- data.frame()
SEN1000G1 <- data.frame()
SDN1000G1 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN1000G1 <- rbind(ATEN1000G1, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN1000G1 <- rbind(SEN1000G1,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN1000G1 <- rbind(SDN1000G1, sd)
}

#sample 1000 and G 2
G <- 2
ATEN1000G2 <- data.frame()
SEN1000G2 <- data.frame()
SDN1000G2 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN1000G2 <- rbind(ATEN1000G2, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN1000G2 <- rbind(SEN1000G2,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN1000G2 <- rbind(SDN1000G2, sd)
}

#sample 1000 and G 3
G <- 3
ATEN1000G3 <- data.frame()
SEN1000G3 <- data.frame()
SDN1000G3 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN1000G3 <- rbind(ATEN1000G3, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN1000G3 <- rbind(SEN1000G3,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN1000G3 <- rbind(SDN1000G3, sd)
}

#sample 2500 and G 1
G <- 1
N <- 2500
ATEN2500G1 <- data.frame()
SEN2500G1 <- data.frame()
SDN2500G1 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN2500G1 <- rbind(ATEN2500G1, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN2500G1 <- rbind(SEN2500G1,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN2500G1 <- rbind(SDN2500G1, sd)
}

#sample 2500 and G2
G <- 2
ATEN2500G2 <- data.frame()
SEN2500G2 <- data.frame()
SDN2500G2 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN2500G2 <- rbind(ATEN2500G2, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN2500G2 <- rbind(SEN2500G2,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN2500G2 <- rbind(SDN2500G2, sd)
}

#sample 2500 and G3
G <- 3
ATEN2500G3 <- data.frame()
SEN2500G3 <- data.frame()
SDN2500G3 <- data.frame()
for(b in B) {
  results <- read.csv(paste0("resultsbiB",b, "G",G, "N",N, ".csv"), header = FALSE)
  margeff <- results[results$V4 ==5, ]
  means <- aggregate(margeff[,c(7,8)], list(margeff$V1), FUN=mean)
  means$treatment <- rep(b,7)
  ATEN2500G3 <- rbind(ATEN2500G3, means)
  margeff_SE <- results[results$V4 ==9, ]
  means_SE <- aggregate(margeff_SE[, c(7,8)], list(margeff_SE$V1), FUN=mean)
  means_SE$treatment <- rep(b,7)
  SEN2500G3 <- rbind(SEN2500G3,means_SE)
  sd <- aggregate(margeff[, c(7,8)], list(margeff$V1), FUN=sd)
  sd$treatment <- rep(b,7)
  SDN2500G3 <- rbind(SDN2500G3, sd)
}

#combind in 3 big data sets
finalATE <- rbind(ATEN500G1,ATEN500G2,ATEN500G3,ATEN1000G1,ATEN1000G2, ATEN1000G3,ATEN2500G1,ATEN2500G2,ATEN2500G3)
finalSE <- rbind(SEN500G1,SEN500G2,SEN500G3,SEN1000G1,SEN1000G2, SEN1000G3,SEN2500G1,SEN2500G2,SEN2500G3)
finalSD <- rbind(SDN500G1,SDN500G2,SDN500G3,SDN1000G1,SDN1000G2, SDN1000G3,SDN2500G1,SDN2500G2,SDN2500G3)

confounding <- data.frame(gamma = c(1,2,3))
confounding <- confounding %>% slice(rep(1:n(), each = 21))
confounding <- confounding %>% slice(rep(1:n(), 3))

sample <- data.frame(size = c(500,1000,2500))
sample <- sample %>% slice(rep(1:n(), each = 63))

finalATE <- cbind(finalATE, confounding, sample)
finalSE <- cbind(finalSE, confounding, sample)
finalSD <- cbind(finalSD, confounding, sample)

#bias of ATE
true <- data.frame(V1 = c(0.1637,0.1637, 0.1637), V2 = c(0.1637,0.3036, .4021))
true <- true %>% slice(rep(1:n(), each = 7))
true <- true %>% slice(rep(1:n(),9))
bias1 <- finalATE[2]-true[1]
bias2 <- finalATE[3]-true[2]
finalATE <- cbind(finalATE, true, bias1, bias2)
colnames(finalATE) <- c("method","cluster2", "cluster3","treatment", "confounding", "sample","true ES1","true ES2", "bias_cluster2", "bias_cluster3")


#bias of SE
biasSE1 <- finalSE[2]-finalSD[2]
biasSE2 <- finalSE[3]-finalSD[3]
finalSE <- cbind(finalSE, biasSE1, biasSE2)
colnames(finalSE) <- c("method","cluster2", "cluster3","treatment", "confounding", "sample", "bias_cluster2", "bias_cluster3")

#SD
colnames(finalSD) <- c("method","cluster2", "cluster3","treatment", "confounding", "sample")


