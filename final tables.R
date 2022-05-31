################################################################################
#### Title: Produce tables 
#### Author: Tra T. Le
#### Last modified: May 31, 2022

#example codes for 1 condition
#modify to use for other conditions

#bias ATE, bias SE and SD averaged across all conditions
library(rio)
library(xtable) #generate tables for LaTeX
ATEB1 <- finalATE[finalATE$treatment == 1,]
ATEB2 <- finalATE[finalATE$treatment == 2,]
ATEB3 <- finalATE[finalATE$treatment == 3,]

SEB1 <- finalSE[finalSE$treatment == 1,]
SEB2 <- finalSE[finalSE$treatment == 2,]
SEB3 <- finalSE[finalSE$treatment == 3,]

SDB1 <- finalSD[finalSD$treatment == 1,]
SDB2 <- finalSD[finalSD$treatment == 2,]
SDB3 <- finalSD[finalSD$treatment == 3,]

ATEB1_mean <- aggregATE(ATEB1[,c(2,3)], list(ATEB1$method), FUN=mean)
ATEB2_mean <- aggregATE(ATEB2[,c(2,3)], list(ATEB2$method), FUN=mean)
ATEB3_mean <- aggregATE(ATEB3[,c(2,3)], list(ATEB3$method), FUN=mean)

SEB1_mean <- aggregATE(SEB1[,c(2,3)], list(SEB1$method), FUN=mean)
SEB2_mean <- aggregATE(SEB2[,c(2,3)], list(SEB2$method), FUN=mean)
SEB3_mean <- aggregATE(SEB3[,c(2,3)], list(SEB3$method), FUN=mean)

SDB1_mean <- aggregSD(SDB1[,c(2,3)], list(SDB1$method), FUN=mean)
SDB2_mean <- aggregSD(SDB2[,c(2,3)], list(SDB2$method), FUN=mean)
SDB3_mean <- aggregSD(SDB3[,c(2,3)], list(SDB3$method), FUN=mean)

table1 <- cbind(ATEB1_mean[,c(1,2)], SEB1_mean[,2], SDB1_mean[,2],
                ATEB2_mean[,c(2)], SEB2_mean[,2], SDB2_mean[,2],
                ATEB3_mean[,c(2)], SEB3_mean[,2], SDB3_mean[,2])

table2 <- cbind(ATEB1_mean[,c(1,2)], SEB1_mean[,3], SDB1_mean[,3],
                ATEB2_mean[,3], SEB2_mean[,3], SDB2_mean[,3],
                ATEB3_mean[,3], SEB3_mean[,3], SDB3_mean[,3])

colnames(table1) <- c("method", "estimSDd SD", "SE", "SD","estimSDd SD", "SE", "SD","estimSDd SD", "SE", "SD")
colnames(table2) <- c("method", "estimSDd SD", "SE", "SD","estimSDd SD", "SE", "SD","estimSDd SD", "SE", "SD")
table1 <- format(table1, digits = 2)
table2 <- format(table2, digits = 2)
write.csv(table1, "Average across all conditions class 2.csv",row.names = F)
write.csv(table2, "Average across all conditions class 3.csv",row.names = F)

#bias ATE across N and G
biasATEG1N500 <- finalATE[finalATE$sample == 500 & finalATE$confounding==1,c(1,9,10)]
biasATEG2N500 <- finalATE[finalATE$sample == 500 & finalATE$confounding==2,c(1,9,10)]
biasATEG3N500 <- finalATE[finalATE$sample == 500 & finalATE$confounding==3,c(1,9,10)]
meanATEG1N500 <- aggregate(biasATEG1N500[,c(2,3)], list(biasATEG1N500$method), FUN=mean)
meanATEG2N500 <- aggregate(biasATEG2N500[,c(2,3)], list(biasATEG2N500$method), FUN=mean)
meanATEG3N500 <- aggregate(biasATEG3N500[,c(2,3)], list(biasATEG3N500$method), FUN=mean)

biasATEG1N1000 <- finalATE[finalATE$sample == 1000 & finalATE$confounding==1,c(1,9,10)]
biasATEG2N1000 <- finalATE[finalATE$sample == 1000 & finalATE$confounding==2,c(1,9,10)]
biasATEG3N1000 <- finalATE[finalATE$sample == 1000 & finalATE$confounding==3,c(1,9,10)]
meanATEG1N1000 <- aggregate(biasATEG1N1000[,c(2,3)], list(biasATEG1N1000$method), FUN=mean)
meanATEG2N1000 <- aggregate(biasATEG2N1000[,c(2,3)], list(biasATEG2N1000$method), FUN=mean)
meanATEG3N1000 <- aggregate(biasATEG3N1000[,c(2,3)], list(biasATEG3N1000$method), FUN=mean)

biasATEG1N2500 <- finalATE[finalATE$sample == 2500 & finalATE$confounding==1,c(1,9,10)]
biasATEG2N2500 <- finalATE[finalATE$sample == 2500 & finalATE$confounding==2,c(1,9,10)]
biasATEG3N2500 <- finalATE[finalATE$sample == 2500 & finalATE$confounding==3,c(1,9,10)]
meanATEG1N2500 <- aggregate(biasATEG1N2500[,c(2,3)], list(biasATEG1N2500$method), FUN=mean)
meanATEG2N2500 <- aggregate(biasATEG2N2500[,c(2,3)], list(biasATEG2N2500$method), FUN=mean)
meanATEG3N2500 <- aggregate(biasATEG3N2500[,c(2,3)], list(biasATEG3N2500$method), FUN=mean)

table6_2 <- cbind(meanATEG1N500[,c(1,2)], meanATEG2N500[,2],meanATEG3N500[,2],
                  meanATEG1N1000[,2], meanATEG2N1000[,2],meanATEG3N1000[,2],
                  meanATEG1N2500[,2], meanATEG2N2500[,2],meanATEG3N2500[,2])
table6_3 <- cbind(meanATEG1N500[,c(1,3)], meanATEG2N500[,3],meanATEG3N500[,3],
                  meanATEG1N1000[,3], meanATEG2N1000[,3],meanATEG3N1000[,3],
                  meanATEG1N2500[,3], meanATEG2N2500[,3],meanATEG3N2500[,3])
colnames(table6_2) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
colnames(table6_3) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
table6 <- rbind(table6_2,table6_3)
write.csv(table6, "bias ATE across N and G (table6).csv")
xtable(table6_3, digits = 3)

#bias SE across N and G
biasSEG1N500 <- finalSE[finalSE$sample == 500 & finalSE$confounding==1,c(1,7,8)]
biasSEG2N500 <- finalSE[finalSE$sample == 500 & finalSE$confounding==2,c(1,7,8)]
biasSEG3N500 <- finalSE[finalSE$sample == 500 & finalSE$confounding==3,c(1,7,8)]
meanSEG1N500 <- aggregate(biasSEG1N500[,c(2,3)], list(biasSEG1N500$method), FUN=mean)
meanSEG2N500 <- aggregate(biasSEG2N500[,c(2,3)], list(biasSEG2N500$method), FUN=mean)
meanSEG3N500 <- aggregate(biasSEG3N500[,c(2,3)], list(biasSEG3N500$method), FUN=mean)

biasSEG1N1000 <- finalSE[finalSE$sample == 1000 & finalSE$confounding==1,c(1,7,8)]
biasSEG2N1000 <- finalSE[finalSE$sample == 1000 & finalSE$confounding==2,c(1,7,8)]
biasSEG3N1000 <- finalSE[finalSE$sample == 1000 & finalSE$confounding==3,c(1,7,8)]
meanSEG1N1000 <- aggregate(biasSEG1N1000[,c(2,3)], list(biasSEG1N1000$method), FUN=mean)
meanSEG2N1000 <- aggregate(biasSEG2N1000[,c(2,3)], list(biasSEG2N1000$method), FUN=mean)
meanSEG3N1000 <- aggregate(biasSEG3N1000[,c(2,3)], list(biasSEG3N1000$method), FUN=mean)

biasSEG1N2500 <- finalSE[finalSE$sample == 2500 & finalSE$confounding==1,c(1,7,8)]
biasSEG2N2500 <- finalSE[finalSE$sample == 2500 & finalSE$confounding==2,c(1,7,8)]
biasSEG3N2500 <- finalSE[finalSE$sample == 2500 & finalSE$confounding==3,c(1,7,8)]
meanSEG1N2500 <- aggregate(biasSEG1N2500[,c(2,3)], list(biasSEG1N2500$method), FUN=mean)
meanSEG2N2500 <- aggregate(biasSEG2N2500[,c(2,3)], list(biasSEG2N2500$method), FUN=mean)
meanSEG3N2500 <- aggregate(biasSEG3N2500[,c(2,3)], list(biasSEG3N2500$method), FUN=mean)

table7_2 <- cbind(meanSEG1N500[,c(1,2)], meanSEG2N500[,2],meanSEG3N500[,2],
                  meanSEG1N1000[,2], meanSEG2N1000[,2],meanSEG3N1000[,2],
                  meanSEG1N2500[,2], meanSEG2N2500[,2],meanSEG3N2500[,2])
table7_3 <- cbind(meanSEG1N500[,c(1,3)], meanSEG2N500[,3],meanSEG3N500[,3],
                  meanSEG1N1000[,3], meanSEG2N1000[,3],meanSEG3N1000[,3],
                  meanSEG1N2500[,3], meanSEG2N2500[,3],meanSEG3N2500[,3])
colnames(table7_2) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
colnames(table7_3) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
table7 <- rbind(table7_2,table7_3)
write.csv(table7, "bias SE across N and G (table7).csv")
xtable(table7_3, digits = 3)

#bias SD across G and N
biasSDG1N500 <- finalSD[finalSD$sample == 500 & finalSD$confounding==1,c(1:3)]
biasSDG2N500 <- finalSD[finalSD$sample == 500 & finalSD$confounding==2,c(1:3)]
biasSDG3N500 <- finalSD[finalSD$sample == 500 & finalSD$confounding==3,c(1:3)]
meanSDG1N500 <- aggregate(biasSDG1N500[,c(2,3)], list(biasSDG1N500$method), FUN=mean)
meanSDG2N500 <- aggregate(biasSDG2N500[,c(2,3)], list(biasSDG2N500$method), FUN=mean)
meanSDG3N500 <- aggregate(biasSDG3N500[,c(2,3)], list(biasSDG3N500$method), FUN=mean)

biasSDG1N1000 <- finalSD[finalSD$sample == 1000 & finalSD$confounding==1,c(1:3)]
biasSDG2N1000 <- finalSD[finalSD$sample == 1000 & finalSD$confounding==2,c(1:3)]
biasSDG3N1000 <- finalSD[finalSD$sample == 1000 & finalSD$confounding==3,c(1:3)]
meanSDG1N1000 <- aggregate(biasSDG1N1000[,c(2,3)], list(biasSDG1N1000$method), FUN=mean)
meanSDG2N1000 <- aggregate(biasSDG2N1000[,c(2,3)], list(biasSDG2N1000$method), FUN=mean)
meanSDG3N1000 <- aggregate(biasSDG3N1000[,c(2,3)], list(biasSDG3N1000$method), FUN=mean)

biasSDG1N2500 <- finalSD[finalSD$sample == 2500 & finalSD$confounding==1,c(1:3)]
biasSDG2N2500 <- finalSD[finalSD$sample == 2500 & finalSD$confounding==2,c(1:3)]
biasSDG3N2500 <- finalSD[finalSD$sample == 2500 & finalSD$confounding==3,c(1:3)]
meanSDG1N2500 <- aggregate(biasSDG1N2500[,c(2,3)], list(biasSDG1N2500$method), FUN=mean)
meanSDG2N2500 <- aggregate(biasSDG2N2500[,c(2,3)], list(biasSDG2N2500$method), FUN=mean)
meanSDG3N2500 <- aggregate(biasSDG3N2500[,c(2,3)], list(biasSDG3N2500$method), FUN=mean)

table8_2 <- cbind(meanSDG1N500[,c(1,2)], meanSDG2N500[,2],meanSDG3N500[,2],
                  meanSDG1N1000[,2], meanSDG2N1000[,2],meanSDG3N1000[,2],
                  meanSDG1N2500[,2], meanSDG2N2500[,2],meanSDG3N2500[,2])
table8_3 <- cbind(meanSDG1N500[,c(1,3)], meanSDG2N500[,3],meanSDG3N500[,3],
                  meanSDG1N1000[,3], meanSDG2N1000[,3],meanSDG3N1000[,3],
                  meanSDG1N2500[,3], meanSDG2N2500[,3],meanSDG3N2500[,3])
colnames(table8_2) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
colnames(table8_3) <- c("method","bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3",
                        "bias G1", "bias G2", "bias G3")
table8 <- rbind(table8_2,table8_3)
write.csv(table8, "bias SD across N and G (table8).csv")
xtable(table8_3, digits = 3)
