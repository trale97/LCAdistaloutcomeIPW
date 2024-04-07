###### UNEQUAL CLASS SIZE #####
rm(list = ls())
library(ggplot2)
library(ggsci)
library(ggrepel)
#library(ggtext)
library(ggpubr)
library(grid)
theme_set(theme_pubr())

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

# load bias of ATE for binary outcome
load("biasATE_extreme_binary.RData")
load("biasATE_moderate_binary.RData")
# combine in one dataframe 
biasATE_bi <- rbind(finalATE_extreme_binary, finalATE_moderate_binary)
biasATE_bi$class_diff <- c(rep("extreme",45), rep("moderate", 45))

biasATE_bi[biasATE_bi == "bray"] <- "Bray et al. (2019)"
biasATE_bi[biasATE_bi == "tra"] <- "New 3-Step IPW"
biasATE_bi[biasATE_bi == "schuler"] <- "Schuler et al. (2014)"
biasATE_bi[biasATE_bi == "yamaguchi"] <- "Yamaguchi (2015)"
biasATE_bi[biasATE_bi == "propensity"] <- "New PS as Covariate"

biasATE_bi[,c("method","class_diff")]= lapply(biasATE_bi[,c("method","class_diff")],
                                              as.factor)

biasATE_bi$method2 <- ""
biasATE_bi$method2[biasATE_bi$confounding==3 & biasATE_bi$sample == 2500 & biasATE_bi$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
biasATE_bi$method2[biasATE_bi$confounding==3 & biasATE_bi$sample == 2500 & biasATE_bi$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
biasATE_bi$method2[biasATE_bi$confounding==3 & biasATE_bi$sample == 2500 & biasATE_bi$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
biasATE_bi$method2[biasATE_bi$confounding==3 & biasATE_bi$sample == 2500 & biasATE_bi$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
biasATE_bi$method2[biasATE_bi$confounding==3 & biasATE_bi$sample == 2500 & biasATE_bi$method=="New PS as Covariate"] <- "New PS as Covariate  "

# bias of the ATE 
p1 <- ggplot(data = biasATE_bi[biasATE_bi$sample == 500 | biasATE_bi$sample == 1000, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.18,0.085))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                       class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = biasATE_bi[biasATE_bi$sample == 2500, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.18,0.085))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                       class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA9.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

### LOAD BIAS OF ATE FOR CONTINUOUS OUTCOME
rm(list = ls())
load("biasATE_extreme_cont.RData")
load("biasATE_moderate_cont.RData")

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

biasATE_cont <- rbind(finalATE_extreme_cont, finalATE_moderate_cont)
biasATE_cont$class_diff <- c(rep("extreme",45), rep("moderate", 45))

biasATE_cont[biasATE_cont == "bray"] <- "Bray et al. (2019)"
biasATE_cont[biasATE_cont == "tra"] <- "New 3-Step IPW"
biasATE_cont[biasATE_cont == "schuler"] <- "Schuler et al. (2014)"
biasATE_cont[biasATE_cont == "yamaguchi"] <- "Yamaguchi (2015)"
biasATE_cont[biasATE_cont == "propensity"] <- "New PS as Covariate"

biasATE_cont[,c("method","class_diff")]= lapply(biasATE_cont[,c("method","class_diff")],
                                                as.factor)

biasATE_cont$method2 <- ""
biasATE_cont$method2[biasATE_cont$confounding==3 & biasATE_cont$sample == 2500 & biasATE_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
biasATE_cont$method2[biasATE_cont$confounding==3 & biasATE_cont$sample == 2500 & biasATE_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
biasATE_cont$method2[biasATE_cont$confounding==3 & biasATE_cont$sample == 2500 & biasATE_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
biasATE_cont$method2[biasATE_cont$confounding==3 & biasATE_cont$sample == 2500 & biasATE_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
biasATE_cont$method2[biasATE_cont$confounding==3 & biasATE_cont$sample == 2500 & biasATE_cont$method=="New PS as Covariate"] <- "New PS as Covariate  "

p1 <- ggplot(data = biasATE_cont[biasATE_cont$sample == 500 | biasATE_cont$sample == 1000, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.5,0.45))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = biasATE_cont[biasATE_cont$sample == 2500, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.5,0.45))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA10.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

### LOAD SD OF ATE FOR BINARY OUTCOME
rm(list = ls())
load("SD_extreme_binary.RData")
load("SD_moderate_binary.RData")

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

SD_bi <- rbind(finalSD_extreme_binary, finalSD_moderate_binary)
colnames(SD_bi) <- c("method", "cluster2", "cluster3", "confounding", "treatment", "sample")
SD_bi$class_diff <- c(rep("extreme",45), rep("moderate", 45))

SD_bi[SD_bi == "bray"] <- "Bray et al. (2019)"
SD_bi[SD_bi == "tra"] <- "New 3-Step IPW"
SD_bi[SD_bi == "schuler"] <- "Schuler et al. (2014)"
SD_bi[SD_bi == "yamaguchi"] <- "Yamaguchi (2015)"
SD_bi[SD_bi == "propensity"] <- "New PS as Covariate"

SD_bi[,c("method","class_diff")]= lapply(SD_bi[,c("method","class_diff")],
                                                as.factor)

SD_bi$method2 <- ""
SD_bi$method2[SD_bi$confounding==3 & SD_bi$sample == 2500 & SD_bi$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
SD_bi$method2[SD_bi$confounding==3 & SD_bi$sample == 2500 & SD_bi$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SD_bi$method2[SD_bi$confounding==3 & SD_bi$sample == 2500 & SD_bi$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
SD_bi$method2[SD_bi$confounding==3 & SD_bi$sample == 2500 & SD_bi$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SD_bi$method2[SD_bi$confounding==3 & SD_bi$sample == 2500 & SD_bi$method=="New PS as Covariate"] <- "New PS as Covariate  "

p1 <- ggplot(data = SD_bi[SD_bi$sample == 500 | SD_bi$sample == 1000, ], aes(x=confounding, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0.023,0.28))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SD_bi[SD_bi$sample == 2500, ], aes(x=confounding, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0.023,0.28))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA11.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

### LOAD SD OF ATE FOR CONTINUOUS OUTCOME
rm(list = ls())
load("SD_extreme_cont.RData")
load("SD_moderate_cont.RData")

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

SD_cont <- rbind(finalSD_extreme_cont, finalSD_moderate_cont)
colnames(SD_cont) <- c("method", "cluster2", "cluster3", "confounding", "treatment", "sample")
SD_cont$class_diff <- c(rep("extreme",45), rep("moderate", 45))

SD_cont[SD_cont == "bray"] <- "Bray et al. (2019)"
SD_cont[SD_cont == "tra"] <- "New 3-Step IPW"
SD_cont[SD_cont == "schuler"] <- "Schuler et al. (2014)"
SD_cont[SD_cont == "yamaguchi"] <- "Yamaguchi (2015)"
SD_cont[SD_cont == "propensity"] <- "New PS as Covariate"

SD_cont[,c("method","class_diff")]= lapply(SD_cont[,c("method","class_diff")],
                                         as.factor)

SD_cont$method2 <- ""
SD_cont$method2[SD_cont$confounding==3 & SD_cont$sample == 2500 & SD_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
SD_cont$method2[SD_cont$confounding==3 & SD_cont$sample == 2500 & SD_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SD_cont$method2[SD_cont$confounding==3 & SD_cont$sample == 2500 & SD_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
SD_cont$method2[SD_cont$confounding==3 & SD_cont$sample == 2500 & SD_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SD_cont$method2[SD_cont$confounding==3 & SD_cont$sample == 2500 & SD_cont$method=="New PS as Covariate"] <- "New PS as Covariate  "

p1 <- ggplot(data = SD_cont[SD_cont$sample == 500 | SD_cont$sample == 1000, ], aes(x=confounding, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0.19, 2.95))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SD_cont[SD_cont$sample == 2500, ], aes(x=confounding, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0.19, 2.95))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA12.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

# LOAD BIAS OF SE OF BINARY OUTCOME
rm(list = ls())
load("biasSE_extreme_binary.RData")
load("biasSE_moderate_binary.RData")
# combine in one dataframe 
biasSE_bi <- rbind(finalSE_extreme_binary, finalSE_moderate_binary)
biasSE_bi$class_diff <- c(rep("extreme",45), rep("moderate", 45))

biasSE_bi[biasSE_bi == "bray"] <- "Bray et al. (2019)"
biasSE_bi[biasSE_bi == "tra"] <- "New 3-Step IPW"
biasSE_bi[biasSE_bi == "schuler"] <- "Schuler et al. (2014)"
biasSE_bi[biasSE_bi == "yamaguchi"] <- "Yamaguchi (2015)"
biasSE_bi[biasSE_bi == "propensity"] <- "New PS as Covariate"

biasSE_bi[,c("method","class_diff")]= lapply(biasSE_bi[,c("method","class_diff")],
                                              as.factor)

biasSE_bi$method2 <- ""
biasSE_bi$method2[biasSE_bi$confounding==3 & biasSE_bi$sample == 2500 & biasSE_bi$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
biasSE_bi$method2[biasSE_bi$confounding==3 & biasSE_bi$sample == 2500 & biasSE_bi$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
biasSE_bi$method2[biasSE_bi$confounding==3 & biasSE_bi$sample == 2500 & biasSE_bi$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
biasSE_bi$method2[biasSE_bi$confounding==3 & biasSE_bi$sample == 2500 & biasSE_bi$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
biasSE_bi$method2[biasSE_bi$confounding==3 & biasSE_bi$sample == 2500 & biasSE_bi$method=="New PS as Covariate"] <- "New PS as Covariate  "

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

# bias of the ATE 
p1 <- ggplot(data = biasSE_bi[biasSE_bi$sample == 500 | biasSE_bi$sample == 1000, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.08,0.18))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = biasSE_bi[biasSE_bi$sample == 2500, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.08,0.18))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA13.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

# LOAD BIAS OF SE OF CONTINUOUS OUTCOME
rm(list = ls())
load("biasSE_extreme_cont.RData")
load("biasSE_moderate_cont.RData")
# combine in one dataframe 
biasSE_cont <- rbind(finalSE_extreme_cont, finalSE_moderate_cont)
biasSE_cont$class_diff <- c(rep("extreme",45), rep("moderate", 45))

biasSE_cont[biasSE_cont == "bray"] <- "Bray et al. (2019)"
biasSE_cont[biasSE_cont == "tra"] <- "New 3-Step IPW"
biasSE_cont[biasSE_cont == "schuler"] <- "Schuler et al. (2014)"
biasSE_cont[biasSE_cont == "yamaguchi"] <- "Yamaguchi (2015)"
biasSE_cont[biasSE_cont == "propensity"] <- "New PS as Covariate"

biasSE_cont[,c("method","class_diff")]= lapply(biasSE_cont[,c("method","class_diff")],
                                             as.factor)

biasSE_cont$method2 <- ""
biasSE_cont$method2[biasSE_cont$confounding==3 & biasSE_cont$sample == 2500 & biasSE_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
biasSE_cont$method2[biasSE_cont$confounding==3 & biasSE_cont$sample == 2500 & biasSE_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
biasSE_cont$method2[biasSE_cont$confounding==3 & biasSE_cont$sample == 2500 & biasSE_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
biasSE_cont$method2[biasSE_cont$confounding==3 & biasSE_cont$sample == 2500 & biasSE_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
biasSE_cont$method2[biasSE_cont$confounding==3 & biasSE_cont$sample == 2500 & biasSE_cont$method=="New PS as Covariate"] <- "New PS as Covariate  "

# create labels
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
sizediff <- c('extreme' = "extreme class size difference", 'moderate' = "moderate class size difference")

# bias of the ATE 
p1 <- ggplot(data = biasSE_cont[biasSE_cont$sample == 500 | biasSE_cont$sample == 1000, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.8,1.15))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = biasSE_cont[biasSE_cont$sample == 2500, ], aes(x=confounding, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.8,1.15))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, Inf),
                  nudge_x = 1, hjust = 1, direction = "both") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(class_diff ~ sample, labeller = labeller(sample = as_labeller(Nlabels),
                                                      class_diff = as_labeller(sizediff)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Confounding strength", gp = gpar(cex = 1.3)))

ggsave("FigureA14.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)

