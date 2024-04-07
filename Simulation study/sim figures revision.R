

rm(list = ls())
library(ggplot2)
library(ggsci)
library(ggrepel)
library(ggtext)
library(ggpubr)
library(grid)
theme_set(theme_pubr())

setwd("C:/Users/fjclouth/Desktop/Tilburg PhD/Projects/5th paper/revision/New Graphs")





##### Figure 2 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("ATEbias_mod_binary.RData")

ATEfinal_mod_binary <- finalATE
ATEfinal_mod_binary <- ATEfinal_mod_binary[!(ATEfinal_mod_binary$method == "naive"),]

ATEfinal_mod_binary[ATEfinal_mod_binary == "bray"] <- "Bray et al. (2019)"
ATEfinal_mod_binary[ATEfinal_mod_binary == "tra"] <- "New 3-Step IPW"
ATEfinal_mod_binary[ATEfinal_mod_binary == "schuler"] <- "Schuler et al. (2014)"
ATEfinal_mod_binary[ATEfinal_mod_binary == "yamaguchi"] <- "Yamaguchi (2015)"
ATEfinal_mod_binary[ATEfinal_mod_binary == "propensity"] <- "New PS as Covariate"

ATEfinal_mod_binary$method2 <- ""
ATEfinal_mod_binary$method2[ATEfinal_mod_binary$treatment==3 & ATEfinal_mod_binary$confounding==3 & ATEfinal_mod_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
ATEfinal_mod_binary$method2[ATEfinal_mod_binary$treatment==3 & ATEfinal_mod_binary$confounding==3 & ATEfinal_mod_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
ATEfinal_mod_binary$method2[ATEfinal_mod_binary$treatment==3 & ATEfinal_mod_binary$confounding==3 & ATEfinal_mod_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
ATEfinal_mod_binary$method2[ATEfinal_mod_binary$treatment==3 & ATEfinal_mod_binary$confounding==3 & ATEfinal_mod_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
ATEfinal_mod_binary$method2[ATEfinal_mod_binary$treatment==3 & ATEfinal_mod_binary$confounding==3 & ATEfinal_mod_binary$method=="New PS as Covariate"] <- "New PS as Covariate  "

p1 <- ggplot(data = ATEfinal_mod_binary[ATEfinal_mod_binary$confounding == 1 | ATEfinal_mod_binary$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.15,0.052))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = ATEfinal_mod_binary[ATEfinal_mod_binary$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.15,0.052))+
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
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("Figure2.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure 3 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("ATEbias_mod_cont.RData")

ATEfinal_mod_cont <- finalATE
ATEfinal_mod_cont <- ATEfinal_mod_cont[!(ATEfinal_mod_cont$method == "naive"),]

ATEfinal_mod_cont[ATEfinal_mod_cont == "bray"] <- "Bray et al. (2019)"
ATEfinal_mod_cont[ATEfinal_mod_cont == "tra"] <- "New 3-Step IPW"
ATEfinal_mod_cont[ATEfinal_mod_cont == "schuler"] <- "Schuler et al. (2014)"
ATEfinal_mod_cont[ATEfinal_mod_cont == "yamaguchi"] <- "Yamaguchi (2015)"
ATEfinal_mod_cont[ATEfinal_mod_cont == "propensity"] <- "New PS as Covariate"

ATEfinal_mod_cont$method2 <- ""
ATEfinal_mod_cont$method2[ATEfinal_mod_cont$treatment==3 & ATEfinal_mod_cont$confounding==3 & ATEfinal_mod_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)    "
ATEfinal_mod_cont$method2[ATEfinal_mod_cont$treatment==3 & ATEfinal_mod_cont$confounding==3 & ATEfinal_mod_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
ATEfinal_mod_cont$method2[ATEfinal_mod_cont$treatment==3 & ATEfinal_mod_cont$confounding==3 & ATEfinal_mod_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)"
ATEfinal_mod_cont$method2[ATEfinal_mod_cont$treatment==3 & ATEfinal_mod_cont$confounding==3 & ATEfinal_mod_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
ATEfinal_mod_cont$method2[ATEfinal_mod_cont$treatment==3 & ATEfinal_mod_cont$confounding==3 & ATEfinal_mod_cont$method=="New PS as Covariate"] <- "New PS as Covariate  "

p1 <- ggplot(data = ATEfinal_mod_cont[ATEfinal_mod_cont$confounding == 1 | ATEfinal_mod_cont$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.1,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = ATEfinal_mod_cont[ATEfinal_mod_cont$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.1,0.4))+
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
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("Figure3.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure 4 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SD_mod_binary.RData")

SDfinal_mod_binary <- finalSD
SDfinal_mod_binary <- SDfinal_mod_binary[!(SDfinal_mod_binary$method == "naive"),]

SDfinal_mod_binary[SDfinal_mod_binary == "bray"] <- "Bray et al. (2019)"
SDfinal_mod_binary[SDfinal_mod_binary == "tra"] <- "New 3-Step IPW"
SDfinal_mod_binary[SDfinal_mod_binary == "schuler"] <- "Schuler et al. (2014)"
SDfinal_mod_binary[SDfinal_mod_binary == "yamaguchi"] <- "Yamaguchi (2015)"
SDfinal_mod_binary[SDfinal_mod_binary == "propensity"] <- "New PS as Covariate"

SDfinal_mod_binary$method2 <- ""
SDfinal_mod_binary$method2[SDfinal_mod_binary$treatment==3 & SDfinal_mod_binary$confounding==3 & SDfinal_mod_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)       "
SDfinal_mod_binary$method2[SDfinal_mod_binary$treatment==3 & SDfinal_mod_binary$confounding==3 & SDfinal_mod_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
SDfinal_mod_binary$method2[SDfinal_mod_binary$treatment==3 & SDfinal_mod_binary$confounding==3 & SDfinal_mod_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SDfinal_mod_binary$method2[SDfinal_mod_binary$treatment==3 & SDfinal_mod_binary$confounding==3 & SDfinal_mod_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SDfinal_mod_binary$method2[SDfinal_mod_binary$treatment==3 & SDfinal_mod_binary$confounding==3 & SDfinal_mod_binary$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SDfinal_mod_binary[SDfinal_mod_binary$confounding == 1 | SDfinal_mod_binary$confounding == 2, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,0.21))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SDfinal_mod_binary[SDfinal_mod_binary$confounding == 3, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,0.21))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("Figure4.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)





##### Figure 5 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SD_mod_cont.RData")

SDfinal_mod_cont <- finalSD
SDfinal_mod_cont <- SDfinal_mod_cont[!(SDfinal_mod_cont$method == "naive"),]

SDfinal_mod_cont[SDfinal_mod_cont == "bray"] <- "Bray et al. (2019)"
SDfinal_mod_cont[SDfinal_mod_cont == "tra"] <- "New 3-Step IPW"
SDfinal_mod_cont[SDfinal_mod_cont == "schuler"] <- "Schuler et al. (2014)"
SDfinal_mod_cont[SDfinal_mod_cont == "yamaguchi"] <- "Yamaguchi (2015)"
SDfinal_mod_cont[SDfinal_mod_cont == "propensity"] <- "New PS as Covariate"

SDfinal_mod_cont$method2 <- ""
SDfinal_mod_cont$method2[SDfinal_mod_cont$treatment==3 & SDfinal_mod_cont$confounding==3 & SDfinal_mod_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)       "
SDfinal_mod_cont$method2[SDfinal_mod_cont$treatment==3 & SDfinal_mod_cont$confounding==3 & SDfinal_mod_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
SDfinal_mod_cont$method2[SDfinal_mod_cont$treatment==3 & SDfinal_mod_cont$confounding==3 & SDfinal_mod_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SDfinal_mod_cont$method2[SDfinal_mod_cont$treatment==3 & SDfinal_mod_cont$confounding==3 & SDfinal_mod_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SDfinal_mod_cont$method2[SDfinal_mod_cont$treatment==3 & SDfinal_mod_cont$confounding==3 & SDfinal_mod_cont$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SDfinal_mod_cont[SDfinal_mod_cont$confounding == 1 | SDfinal_mod_cont$confounding == 2, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,1.7))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SDfinal_mod_cont[SDfinal_mod_cont$confounding == 3, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,1.7))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("Figure5.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A1 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SEbias_mod_binary.RData")

SEfinal_mod_binary <- finalSE
SEfinal_mod_binary <- SEfinal_mod_binary[!(SEfinal_mod_binary$method == "naive"),]

SEfinal_mod_binary[SEfinal_mod_binary == "bray"] <- "Bray et al. (2019)"
SEfinal_mod_binary[SEfinal_mod_binary == "tra"] <- "New 3-Step IPW"
SEfinal_mod_binary[SEfinal_mod_binary == "schuler"] <- "Schuler et al. (2014)"
SEfinal_mod_binary[SEfinal_mod_binary == "yamaguchi"] <- "Yamaguchi (2015)"
SEfinal_mod_binary[SEfinal_mod_binary == "propensity"] <- "New PS as Covariate"

SEfinal_mod_binary$method2 <- ""
SEfinal_mod_binary$method2[SEfinal_mod_binary$treatment==3 & SEfinal_mod_binary$confounding==3 & SEfinal_mod_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)       "
SEfinal_mod_binary$method2[SEfinal_mod_binary$treatment==3 & SEfinal_mod_binary$confounding==3 & SEfinal_mod_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
SEfinal_mod_binary$method2[SEfinal_mod_binary$treatment==3 & SEfinal_mod_binary$confounding==3 & SEfinal_mod_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SEfinal_mod_binary$method2[SEfinal_mod_binary$treatment==3 & SEfinal_mod_binary$confounding==3 & SEfinal_mod_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SEfinal_mod_binary$method2[SEfinal_mod_binary$treatment==3 & SEfinal_mod_binary$confounding==3 & SEfinal_mod_binary$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SEfinal_mod_binary[SEfinal_mod_binary$confounding == 1 | SEfinal_mod_binary$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.04,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SEfinal_mod_binary[SEfinal_mod_binary$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.04,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA1.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A2 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SEbias_mod_cont.RData")

SEfinal_mod_cont <- finalSE
SEfinal_mod_cont <- SEfinal_mod_cont[!(SEfinal_mod_cont$method == "naive"),]

SEfinal_mod_cont[SEfinal_mod_cont == "bray"] <- "Bray et al. (2019)"
SEfinal_mod_cont[SEfinal_mod_cont == "tra"] <- "New 3-Step IPW"
SEfinal_mod_cont[SEfinal_mod_cont == "schuler"] <- "Schuler et al. (2014)"
SEfinal_mod_cont[SEfinal_mod_cont == "yamaguchi"] <- "Yamaguchi (2015)"
SEfinal_mod_cont[SEfinal_mod_cont == "propensity"] <- "New PS as Covariate"

SEfinal_mod_cont$method2 <- ""
SEfinal_mod_cont$method2[SEfinal_mod_cont$treatment==3 & SEfinal_mod_cont$confounding==3 & SEfinal_mod_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)       "
SEfinal_mod_cont$method2[SEfinal_mod_cont$treatment==3 & SEfinal_mod_cont$confounding==3 & SEfinal_mod_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
SEfinal_mod_cont$method2[SEfinal_mod_cont$treatment==3 & SEfinal_mod_cont$confounding==3 & SEfinal_mod_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SEfinal_mod_cont$method2[SEfinal_mod_cont$treatment==3 & SEfinal_mod_cont$confounding==3 & SEfinal_mod_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SEfinal_mod_cont$method2[SEfinal_mod_cont$treatment==3 & SEfinal_mod_cont$confounding==3 & SEfinal_mod_cont$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SEfinal_mod_cont[SEfinal_mod_cont$confounding == 1 | SEfinal_mod_cont$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.3,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SEfinal_mod_cont[SEfinal_mod_cont$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.3,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA2.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)








##### Figure A3 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("ATEbias_good_binary.RData")

ATEfinal_good_binary <- finalATE
ATEfinal_good_binary <- ATEfinal_good_binary[!(ATEfinal_good_binary$method == "naive"),]

ATEfinal_good_binary[ATEfinal_good_binary == "bray"] <- "Bray et al. (2019)"
ATEfinal_good_binary[ATEfinal_good_binary == "tra"] <- "New 3-Step IPW"
ATEfinal_good_binary[ATEfinal_good_binary == "schuler"] <- "Schuler et al. (2014)"
ATEfinal_good_binary[ATEfinal_good_binary == "yamaguchi"] <- "Yamaguchi (2015)"
ATEfinal_good_binary[ATEfinal_good_binary == "propensity"] <- "New PS as Covariate"

ATEfinal_good_binary$method2 <- ""
ATEfinal_good_binary$method2[ATEfinal_good_binary$treatment==3 & ATEfinal_good_binary$confounding==3 & ATEfinal_good_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)       "
ATEfinal_good_binary$method2[ATEfinal_good_binary$treatment==3 & ATEfinal_good_binary$confounding==3 & ATEfinal_good_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
ATEfinal_good_binary$method2[ATEfinal_good_binary$treatment==3 & ATEfinal_good_binary$confounding==3 & ATEfinal_good_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
ATEfinal_good_binary$method2[ATEfinal_good_binary$treatment==3 & ATEfinal_good_binary$confounding==3 & ATEfinal_good_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
ATEfinal_good_binary$method2[ATEfinal_good_binary$treatment==3 & ATEfinal_good_binary$confounding==3 & ATEfinal_good_binary$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = ATEfinal_good_binary[ATEfinal_good_binary$confounding == 1 | ATEfinal_good_binary$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.15,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = ATEfinal_good_binary[ATEfinal_good_binary$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.15,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA3.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A4 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("ATEbias_good_cont.RData")

ATEfinal_good_cont <- finalATE
ATEfinal_good_cont <- ATEfinal_good_cont[!(ATEfinal_good_cont$method == "naive"),]

ATEfinal_good_cont[ATEfinal_good_cont == "bray"] <- "Bray et al. (2019)"
ATEfinal_good_cont[ATEfinal_good_cont == "tra"] <- "New 3-Step IPW"
ATEfinal_good_cont[ATEfinal_good_cont == "schuler"] <- "Schuler et al. (2014)"
ATEfinal_good_cont[ATEfinal_good_cont == "yamaguchi"] <- "Yamaguchi (2015)"
ATEfinal_good_cont[ATEfinal_good_cont == "propensity"] <- "New PS as Covariate"

ATEfinal_good_cont$method2 <- ""
ATEfinal_good_cont$method2[ATEfinal_good_cont$treatment==3 & ATEfinal_good_cont$confounding==3 & ATEfinal_good_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)      "
ATEfinal_good_cont$method2[ATEfinal_good_cont$treatment==3 & ATEfinal_good_cont$confounding==3 & ATEfinal_good_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW        "
ATEfinal_good_cont$method2[ATEfinal_good_cont$treatment==3 & ATEfinal_good_cont$confounding==3 & ATEfinal_good_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014) "
ATEfinal_good_cont$method2[ATEfinal_good_cont$treatment==3 & ATEfinal_good_cont$confounding==3 & ATEfinal_good_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
ATEfinal_good_cont$method2[ATEfinal_good_cont$treatment==3 & ATEfinal_good_cont$confounding==3 & ATEfinal_good_cont$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = ATEfinal_good_cont[ATEfinal_good_cont$confounding == 1 | ATEfinal_good_cont$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.1,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = ATEfinal_good_cont[ATEfinal_good_cont$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-1.1,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA4.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A5 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SD_good_binary.RData")

SDfinal_good_binary <- finalSD
SDfinal_good_binary <- SDfinal_good_binary[!(SDfinal_good_binary$method == "naive"),]

SDfinal_good_binary[SDfinal_good_binary == "bray"] <- "Bray et al. (2019)"
SDfinal_good_binary[SDfinal_good_binary == "tra"] <- "New 3-Step IPW"
SDfinal_good_binary[SDfinal_good_binary == "schuler"] <- "Schuler et al. (2014)"
SDfinal_good_binary[SDfinal_good_binary == "yamaguchi"] <- "Yamaguchi (2015)"
SDfinal_good_binary[SDfinal_good_binary == "propensity"] <- "New PS as Covariate"

SDfinal_good_binary$method2 <- ""
SDfinal_good_binary$method2[SDfinal_good_binary$treatment==3 & SDfinal_good_binary$confounding==3 & SDfinal_good_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)      "
SDfinal_good_binary$method2[SDfinal_good_binary$treatment==3 & SDfinal_good_binary$confounding==3 & SDfinal_good_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SDfinal_good_binary$method2[SDfinal_good_binary$treatment==3 & SDfinal_good_binary$confounding==3 & SDfinal_good_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SDfinal_good_binary$method2[SDfinal_good_binary$treatment==3 & SDfinal_good_binary$confounding==3 & SDfinal_good_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SDfinal_good_binary$method2[SDfinal_good_binary$treatment==3 & SDfinal_good_binary$confounding==3 & SDfinal_good_binary$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SDfinal_good_binary[SDfinal_good_binary$confounding == 1 | SDfinal_good_binary$confounding == 2, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,0.21))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SDfinal_good_binary[SDfinal_good_binary$confounding == 3, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,0.21))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA5.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)





##### Figure A6 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SD_good_cont.RData")

SDfinal_good_cont <- finalSD
SDfinal_good_cont <- SDfinal_good_cont[!(SDfinal_good_cont$method == "naive"),]

SDfinal_good_cont[SDfinal_good_cont == "bray"] <- "Bray et al. (2019)"
SDfinal_good_cont[SDfinal_good_cont == "tra"] <- "New 3-Step IPW"
SDfinal_good_cont[SDfinal_good_cont == "schuler"] <- "Schuler et al. (2014)"
SDfinal_good_cont[SDfinal_good_cont == "yamaguchi"] <- "Yamaguchi (2015)"
SDfinal_good_cont[SDfinal_good_cont == "propensity"] <- "New PS as Covariate"

SDfinal_good_cont$method2 <- ""
SDfinal_good_cont$method2[SDfinal_good_cont$treatment==3 & SDfinal_good_cont$confounding==3 & SDfinal_good_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)      "
SDfinal_good_cont$method2[SDfinal_good_cont$treatment==3 & SDfinal_good_cont$confounding==3 & SDfinal_good_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SDfinal_good_cont$method2[SDfinal_good_cont$treatment==3 & SDfinal_good_cont$confounding==3 & SDfinal_good_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014)  "
SDfinal_good_cont$method2[SDfinal_good_cont$treatment==3 & SDfinal_good_cont$confounding==3 & SDfinal_good_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SDfinal_good_cont$method2[SDfinal_good_cont$treatment==3 & SDfinal_good_cont$confounding==3 & SDfinal_good_cont$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SDfinal_good_cont[SDfinal_good_cont$confounding == 1 | SDfinal_good_cont$confounding == 2, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,1.7))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SDfinal_good_cont[SDfinal_good_cont$confounding == 3, ], aes(x=treatment, y = cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(0,1.7))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("SD of the ATE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA6.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A7 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SEbias_good_binary.RData")

SEfinal_good_binary <- finalSE
SEfinal_good_binary <- SEfinal_good_binary[!(SEfinal_good_binary$method == "naive"),]

SEfinal_good_binary[SEfinal_good_binary == "bray"] <- "Bray et al. (2019)"
SEfinal_good_binary[SEfinal_good_binary == "tra"] <- "New 3-Step IPW"
SEfinal_good_binary[SEfinal_good_binary == "schuler"] <- "Schuler et al. (2014)"
SEfinal_good_binary[SEfinal_good_binary == "yamaguchi"] <- "Yamaguchi (2015)"
SEfinal_good_binary[SEfinal_good_binary == "propensity"] <- "New PS as Covariate"

SEfinal_good_binary$method2 <- ""
SEfinal_good_binary$method2[SEfinal_good_binary$treatment==3 & SEfinal_good_binary$confounding==3 & SEfinal_good_binary$method=="Bray et al. (2019)"] <- "Bray et al. (2019)      "
SEfinal_good_binary$method2[SEfinal_good_binary$treatment==3 & SEfinal_good_binary$confounding==3 & SEfinal_good_binary$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SEfinal_good_binary$method2[SEfinal_good_binary$treatment==3 & SEfinal_good_binary$confounding==3 & SEfinal_good_binary$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014) "
SEfinal_good_binary$method2[SEfinal_good_binary$treatment==3 & SEfinal_good_binary$confounding==3 & SEfinal_good_binary$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SEfinal_good_binary$method2[SEfinal_good_binary$treatment==3 & SEfinal_good_binary$confounding==3 & SEfinal_good_binary$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SEfinal_good_binary[SEfinal_good_binary$confounding == 1 | SEfinal_good_binary$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.04,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SEfinal_good_binary[SEfinal_good_binary$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.04,0.05))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA7.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)




##### Figure A8 #####
rm(list = ls())

Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

load("SEbias_good_cont.RData")

SEfinal_good_cont <- finalSE
SEfinal_good_cont <- SEfinal_good_cont[!(SEfinal_good_cont$method == "naive"),]

SEfinal_good_cont[SEfinal_good_cont == "bray"] <- "Bray et al. (2019)"
SEfinal_good_cont[SEfinal_good_cont == "tra"] <- "New 3-Step IPW"
SEfinal_good_cont[SEfinal_good_cont == "schuler"] <- "Schuler et al. (2014)"
SEfinal_good_cont[SEfinal_good_cont == "yamaguchi"] <- "Yamaguchi (2015)"
SEfinal_good_cont[SEfinal_good_cont == "propensity"] <- "New PS as Covariate"

SEfinal_good_cont$method2 <- ""
SEfinal_good_cont$method2[SEfinal_good_cont$treatment==3 & SEfinal_good_cont$confounding==3 & SEfinal_good_cont$method=="Bray et al. (2019)"] <- "Bray et al. (2019)      "
SEfinal_good_cont$method2[SEfinal_good_cont$treatment==3 & SEfinal_good_cont$confounding==3 & SEfinal_good_cont$method=="New 3-Step IPW"] <- "New 3-Step IPW       "
SEfinal_good_cont$method2[SEfinal_good_cont$treatment==3 & SEfinal_good_cont$confounding==3 & SEfinal_good_cont$method=="Schuler et al. (2014)"] <- "Schuler et al. (2014) "
SEfinal_good_cont$method2[SEfinal_good_cont$treatment==3 & SEfinal_good_cont$confounding==3 & SEfinal_good_cont$method=="Yamaguchi (2015)"] <- "Yamaguchi (2015)     "
SEfinal_good_cont$method2[SEfinal_good_cont$treatment==3 & SEfinal_good_cont$confounding==3 & SEfinal_good_cont$method=="New PS as Covariate"] <- "New PS as Covariate"

p1 <- ggplot(data = SEfinal_good_cont[SEfinal_good_cont$confounding == 1 | SEfinal_good_cont$confounding == 2, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.3,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels))) +
  theme(strip.text.y = element_blank())

p2 <- ggplot(data = SEfinal_good_cont[SEfinal_good_cont$confounding == 3, ], aes(x=treatment, y = bias_cluster3, group = method, color = method, label = method2))+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(limits = c(-0.3,0.4))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#882255","#1e88e5","#004d40"))+
  labs(color='Method') +
  theme_bw() +
  geom_text_repel(aes(label = ifelse(method2 == "", "", method2)), xlim = c(3, NA),
                  nudge_x = 1, hjust = 0, direction = "y", min.segment.length = Inf) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=15), strip.text = element_text(size = 12), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(-0.18,1,-.18,-.18), "lines")) + # this is for the gap between the two panels
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))

figure <- ggarrange(p1, p2,
                    ncol = 2, nrow = 1, align = "h", widths = c(1.3,1)) #you can adjust the widths for the x-axis ticks

annotate_figure(figure, left = textGrob("Bias of the SE", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Effect size", gp = gpar(cex = 1.3)))

ggsave("FigureA8.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)
