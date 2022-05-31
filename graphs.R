################################################################################
#### Title: Generate graphs
#### Author: Tra T. Le
#### Last modified: May 31, 2022

#example code for the binary Z and moderate separation
#modify to use for other conditions

library(ggplot2)
library(ggsci)
Nlabels <- c('500' = "N = 500", '1000' = "N = 1000", '2500' = "N = 2500")
glabels <- c('1' = "Confounding = 1", '2' = "Confounding = 2", '3' = "Confounding = 3")

finalSD2 <- finalSD[!(finalSD$method == "tra modal"),]
finalSD3 <- finalSD2[!(finalSD2$method == "bray proportional"),]
finalSD3[finalSD3 == "bray"] <- "Bray et al. (2019)"
finalSD3[finalSD3 == "tra"] <- "Our method"
finalSD3[finalSD3 == "schuler"] <- "Schuler et al. (2014)"
finalSD3[finalSD3 == "yamaguchi"] <- "Yamaguchi (2015)"
finalSD3[finalSD3 == "naive"] <- "Naive method"

png("SD.png", width = 3100, height = 2100, res = 300)
ggplot(data = finalSD3, aes(x=treatment, y = cluster3, group = method, color = method))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_color_manual(values = c("#EFC000FF","#868686FF","#A73030FF","#0073C2FF","#20854EFF"))+
  xlab("Effect Size") +
  ylab("SD of the ATE") + 
  theme(panel.background = element_rect(fill = "grey96"), axis.text=element_text(size=12), 
        axis.title=element_text(size=15), legend.text = element_text(size=15), 
        legend.title = element_text(size=15), strip.text = element_text(size = 12)) +
  facet_grid(sample ~ confounding, labeller = labeller(sample = as_labeller(Nlabels),
                                                       confounding = as_labeller(glabels)))
dev.off()