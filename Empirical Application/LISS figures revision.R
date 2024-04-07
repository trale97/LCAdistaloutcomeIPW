library(dplyr)
library(haven)
library(ggplot2)
library(ggrepel)



rm(list = ls())


setwd("C:/Users/fjclouth/Desktop/Tilburg PhD/Projects/5th paper/LISS")

LISS_3class_PS <- read_sav("LISS_3class_PS.sav")


setwd("C:/Users/fjclouth/Desktop/Tilburg PhD/Projects/5th paper/revision/New Graphs")


LISS_ggplot1 <- LISS_3class_PS %>%
  select(id, PS = `mCluster#1`)

LISS_ggplot2 <- LISS_3class_PS %>%
  select(id, PS = `mCluster#2`)

LISS_ggplot3 <- LISS_3class_PS %>%
  select(id, PS = `mCluster#3`)

LISS_ggplot <- rbind(LISS_ggplot1, LISS_ggplot2, LISS_ggplot3)
LISS_ggplot$LC <- rep(c(1, 2, 3), each = 3567)

LISS_ggplot$LC <- as.factor(LISS_ggplot$LC)


LISS_gg1anx <- LISS_3class_PS %>%
  filter(`Cluster#` == 1) %>%
  select(id, LC = `Cluster#`, Score = anxious)
LISS_gg1anx$Var <- "anxious"
LISS_gg1dow <- LISS_3class_PS %>%
  filter(`Cluster#` == 1) %>%
  select(id, LC = `Cluster#`, Score = feeldown)
LISS_gg1dow$Var <- "feeldown"
LISS_gg1cal <- LISS_3class_PS %>%
  filter(`Cluster#` == 1) %>%
  select(id, LC = `Cluster#`, Score = feelcalm)
LISS_gg1cal$Var <- "feelcalm"
LISS_gg1dep <- LISS_3class_PS %>%
  filter(`Cluster#` == 1) %>%
  select(id, LC = `Cluster#`, Score = depressed)
LISS_gg1dep$Var <- "depressed"
LISS_gg1hap <- LISS_3class_PS %>%
  filter(`Cluster#` == 1) %>%
  select(id, LC = `Cluster#`, Score = happy)
LISS_gg1hap$Var <- "happy"

LISS_gg1 <- rbind(LISS_gg1anx, LISS_gg1dow, LISS_gg1cal, LISS_gg1dep, LISS_gg1hap)


LISS_gg2anx <- LISS_3class_PS %>%
  filter(`Cluster#` == 2) %>%
  select(id, LC = `Cluster#`, Score = anxious)
LISS_gg2anx$Var <- "anxious"
LISS_gg2dow <- LISS_3class_PS %>%
  filter(`Cluster#` == 2) %>%
  select(id, LC = `Cluster#`, Score = feeldown)
LISS_gg2dow$Var <- "feeldown"
LISS_gg2cal <- LISS_3class_PS %>%
  filter(`Cluster#` == 2) %>%
  select(id, LC = `Cluster#`, Score = feelcalm)
LISS_gg2cal$Var <- "feelcalm"
LISS_gg2dep <- LISS_3class_PS %>%
  filter(`Cluster#` == 2) %>%
  select(id, LC = `Cluster#`, Score = depressed)
LISS_gg2dep$Var <- "depressed"
LISS_gg2hap <- LISS_3class_PS %>%
  filter(`Cluster#` == 2) %>%
  select(id, LC = `Cluster#`, Score = happy)
LISS_gg2hap$Var <- "happy"

LISS_gg2 <- rbind(LISS_gg2anx, LISS_gg2dow, LISS_gg2cal, LISS_gg2dep, LISS_gg2hap)


LISS_gg3anx <- LISS_3class_PS %>%
  filter(`Cluster#` == 3) %>%
  select(id, LC = `Cluster#`, Score = anxious)
LISS_gg3anx$Var <- "anxious"
LISS_gg3dow <- LISS_3class_PS %>%
  filter(`Cluster#` == 3) %>%
  select(id, LC = `Cluster#`, Score = feeldown)
LISS_gg3dow$Var <- "feeldown"
LISS_gg3cal <- LISS_3class_PS %>%
  filter(`Cluster#` == 3) %>%
  select(id, LC = `Cluster#`, Score = feelcalm)
LISS_gg3cal$Var <- "feelcalm"
LISS_gg3dep <- LISS_3class_PS %>%
  filter(`Cluster#` == 3) %>%
  select(id, LC = `Cluster#`, Score = depressed)
LISS_gg3dep$Var <- "depressed"
LISS_gg3hap <- LISS_3class_PS %>%
  filter(`Cluster#` == 3) %>%
  select(id, LC = `Cluster#`, Score = happy)
LISS_gg3hap$Var <- "happy"

LISS_gg3 <- rbind(LISS_gg3anx, LISS_gg3dow, LISS_gg3cal, LISS_gg3dep, LISS_gg3hap)


LISS_gg <- rbind(LISS_gg1, LISS_gg2, LISS_gg3)

LISS_gg$LC <- as.factor(LISS_gg$LC)
LISS_gg$Var <- as.factor(LISS_gg$Var)

data_ends <- LISS_gg %>%
  filter(id %in% c(800216, 845336, 899523)) %>%
  filter(Var == "happy")

data_ends$LC <- c("Class 1", "Class 2", "Class 3")
data_ends$LC <- as.factor(data_ends$LC)
data_ends$Score <- c(3, 2.15, 4.35)
LISS_gg$LC <- as.factor(LISS_gg$LC)


##### Figure 6 #####
figure6 <- ggplot(LISS_gg, aes(y=Score, x=Var, color=LC, group=LC)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  scale_color_manual(values = c("#882255", "#EFC000FF", "#1e88e5", "#882255", "#EFC000FF", "#1e88e5")) +
  xlab("Indicators") +
  labs(fill='Class', color='Class') +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) +
  scale_x_discrete(labels = c("Anxious", "Depressed", "Feeling Calm", "Feeling Down", "Happy")) + 
  geom_text_repel(aes(label = c("Class 1", "Class 2", "Class 3")), data = data_ends, force_pull = 0, 
                  position = position_nudge_repel(x = 0.1, y = 0)) +
  theme_classic() +
  theme(legend.position = "none")

ggsave("Figure6.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)


figure7 <- ggplot(LISS_ggplot, aes(x=PS, fill=LC, color=LC)) +
  geom_density(alpha=0.6) +
  ylab("Density") +
  xlab("Propensity Score") +
  labs(fill='Class', color='Class') +
  scale_fill_manual(values = c("#1e88e5", "#882255", "#EFC000FF")) +
  scale_color_manual(values = c("#1e88e5", "#882255", "#EFC000FF")) +
  theme_classic() + 
  coord_cartesian(expand = FALSE)

ggsave("Figure7.jpeg", width = 12, height = 8.85, limitsize = F, dpi = 600)
