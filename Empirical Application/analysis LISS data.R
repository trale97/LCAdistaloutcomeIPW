

library(dplyr)
library(foreign)
library(rio)
library(ggplot2)
library(haven)
library(mice)
library(survey)
library(tableone)



back20 <- read_sav("avars_202011_EN_1.0p.sav")

back20$id <- back20$nomem_encr

table(back20$geslacht)
back20$gender <- "Male"
back20$gender[back20$geslacht==2] <- "Female"
table(back20$geslacht, back20$gender, useNA = "always")

table(back20$leeftijd)
back20$age <- back20$leeftijd

table(back20$burgstat)
back20$HHstatus <- NA
back20$HHstatus[back20$burgstat==1] <- "Married"
back20$HHstatus[back20$burgstat==2] <- "Separated"
back20$HHstatus[back20$burgstat==3] <- "Divorced"
back20$HHstatus[back20$burgstat==4] <- "Widowed"
back20$HHstatus[back20$burgstat==5] <- "Never been married"
table(back20$burgstat, back20$HHstatus, useNA = "always")

summary(back20$brutoink_f)
back20$income <- back20$brutoink_f
summary(back20$brutohh_f)
back20$HHincome <- back20$brutohh_f

table(back20$oplzon)
table(back20$oplcat)
back20$education <- NA
back20$education[back20$oplcat==1] <- "Primary school"
back20$education[back20$oplcat==2] <- "vmbo"
back20$education[back20$oplcat==3] <- "havo/vwo"
back20$education[back20$oplcat==4] <- "mbo"
back20$education[back20$oplcat==5] <- "hbo"
back20$education[back20$oplcat==6] <- "wo"
table(back20$oplcat, back20$education, useNA = "always")

table(back20$herkomstgroep)
back20$origin <- NA
back20$origin[back20$herkomstgroep==0] <- "Dutch"
back20$origin[back20$herkomstgroep==101] <- "first gen West"
back20$origin[back20$herkomstgroep==102] <- "first gen non-West"
back20$origin[back20$herkomstgroep==201] <- "second gen West"
back20$origin[back20$herkomstgroep==202] <- "second gen non-West"
table(back20$herkomstgroep, back20$origin, useNA = "always")

table(back20$belbezig)
back20$job <- NA
back20$job[back20$belbezig==1] <- "Paid employment"
back20$job[back20$belbezig==2] <- "Works or assists in family business"
back20$job[back20$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back20$job[back20$belbezig==4] <- "Job seeker following job loss"
back20$job[back20$belbezig==5] <- "First-time job seeker"
back20$job[back20$belbezig==6] <- "Exempted from job seeking following job loss"
back20$job[back20$belbezig==7] <- "Attends school or is studying"
back20$job[back20$belbezig==8] <- "Takes care of the housekeeping"
back20$job[back20$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back20$job[back20$belbezig==10] <- "Has (partial) work disability"
back20$job[back20$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back20$job[back20$belbezig==12] <- "Performs voluntary work"
back20$job[back20$belbezig==13] <- "Does something else"
back20$job[back20$belbezig==14] <- "Is too young to have an occupation"
table(back20$belbezig, back20$job, useNA = "always")

back20$job_lag <- NA
back20$job_lag[back20$belbezig==1] <- "Paid work"
back20$job_lag[back20$belbezig==2] <- "Paid work"
back20$job_lag[back20$belbezig==3] <- "Paid work"
back20$job_lag[back20$belbezig==4] <- "No paid work"
back20$job_lag[back20$belbezig==5] <- "No paid work"
back20$job_lag[back20$belbezig==6] <- "No paid work"
back20$job_lag[back20$belbezig==7] <- "School/ too young"
back20$job_lag[back20$belbezig==8] <- "No paid work"
back20$job_lag[back20$belbezig==9] <- "Pensioner"
back20$job_lag[back20$belbezig==10] <- "No paid work"
back20$job_lag[back20$belbezig==11] <- "No paid work"
back20$job_lag[back20$belbezig==12] <- "No paid work"
back20$job_lag[back20$belbezig==13] <- "No paid work"
back20$job_lag[back20$belbezig==14] <- "School/ too young"
table(back20$job, back20$job_lag, useNA = "always")
table(back20$job_lag, useNA = "always")

back20 <- back20 %>%
  select(id, job_lag, gender, age, HHstatus, HHincome, education, origin)



health20 <- read.spss("ch20m_EN_1.0p.sav", to.data.frame = T)

health20$id <- health20$nomem_encr

table(health20$ch20m004, useNA = "always")
health20$health <- NA
health20$health[health20$ch20m004 == "poor"] <- 1
health20$health[health20$ch20m004 == "moderate"] <- 2
health20$health[health20$ch20m004 == "good"] <- 3
health20$health[health20$ch20m004 == "very good"] <- 4
health20$health[health20$ch20m004 == "excellent"] <- 5
table(health20$health, health20$ch20m004, useNA = "always")

table(health20$ch20m011, useNA = "always")
health20$anxious <- NA
health20$anxious[health20$ch20m011 == "never"] <- 1
health20$anxious[health20$ch20m011 == "seldom"] <- 2
health20$anxious[health20$ch20m011 == "sometimes"] <- 3
health20$anxious[health20$ch20m011 == "often"] <- 4
health20$anxious[health20$ch20m011 == "mostly"] <- 5
health20$anxious[health20$ch20m011 == "continuously"] <- 6
table(health20$anxious, useNA = "always")

table(health20$ch20m012, useNA = "always")
health20$feeldown <- NA
health20$feeldown[health20$ch20m012 == "never"] <- 1
health20$feeldown[health20$ch20m012 == "seldom"] <- 2
health20$feeldown[health20$ch20m012 == "sometimes"] <- 3
health20$feeldown[health20$ch20m012 == "often"] <- 4
health20$feeldown[health20$ch20m012 == "mostly"] <- 5
health20$feeldown[health20$ch20m012 == "continuously"] <- 6
table(health20$feeldown, useNA = "always")

table(health20$ch20m013, useNA = "always")
health20$feelcalm <- NA
health20$feelcalm[health20$ch20m013 == "never"] <- 6
health20$feelcalm[health20$ch20m013 == "seldom"] <- 5
health20$feelcalm[health20$ch20m013 == "sometimes"] <- 4
health20$feelcalm[health20$ch20m013 == "often"] <- 3
health20$feelcalm[health20$ch20m013 == "mostly"] <- 2
health20$feelcalm[health20$ch20m013 == "continuously"] <- 1
table(health20$feelcalm, useNA = "always")

table(health20$ch20m014, useNA = "always")
health20$depressed <- NA
health20$depressed[health20$ch20m014 == "never"] <- 1
health20$depressed[health20$ch20m014 == "seldom"] <- 2
health20$depressed[health20$ch20m014 == "sometimes"] <- 3
health20$depressed[health20$ch20m014 == "often"] <- 4
health20$depressed[health20$ch20m014 == "mostly"] <- 5
health20$depressed[health20$ch20m014 == "continuously"] <- 6
table(health20$depressed, useNA = "always")

table(health20$ch20m015, useNA = "always")
health20$happy <- NA
health20$happy[health20$ch20m015 == "never"] <- 6
health20$happy[health20$ch20m015 == "seldom"] <- 5
health20$happy[health20$ch20m015 == "sometimes"] <- 4
health20$happy[health20$ch20m015 == "often"] <- 3
health20$happy[health20$ch20m015 == "mostly"] <- 2
health20$happy[health20$ch20m015 == "continuously"] <- 1
table(health20$happy, useNA = "always")

health20 <- health20 %>%
  select(id, health, anxious, feeldown, feelcalm, depressed, happy)



back21 <- read_sav("avars_202111_EN_1.0p.sav")

back21$id <- back21$nomem_encr

table(back21$belbezig)
back21$job <- NA
back21$job[back21$belbezig==1] <- "Paid employment"
back21$job[back21$belbezig==2] <- "Works or assists in family business"
back21$job[back21$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back21$job[back21$belbezig==4] <- "Job seeker following job loss"
back21$job[back21$belbezig==5] <- "First-time job seeker"
back21$job[back21$belbezig==6] <- "Exempted from job seeking following job loss"
back21$job[back21$belbezig==7] <- "Attends school or is studying"
back21$job[back21$belbezig==8] <- "Takes care of the housekeeping"
back21$job[back21$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back21$job[back21$belbezig==10] <- "Has (partial) work disability"
back21$job[back21$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back21$job[back21$belbezig==12] <- "Performs voluntary work"
back21$job[back21$belbezig==13] <- "Does something else"
back21$job[back21$belbezig==14] <- "Is too young to have an occupation"
table(back21$belbezig, back21$job, useNA = "always")

back21$job_cat <- NA
back21$job_cat[back21$belbezig==1] <- "Paid work"
back21$job_cat[back21$belbezig==2] <- "Paid work"
back21$job_cat[back21$belbezig==3] <- "Paid work"
back21$job_cat[back21$belbezig==4] <- "No paid work"
back21$job_cat[back21$belbezig==5] <- "No paid work"
back21$job_cat[back21$belbezig==6] <- "No paid work"
back21$job_cat[back21$belbezig==7] <- "School/ too young"
back21$job_cat[back21$belbezig==8] <- "No paid work"
back21$job_cat[back21$belbezig==9] <- "Pensioner"
back21$job_cat[back21$belbezig==10] <- "No paid work"
back21$job_cat[back21$belbezig==11] <- "No paid work"
back21$job_cat[back21$belbezig==12] <- "No paid work"
back21$job_cat[back21$belbezig==13] <- "No paid work"
back21$job_cat[back21$belbezig==14] <- "School/ too young"
table(back21$job, back21$job_cat, useNA = "always")
table(back21$job_cat, useNA = "always")

back21 <- back21 %>%
  select(id, job_cat)



LISS_raw <- merge(back20, health20, by = "id")

LISS_raw <- merge(LISS_raw, back21, by = "id")

LISS_raw <- LISS_raw %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner") %>%
  filter(job_lag != "School/ too young") %>%
  filter(job_lag != "Pensioner")

table(LISS_raw$job_cat, useNA = "always")
table(LISS_raw$job_lag, useNA = "always")
table(LISS_raw$gender, useNA = "always")
table(LISS_raw$age, useNA = "always")
table(LISS_raw$HHstatus, useNA = "always")
table(is.na(LISS_raw$HHincome), useNA = "always")
table(LISS_raw$education, useNA = "always")
table(LISS_raw$origin, useNA = "always")
table(LISS_raw$health, useNA = "always")

LISS_raw$job_cat <- as.factor(LISS_raw$job_cat)
LISS_raw$job_lag <- as.factor(LISS_raw$job_lag)
LISS_raw$gender <- as.factor(LISS_raw$gender)
LISS_raw$HHstatus <- as.factor(LISS_raw$HHstatus)
LISS_raw$education <- as.factor(LISS_raw$education)
LISS_raw$origin <- as.factor(LISS_raw$origin)

# Set imputation method
impmethod <- character(ncol(LISS_raw))
names(impmethod) <- colnames(LISS_raw)
impmethod["education"] <- impmethod["origin"] <- impmethod["HHincome"] <- "pmm"
impmethod

pm <- make.predictorMatrix(LISS_raw)

pm[, "id"] <- 0
pm["id",] <- 0
pm["anxious",] <- 0
pm["feeldown",] <- 0
pm["feelcalm",] <- 0
pm["depressed",] <- 0
pm["happy",] <- 0
pm

LISS_imp <- mice(LISS_raw, m=1, predictorMatrix = pm,
                          method=impmethod, maxit=10, printFlag = FALSE, seed=1874)


LISS_imp <- complete(LISS_imp, action = "long")

table(LISS_imp$job_cat, useNA = "always")
table(LISS_imp$job_lag, useNA = "always")
table(LISS_imp$gender, useNA = "always")
table(LISS_imp$age, useNA = "always")
table(LISS_imp$HHstatus, useNA = "always")
table(is.na(LISS_imp$HHincome), useNA = "always")
table(LISS_imp$education, useNA = "always")
table(LISS_imp$origin, useNA = "always")
table(LISS_imp$health, useNA = "always")
summary(LISS_imp$HHincome)
summary(LISS_raw$HHincome)


LISS_imp <- LISS_imp %>%
  filter(!is.na(HHincome))


LISS_imp <- LISS_imp %>%
  select(id, job_cat, job_lag, gender, age, HHstatus, HHincome, education, origin, health, anxious, feeldown, feelcalm, depressed, happy)


export(LISS_imp, "LISS_imp.sav")


# run LG syntax files


LISS_3class_PS <- read_sav("LISS_3class_PS.sav")


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
