# 7/21/2022 
# Question: Effect of antibiotics on phys variables - totri? 
# Method: RM - two way ANOVA, compare across treatment (clind, pen, ctrl) from january 20 to january 24

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")

str(gi)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

# convert to wide
data <- gi %>%
  select(jan20totri, jan24totri, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "totri", jan20totri, jan24totri) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "time", y = "totri", color = "abx")


# summary stats
data %>%
  group_by(time, abx) %>%
  get_summary_stats(totri, type = "mean_se")

# outliers
data %>%
  group_by(time, abx) %>%
  identify_outliers(totri) #----- is.extreme ID #46, 49, 57, 64

data <- data %>%
  filter(!iguanaID %in% c("46", "49", "57", "64"))
View(data)

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx) %>%
  shapiro_test(totri) # not normal

hist(log(data$totri)) # log to normalize

data  <- data %>%
  mutate(totrilogged = log(totri))
View(data)

# ANOVA
data$iguanaID <- as.numeric(data$iguanaID)
model <- lm(data = data, totrilogged ~ abx * time + (1|iguanaID))
Anova(model)


# not significant so not doing txtime model because it will also not be significant