# 7/25/2022 
# Question: Effect of antibiotics and LPS challenge on phys variables - BKA? 
# Method: beta regression, compare abx * LPS *dates, jan 24, jan 26, feb1, feb 8 + iguanaID random effect

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")
library("betareg")
library('lmtest')
library('emmeans')
str(gi)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)
# convert to wide
data <- gi %>%
  select(jan24bka, jan26bka, feb1bka, feb8bka, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA", jan24bka, jan26bka, feb1bka, feb8bka) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# need to replace over 1 == 1, same for 0
data$BKA[data$BKA > 100] <- 99.99
data$BKA[data$BKA < 0] <- 0.01

data2 <- data %>%
  mutate(decimal = BKA/100) 
View(data2)

range(data2$decimal) #looks good

# visualization
ggboxplot(data2, x = "time", y = "BKA", color = "abx")

# summary stats
data2 %>%
  group_by(abx) %>%
  get_summary_stats(BKA, type = "mean_se")

# normality
hist(data2$decimal) #skewed

# add new column
data3 <- data2 %>%
  unite(txtime, abx, time, lps, sep = "", remove = FALSE)
View(data3)
null <- (data2$decimal~1)

# 3-way model
library("glmmTMB")
glmm <- glmmTMB(decimal ~ abx* lps * time + (1|iguanaID), 
                data = data2, (family = beta_family(link = "logit")))
summary(glmm)
Anova(glmm)

lrtest(glmm, null)

# tx_time model
glmm2 <- glmmTMB(decimal ~ txtime + (1|iguanaID), 
                data = data3, (family = beta_family(link = "logit")))

Anova(glmm2)

# compare the two models
lrtest(glmm, glmm2) # exactly the same!

# pairwise
emmeans(glmm2, list(pairwise ~ txtime), adjust = "tukey")
emmeans(glmm, list(pairwise ~ abx), adjust = "tukey")
emmeans(glmm, list(pairwise ~ time), adjust = "tukey")
