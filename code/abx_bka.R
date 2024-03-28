# 7/21/2022 
# Question: Effect of antibiotics on phys variables - BKA? 
# Method: beta regression, compare across treatment (clind, pen, ctrl) from january 20 to january 24

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")
library("betareg")
library("AICcmodavg")
str(gi)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

# convert to wide
data <- gi %>%
  select(jan20bka, jan24bka, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA", jan20bka, jan24bka) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# need to replace over 1 == 1, same for 0
data$BKA[data$BKA > 100] <- 99.99 
data$BKA[data$BKA < 0] <- 0.01
View(data)

# visualization
ggboxplot(data, x = "time", y = "BKA", color = "abx")

# summary stats
data %>%
  group_by(time, abx) %>%
  get_summary_stats(BKA, type = "mean_se")

# normality
hist(data$BKA) #----nope, percent data, skewed

# change data to decimal
data <- data %>%
  mutate(decimal = BKA/100) 
range(data$decimal) # range is good

# beta regression
library("glmmTMB")
glmm <- glmmTMB(decimal ~ abx* time + (1|iguanaID), 
                data = data, (family = beta_family(link = "logit")))

Anova(glmm)

null <- glmmTMB(decimal ~ 1, 
                data = data, (family = beta_family(link = "logit")))
library(lmtest)
lrtest(glmm, null)

#### not doing txtime model because if it wasn't significant in the 2-way model, it won't be in the txtime model since it is suppose to be the same
