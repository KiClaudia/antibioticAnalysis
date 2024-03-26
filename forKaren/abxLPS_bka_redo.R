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
library('AICcmodavg')
library('lmtest')
library('emmeans')
str(gi)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
gi$iguanaID <- as.character(gi$iguanaID)
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
  group_by(time, abx, lps) %>%
  get_summary_stats(BKA, type = "mean_se")

#-----original way I did the analysis using all three variables abx*lps*time--------------------
data2$iguanaID <- as.numeric(data2$iguanaID)

# normality
hist(data2$decimal) #skewed

# model selection
mod1 <- betareg::betareg(data2$decimal ~ data2$abx * data2$lps * data2$time + data2$iguanaID)
mod2 <- betareg::betareg(data2$decimal ~ data2$abx * data2$time + (1|data2$iguanaID)) 
mod3 <- betareg::betareg(data2$decimal ~ data2$abx * data2$lps )
mod4 <- betareg::betareg(data2$decimal ~ data2$lps * data2$time)
mod5 <- betareg::betareg(data2$decimal ~ data2$lps)
mod6 <- betareg::betareg(data2$decimal ~ data2$abx)
mod7 <- betareg::betareg(data2$decimal ~ data2$time)
mod8 <- betareg::betareg(data2$decimal ~ 1)

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

selection <- aictab(cand.set = models)
selection

# regression of top 2 models
summary (mod2)
summary (mod7)
summary(mod1)
lrtest(mod8, mod2)
Anova(mod1)
#plot
ggplot(data, aes(x=factor(time, level=c('jan20bka', 'jan24bka', 'jan26bka', 'feb1bka', 'feb8bka')), y=BKA, color = abx)) + 
  geom_boxplot() +
  xlab("time") +
  scale_x_discrete(breaks=c('jan20bka', 'jan24bka', 'jan26bka', 'feb1bka', 'feb8bka'),
                   labels=c("AbxTrtStart", "AbxTrtEnd", "24hrPostLPS", "1weekPostLPS", "2weekPostLPS"))


#-------Redo using Karen's method (combina abx and time)------------------------

# add new column
data3 <- data2 %>%
  unite(txtime, abx, time, lps, sep = "", remove = FALSE)
View(data3)

# AIC and model
moda <-  betareg::betareg(decimal ~ txtime, data=data3)
mod2 <- betareg::betareg(decimal ~ abxtime, data = data3) 
mod3 <- betareg::betareg(decimal ~ lps, data=data3 )
mod4 <- betareg::betareg(decimal ~ 1, data=data3 )

models <- list(mod1, mod2, mod3, mod4)

selection <- aictab(cand.set = models)
selection

# regression
summary (moda)
Anova(moda)
lrtest(mod4, mod2)
lrtest(mod1, moda)

emmeans(mod2, list(pairwise ~ abxtime), adjust = "tukey")
glmmTMB(y ~ 1 + (1|pond), df, family=beta_family(link="logit"))
