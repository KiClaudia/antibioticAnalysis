# 7/25/2022 
# Question: Effect of antibiotics and LPS challenge on phys variables - BKA? 
# Method: beta regression, compare across treatment (clind, pen, ctrl) for dates, jan20, jan 24, jan 26, feb1, feb 8

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")
library("betareg")
library('AICcmodavg')
str(gi)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

#-------- all 5 time points------------------------
# convert to wide
data <- gi %>%
  select(jan20bka, jan24bka, jan26bka, feb1bka, feb8bka, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA",jan20bka, jan24bka, jan26bka, feb1bka, feb8bka) %>%
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
  group_by(time, abx, lps) %>%
  get_summary_stats(BKA, type = "mean_se")

# normality
hist(data$BKA) #----nope, percent data, skewed

# change data to decimal
data <- data %>%
  mutate(decimal = BKA/100) 
range(data$decimal) # range is good

# model selection

mod1 <-  betareg::betareg(data$decimal ~ data$abx * data$lps * data$time)
mod2 <- betareg::betareg(data$decimal ~ data$abx * data$time) 
mod3 <- betareg::betareg(data$decimal ~ data$abx * data$lps )
mod4 <- betareg::betareg(data$decimal ~ data$lps * data$time)
mod5 <- betareg::betareg(data$decimal ~ data$lps)
mod6 <- betareg::betareg(data$decimal ~ data$abx)
mod7 <- betareg::betareg(data$decimal ~ data$time)
mod8 <- betareg::betareg(data$decimal ~ 1)

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod2)
summary (mod7)

library(lmtest)
lrtest(mod8, mod2)

data %>%
  group_by(abx) %>%
  get_summary_stats(BKA, type = "mean_se")

#-------------4 time points (LPS challenge only)-----------
data2 <- data %>%
  filter(!time == "jan20bka")
View(data2)

range(data2$decimal)

mod1 <-  betareg::betareg(decimal ~ abx * lps * time, data=data2)
mod2 <- betareg::betareg(decimal ~ abx * time, data = data2) 
mod3 <- betareg::betareg(decimal ~ abx * lps, data=data2 )
mod4 <- betareg::betareg(decimal ~ lps * time, data=data2 )
mod5 <- betareg::betareg(decimal ~ lps, data=data2 )
mod6 <- betareg::betareg(decimal ~ abx, data=data2 )
mod7 <- betareg::betareg(decimal ~ time, data=data2 )

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod2)
summary (mod7)

#-------------before and 2week after LPS challenge--------------
data3 <- data %>%
  filter(!time %in% c("jan20bka", "feb1bka", "jan26bka"))
View(data3)

mod1 <-  betareg::betareg(decimal ~ abx * lps * time, data=data3)
mod2 <- betareg::betareg(decimal ~ abx * time, data = data3) 
mod3 <- betareg::betareg(decimal ~ abx * lps, data=data3 )
mod4 <- betareg::betareg(decimal ~ lps * time, data=data3 )
mod5 <- betareg::betareg(decimal ~ lps, data=data3 )
mod6 <- betareg::betareg(decimal ~ abx, data=data3 )
mod7 <- betareg::betareg(decimal ~ time, data=data3 )

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod2)
summary (mod7)

# summary stats
data3 %>%
  group_by( time) %>%
  get_summary_stats(BKA, type = "mean_se")

#-------------before and 24hr after LPS challenge--------------
data4 <- data %>%
  filter(!time %in% c("jan20bka", "feb1bka", "feb8bka"))
View(data4)

mod1 <-  betareg::betareg(decimal ~ abx * lps * time, data=data4)
mod2 <- betareg::betareg(decimal ~ abx * time, data = data4) 
mod3 <- betareg::betareg(decimal ~ abx * lps, data=data4 )
mod4 <- betareg::betareg(decimal ~ lps * time, data=data4 )
mod5 <- betareg::betareg(decimal ~ lps, data=data4 )
mod6 <- betareg::betareg(decimal ~ abx, data=data4 )
mod7 <- betareg::betareg(decimal ~ time, data=data4 )

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod6)
summary (mod7)
summary(mod2)

# summary stats
data4 %>%
  group_by( time) %>%
  get_summary_stats(BKA, type = "mean_se")

#-------------before and 1week after LPS challenge--------------
data5 <- data %>%
  filter(!time %in% c("jan20bka", "jan26bka", "feb8bka"))
View(data5)

mod1 <-  betareg::betareg(decimal ~ abx * lps * time, data=data5)
mod2 <- betareg::betareg(decimal ~ abx * time, data = data5) 
mod3 <- betareg::betareg(decimal ~ abx * lps, data=data5 )
mod4 <- betareg::betareg(decimal ~ lps * time, data=data5 )
mod5 <- betareg::betareg(decimal ~ lps, data=data5 )
mod6 <- betareg::betareg(decimal ~ abx, data=data5 )
mod7 <- betareg::betareg(decimal ~ time, data=data5 )

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod6)
summary (mod7)

# summary stats
data5 %>%
  group_by( abx) %>%
  get_summary_stats(BKA, type = "mean_se")

#---------------graphs-------------------------
ggplot(data, aes(x=factor(time, level=c('jan20bka', 'jan24bka', 'jan26bka', 'feb1bka', 'feb8bka')), y=BKA, color = abx)) + 
  geom_boxplot() +
  xlab("time") +
  scale_x_discrete(breaks=c('jan20bka', 'jan24bka', 'jan26bka', 'feb1bka', 'feb8bka'),
                   labels=c("AbxTrtStart", "AbxTrtEnd", "24hrPostLPS", "1weekPostLPS", "2weekPostLPS"))



###########################################################
###########################################################
###########################################################
# convert to wide
data <- gi %>%
  select(jan24bka, jan26bka, feb1bka, feb8bka, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA",jan24bka, jan26bka, feb1bka, feb8bka) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# add new column
data <- data %>%
  unite(abxtime, abx, time, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "abxtime", y = "BKA", color = "lps")#, facet.by = "lps")

# AIC and model
range(data$decimal)

mod1 <-  betareg::betareg(decimal ~ abxtime*lps, data=data)
mod2 <- betareg::betareg(decimal ~ abxtime, data = data) 
mod3 <- betareg::betareg(decimal ~ lps, data=data )
mod4 <- betareg::betareg(decimal ~ 1, data=data )

models <- list(mod1, mod2, mod3, mod4)

selection <- aictab(cand.set = models)
selection

# regression
summary (mod2)
library(lmtest)
lrtest(mod4, mod2)

emmeans(mod2, list(pairwise ~ abxtime), adjust = "tukey")

data %>%
  group_by(lps) %>%
  get_summary_stats(BKA, type = "mean_se")