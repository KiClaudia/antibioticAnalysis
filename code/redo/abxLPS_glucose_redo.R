
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")

str(gi)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

# convert to wide
data <- gi %>%
  select(jan24glu, feb8glu, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "glucose", jan24glu, feb8glu) %>%
  convert_as_factor(time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "time", y = "glucose")#, color = "abx")#, facet.by = "lps")
data$time <- factor(data$time, levels = c('jan24glu', 'feb8glu'))

# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(glucose, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(glucose) #----ID# 3, 70

data <- data %>%
  filter(!iguanaID %in% c("3", '70'))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(glucose) 

ggqqplot(data, "glucose", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough

# ANOVA
data$iguanaID <- as.numeric(data$iguanaID)
model <- lm(glucose ~ abx*lps*time+(1|iguanaID), data = data)
Anova(model) #n.s.

###################################################################################
###################################################################################
###################################################################################
library('emmeans')

# add new column
data2 <- data %>%
  unite(txtime, abx, lps, time, sep = "", remove = FALSE)
View(data2)

# outliers
data2 %>%
  group_by(txtime, lps) %>%
  identify_outliers(glucose) #----ID# 10,27

data2 <- data2 %>%
  filter(!iguanaID %in% c("10", '27'))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data2 %>%
  group_by(txtime, lps) %>%
  shapiro_test(glucose) 

ggqqplot(data2, "glucose", ggtheme = theme_bw()) +
  facet_grid(txtime ~ time, labeller = "label_both")    # normal enough

# ANOVA
model2 <- lm(glucose ~ txtime + (1|iguanaID), data = data2)
Anova(model2) #n.s.
