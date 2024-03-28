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
  select(jan24mass, feb8mass, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "mass", jan24mass, feb8mass) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "mass", color = "time", facet.by = "lps")


# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(mass, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(mass) # no outliers

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(mass) # normal

# ANOVA
data$iguanaID <- as.numeric(data$iguanaID)
model1 <- lm(data = data, mass ~ abx * lps * time + (1|iguanaID))
Anova(model1)

# ns
################################################################################
################################################################################
################################################################################
library('emmeans')

# add new column
data <- data %>%
  unite(txtime, abx, lps, time, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "txtime", y = "mass", color = "lps")#, facet.by = "lps")

# outliers
data %>%
  group_by(txtime, lps) %>%
  identify_outliers(mass) 

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(txtime, lps) %>%
  shapiro_test(mass) 

ggqqplot(data, "mass", ggtheme = theme_bw()) +
  facet_grid( txtime ~ time, labeller = "label_both")    # normal enough

# ANOVA
data$iguanaID <- as.numeric(data$iguanaID)
model2 <- lm(data = data, mass ~ txtime + (1|iguanaID))
Anova(model2)

