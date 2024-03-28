# 7/21/2022 
# Question: Effect of antibiotics on phys variables - glucose? 
# Method: RM - two way ANOVA, compare across treatment (clind, pen, ctrl) from january 20 to january 24

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
  select(jan20glu, jan24glu, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "glu", jan20glu, jan24glu) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "time", y = "glu", color = "abx")


# summary stats
data %>%
  group_by(time, abx) %>%
  get_summary_stats(glu, type = "mean_se")

# outliers
data %>%
  group_by(time, abx) %>%
  identify_outliers(glu) # no outliers

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx) %>%
  shapiro_test(glu) # normal

# ANOVA
data$iguanaID <- as.numeric(data$iguanaID)
model <- lm(data = data, glu ~ abx * time + (1|iguanaID))
Anova(model)
summary(model)
# effect of time, higher glucose after antibiotic txt
data %>%
  group_by(time) %>%
  get_summary_stats(glu, type = "mean_se")

###### txtime model ##########
data <- data %>%
  unite(abxtime, abx, time, sep = "", remove = FALSE)
head(data)

data$iguanaID <- as.numeric(data$iguanaID)
model2 <- lm(data = data, glu ~ abxtime + (1|iguanaID))
Anova(model2)

library(emmeans)
emmeans(model2, list(pairwise ~ abxtime), adjust = "tukey", data = data) # water group was higher after
