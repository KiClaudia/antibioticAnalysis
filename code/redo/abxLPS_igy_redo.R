gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_igy.csv")
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
  select(jan24igy, jan26igy, feb1igy, feb8igy, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "igy", jan24igy, jan26igy, feb1igy, feb8igy) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(igy)%>%
  print()#----ID# 15, 29, 56 (have to get rid of all since it is rm)

data <- data %>%
  filter(!iguanaID %in% c("15", '29', '56'))

# visualization
ggboxplot(data, x = "abx", y = "igy", color = "time", facet.by = "lps")
ggboxplot(data, x = "lps", y = "igy", color = "time", facet.by = "abx")
ggboxplot(data, x = "time", y = "igy", color = "abx")
# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(igy, type = "mean_se")

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(igy) 

ggqqplot(data, "igy", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough
data$iguanaID <- as.numeric(data$iguanaID)
# ANOVA
model1 <- lm(data = data, igy ~ abx * time * lps + (1|iguanaID))
Anova(model1)
summary(model1)
# main effect of abx, time, lps
data %>%
  pairwise_t_test(igy ~ abx, p.adjust.method = "BH") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(abx) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "abx", y = "igy")

data %>%
  pairwise_t_test(igy ~ time, p.adjust.method = "BH") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(time) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "time", y = "igy")
#######################
data$time <- factor(data$time, levels = c('jan24igy', 'jan26igy','feb1igy','feb8igy'))
ggboxplot(data, x = "time", y = "igy")

#######################
data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "lps", y = "igy")


data %>%
  group_by(lps,time) %>%
  get_summary_stats(igy, type = "mean_se")

###########################################################
###########################################################
###########################################################

# add new column
data2 <- data %>%
  unite(txtime, abx, lps, time, sep = "", remove = FALSE)
View(data2)

# outliers
data2 %>%
  group_by(txtime) %>%
  identify_outliers(igy) #65
data2 <- data2 %>%
  filter(!iguanaID %in% c('65'))
data2
# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data2 %>%
  group_by(txtime) %>%
  shapiro_test(igy) 

ggqqplot(data2, "igy", ggtheme = theme_bw()) +
  facet_grid(txtime~time, labeller = "label_both")    # normal enough

# ANOVA

model2 <- lm(igy ~ txtime, data = data2)
Anova(model2)
summary(model2)
emmeans(model2, list(pairwise ~ txtime), adjust = "tukey")

data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")


