# 4/19/2023
# Question: Effect of antibiotics and LPS on phys variables - igy? 
# Method: RM - three way ANOVA, compare across treatment (clind, pen, ctrl), LPS (L or P) from time period (jan24, jan26, feb1, feb8)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_igy.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")
library('emmeans')
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
  identify_outliers(igy) #----ID# 15, 29, 56 (have to get rid of all since it is rm)

data <- data %>%
  filter(!iguanaID %in% c("15", '29', '56'))

# visualization
ggboxplot(data, x = "abx", y = "igy", color = "time", facet.by = "lps")
ggboxplot(data, x = "lps", y = "igy", color = "time", facet.by = "abx")
ggboxplot(data, x = "time", y = "igy", color = "abx")
data$time <- factor(data$time, levels = c('jan24igy', 'jan26igy','feb1igy','feb8igy'))
ggboxplot(data, x = "time", y = "igy")

# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(igy, type = "mean_se")

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(igy) 

ggqqplot(data, "igy", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal 

#---------Original way I did it------------------------------------
sat = anova_test(
  data = data, 
  igy ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# main effect of abx, time, lps
data %>%
  pairwise_t_test(igy ~ abx, p.adjust.method = "bonferroni") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(abx) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "abx", y = "igy")

data %>%
  pairwise_t_test(igy ~ time, p.adjust.method = "bonferroni") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(time) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "time", y = "igy")

#----------Use Karen's method (combina abx and time into 1 variable)-----------------
# add new column
data2 <- data %>%
  unite(tx_time, abx, time, lps, sep = "", remove = FALSE)
View(data)

# ANOVA
sat2 = anova_test(
  data = data2, 
  igy ~ tx_time,
  wid = iguanaID
)
get_anova_table(sat)

model <- lm(igy ~ abxtime, data = data2)
summary(model)
emmeans(model, list(pairwise ~ abxtime), adjust = "tukey") 
options(max.print=1000000)
data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")


a <- lm(igy ~ abx  * time * lps + (1|iguanaID), data = data)
Anova(a)
data$iguanaID <- as.numeric(data$iguanaID)
b <- lm(igy ~ tx_time + (1|iguanaID), data = data2)
Anova(b)
data2$iguanaID <- as.numeric(data2$iguanaID)
lrtest(a,b)
