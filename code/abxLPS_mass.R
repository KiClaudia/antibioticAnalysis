# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - mass? 
# Method: RM - three way ANOVA, compare across treatment (clind, pen, ctrl), LPS (L or P) from time period (jan20, jan24, feb8)

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
  select(jan20mass, jan24mass, feb8mass, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "mass", jan20mass, jan24mass, feb8mass) %>%
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
sat = anova_test(
  data = data, 
  mass ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# ns