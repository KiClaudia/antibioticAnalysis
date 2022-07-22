# 7/21/2022 
# Question: Effect of antibiotics on phys variables - OSI? 
# Method: RM - two way ANOVA, compare across treatment (clind, pen, ctrl) from january 20 to january 24

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
  select(jan20osi, jan24osi, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "OSI", jan20osi, jan24osi) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "time", y = "OSI", color = "abx")


# summary stats
data %>%
  group_by(time, abx) %>%
  get_summary_stats(OSI, type = "mean_se")

# outliers
data %>%
  group_by(time, abx) %>%
  identify_outliers(OSI) #----- is.extreme ID #53

data <- data %>%
  filter(!iguanaID %in% c("53"))
View(data)

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx) %>%
  shapiro_test(OSI) # not normal

ggqqplot(data, "OSI", ggtheme = theme_bw()) +
  facet_grid(time ~ abx, labeller = "label_both") # normal enough

hist(sqrt(data$OSI)) 

# ANOVA
sat = anova_test(
  data = data, 
  OSI ~ abx  *time,
  wid = iguanaID
)
get_anova_table(sat)

# not significant