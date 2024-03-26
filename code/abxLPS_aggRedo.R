# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - Agglutination? 

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
  select(jan24agg, jan26agg, feb1agg, feb8agg, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "agg", jan24agg, jan26agg, feb1agg, feb8agg) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# add new column
data <- data %>%
  unite(abxtime, abx, time, sep = "", remove = FALSE)
View(data)
data <- data %>%
  unite(all, abx, time, lps, sep = "", remove = FALSE)
View(data)
# visualization
ggboxplot(data, x = "abxtime", y = "agg")

# summary stats
data %>%
  group_by(time) %>%
  get_summary_stats(agg, type = "median")

levels(data$abxtime)

# kruskal wallis is needed (transformation doesn't work, no other distribution matches this)
kruskal.test(agg ~ lps, data = data)
kruskal.test(agg ~ abx, data = data)
kruskal.test(agg ~ time, data = data)
pairwise.wilcox.test(data$agg, data$time,
                     p.adjust.method = "BH")

kruskal.test(agg ~ abxtime, data =data)
pairwise.wilcox.test(data$agg, data$abxtime,
                     p.adjust.method = "BH")


kruskal.test(agg ~ all, data =data)
pairwise.wilcox.test(data$agg, data$all,
                     p.adjust.method = "BH")


