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
  unite(txtime, abx, time, lps, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "txtime", y = "agg")

# summary stats
data %>%
  group_by(time) %>%
  get_summary_stats(agg, type = "median")

levels(data$txtime)
data$txtime <- as.factor(data$txtime)

# kruskal wallis is needed (transformation doesn't work, no other distribution matches this)
kruskal.test(agg ~ lps, data = data) #ns
kruskal.test(agg ~ abx, data = data) #ns
kruskal.test(agg ~ time, data = data) #sig
pairwise.wilcox.test(data$agg, data$time,
                     p.adjust.method = "BH")

kruskal.test(agg ~ txtime, data =data)
pairwise.wilcox.test(data$agg, data$txtime,
                     p.adjust.method = "BH")


