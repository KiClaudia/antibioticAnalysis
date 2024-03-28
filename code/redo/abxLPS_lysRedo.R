# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - Lysis? 

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
  select(jan24lys, jan26lys, feb1lys, feb8lys, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "lysis", jan24lys, jan26lys, feb1lys, feb8lys) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# add new column

data <- data %>%
  unite(txtime, abx, time, lps, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "txtime", y = "lysis")

# summary stats
data %>%
  group_by(time) %>%
  get_summary_stats(agg, type = "median")
str(data)
data$txtime <- as.factor(data$txtime)
levels(data$txtime)

# kruskal wallis is needed (transformation doesn't work, no other distribution matches this)
kruskal.test(lysis ~ lps, data = data) #ns
kruskal.test(lysis ~ abx, data = data) #ns
kruskal.test(lysis ~ time, data = data) #ns
kruskal.test(lysis ~ txtime, data =data) #sig but pairwise did not show anything was actually significant
pairwise.wilcox.test(data$lysis, data$txtime,
                     p.adjust.method = "BH")


