# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - Lysis? 
# Method: RM - three way ANOVA, compare across treatment (clind, pen, ctrl), LPS (L or P) from time period (jan24, jan26, feb1, feb8)

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
  select(jan24lys, jan26lys, feb1lys, feb8lys, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "lysis", jan24lys, jan26lys, feb1lys, feb8lys) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "lysis", color = "time", facet.by = "lps")
ggboxplot(data, x = "abx", y = "lysis", color = "lps", facet.by = "time")
ggboxplot(data, x = "abx", y = "lysis", color = "lps")
# summary stats
data %>%
  group_by(time,abx) %>%
  get_summary_stats(lysis, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(lysis) #----ID# 57, 56, 18,54,16,65

data <- data %>%
  filter(!iguanaID %in% c("57", '56', "18", "54", "16", "65"))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(lysis) 

hist(data$lysis) # skewed

# kruskal wallis is needed (transformation doesn't work, no other distribution matches this)

#------- 24hr - Baseline -------
data2 <- gi %>%
  mutate(score = jan26lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, jan26lys) %>%
  na.exclude()
View(data2)

# graph
ggboxplot(data2, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data2) # ns
kruskal.test(score ~ abx, data = data2) # ns
kruskal.test(score ~ tx, data = data2) # ns

#------- 1week - Baseline -------
data3 <- gi %>%
  mutate(score = feb1lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, feb1lys, jan24lys) %>%
  na.exclude()
View(data3)

# graph
ggboxplot(data3, x = "abx", y = "score")

# test
kruskal.test(score ~ lps, data = data3) # ns
kruskal.test(score ~ abx, data = data3) # ns
kruskal.test(score ~ tx, data = data3) # ns

#------- 2week - Baseline -------
data4 <- gi %>%
  mutate(score = feb8lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, feb8lys, jan24lys) %>%
  na.exclude()
View(data4)

# graph
ggboxplot(data4, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data4) # ns
kruskal.test(score ~ abx, data = data4) # ns
kruskal.test(score ~ tx, data = data4) # ns

#------- 1week - 24hr -------
data5 <- gi %>%
  mutate(score = feb1lys - jan26lys) %>%
  select(score, iguanaID, abx, tx, lps, feb1lys, jan26lys) %>%
  na.exclude()
View(data5)

# graph
ggboxplot(data5, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data5) # ns
kruskal.test(score ~ abx, data = data5) # ns
kruskal.test(score ~ tx, data = data5) # ns

#------- 2week - 24hr -------
data6 <- gi %>%
  mutate(score = feb8lys - jan26lys) %>%
  select(score, iguanaID, abx, tx, lps, feb8lys, jan26lys) %>%
  na.exclude()
View(data6)

# graph
ggboxplot(data6, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data6) # ns
kruskal.test(score ~ abx, data = data6) # ns
kruskal.test(score ~ tx, data = data6) # ns

#------- Compare Abx effect on LPS (do all LPS then do all PBS) still with time diff--------
#--------------- 24hr-baseline
abxL <- gi %>%
  mutate(score = jan26lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, jan26lys) %>%
  filter(lps == "P")%>%
  na.exclude()
View(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns

# --------------1week-baseline
abxL <- gi %>%
  mutate(score = feb1lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, feb1lys) %>%
  filter(lps == "P")%>%
  na.exclude()
head(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns

#----------------- 2week-baseline
abxL <- gi %>%
  mutate(score = feb8lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, feb8lys) %>%
  filter(lps == "L")%>%
  na.exclude()
head(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns for p, p=0.086 for L

pairwise.wilcox.test(abxL$score, abxL$abx) #L is not significant

#------- Compare LPS effect on Abx (do all C, then P, then W) still with time diff-----------

#-------- 24hr-baseline
newdat <- gi %>%
  mutate(score = jan26lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, jan26lys) %>%
  filter(abx == "W")%>%
  na.exclude()
head(newdat)

# graph
ggboxplot(newdat, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat) # ns 

#----------- 1week-baseline
newdat2 <- gi %>%
  mutate(score = feb1lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, feb1lys) %>%
  filter(abx == "C")%>%
  na.exclude()
head(newdat2)

# graph
ggboxplot(newdat2, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat2) # ns 

#----------- 2week-baseline
newdat3 <- gi %>%
  mutate(score = feb8lys - jan24lys) %>%
  select(score, iguanaID, abx, tx, lps, jan24lys, feb8lys) %>%
  filter(abx == "W")%>%
  na.exclude()
head(newdat3)

# graph
ggboxplot(newdat3, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat3) # ns for all but water is 0.0711

newdat3 %>%
  group_by(lps,abx) %>%
  get_summary_stats(score, type = "mean_se")


#------- Compare Abx effect at time points alone, not taking difference----------

# Baseline
kruskal.test(data = gi, jan24lys~abx) # ns
kruskal.test(data = gi, jan24lys~lps) # ns
kruskal.test(data = gi, jan24lys~tx) # ns
# 24 hour
kruskal.test(data = gi, jan26lys~abx) # ns
kruskal.test(data = gi, jan26lys~lps) # ns
kruskal.test(data = gi, jan26lys~tx) # ns
# 1 week
kruskal.test(data = gi, feb1lys~abx) # ns
kruskal.test(data = gi, feb1lys~lps) # ns
kruskal.test(data = gi, feb1lys~tx) # ns
# 2 week
kruskal.test(data = gi, feb8lys~abx) # ns
kruskal.test(data = gi, feb8lys~lps) # ns
kruskal.test(data = gi, feb8lys~tx) # p = 0.011
pairwise.wilcox.test(gi$feb8lys, gi$tx) #wl and cl p = 0.073
gi %>%
  group_by(tx) %>%
  get_summary_stats(feb8lys, type = "mean_se")

