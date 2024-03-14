# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - Agglutination? 
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

# ------ clean up-------------
# convert to wide
data <- gi %>%
  select(jan24agg, jan26agg, feb1agg, feb8agg, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "agglutination", jan24agg, jan26agg, feb1agg, feb8agg) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "agglutination", color = "time", facet.by = "lps")
ggboxplot(data, x = "abx", y = "agglutination", color = "lps", facet.by = "time")
ggboxplot(data, x = "abx", y = "agglutination", color = "lps")
# summary stats
data %>%
  group_by(time,abx,lps) %>%
  get_summary_stats(agglutination, type = "mean_se")

######################
results <- data %>%
  filter(time == c('jan24agg', 'feb8agg'))
View(results)
results %>%
  group_by(time,lps) %>%
  get_summary_stats(agglutination, type = "median")
######################

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(agglutination) #----ID# 57, 36, 60, 18, 15, 65, 14

data <- data %>%
  filter(!iguanaID %in% c("57", '36', "60", "18", "15", "65", "14"))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(agglutination) 

hist(data$agglutination) # skewed


# kruskal wallis is needed (transformation doesn't work, no other distribution matches this)

#------- 24hr - Baseline -------
data2 <- gi %>%
  mutate(score = jan26agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, jan26agg) %>%
  na.exclude()
head(data2)

# graph
ggboxplot(data2, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data2) # ns
kruskal.test(score ~ abx, data = data2) # ns
kruskal.test(score ~ tx, data = data2) # ns

#------- 1week - Baseline -------
data3 <- gi %>%
  mutate(score = feb1agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, feb1agg, jan24agg) %>%
  na.exclude()
head(data3)

# graph
ggboxplot(data3, x = "abx", y = "score")

# test
kruskal.test(score ~ lps, data = data3) # ns
kruskal.test(score ~ abx, data = data3) # ns
kruskal.test(score ~ tx, data = data3) # ns

#------- 2week - Baseline -------
data4 <- gi %>%
  mutate(score = feb8agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, feb8agg, jan24agg) %>%
  na.exclude()
View(data4)

# graph
ggboxplot(data4, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data4) # p=0.0728
kruskal.test(score ~ abx, data = data4) # ns
kruskal.test(score ~ tx, data = data4) # p=0.0184
pairwise.wilcox.test(data4$score, data4$tx)

#------- 1week - 24hr -------
data5 <- gi %>%
  mutate(score = feb1agg - jan26agg) %>%
  select(score, iguanaID, abx, tx, lps, feb1agg, jan26agg) %>%
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
  mutate(score = feb8agg - jan26agg) %>%
  select(score, iguanaID, abx, tx, lps, feb8agg, jan26agg) %>%
  na.exclude()
View(data6)

# graph
ggboxplot(data6, x = "tx", y = "score")

# test
kruskal.test(score ~ lps, data = data6) #p = 0.0145
kruskal.test(score ~ abx, data = data6) ns
kruskal.test(score ~ tx, data = data6) #p = 0.02989
pairwise.wilcox.test(data6$score, data6$tx)

#------- Compare Abx effect on LPS (do all LPS then do all PBS) still with time diff--------
#--------------- 24hr-baseline
abxL <- gi %>%
  mutate(score = jan26agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, jan26agg) %>%
  filter(lps == "L")%>%
  na.exclude()
View(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns

# --------------1week-baseline
abxL <- gi %>%
  mutate(score = feb1agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, feb1agg) %>%
  filter(lps == "L")%>%
  na.exclude()
head(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns

#----------------- 2week-baseline
abxL <- gi %>%
  mutate(score = feb8agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, feb8agg) %>%
  filter(lps == "P")%>%
  na.exclude()
head(abxL)

# graph
ggboxplot(abxL, x = "abx", y = "score")

# test
kruskal.test(score ~ abx, data = abxL) # ns, pbs marginal sig

pairwise.wilcox.test(abxL$score, abxL$abx) #L is not significant

#------- Compare LPS effect on Abx (do all C, then P, then W) still with time diff-----------

#-------- 24hr-prelps
newdat <- gi %>%
  mutate(score = jan26agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, jan26agg) %>%
  filter(abx == "W")%>% #change P C W here to see each result
  na.exclude()
head(newdat)

# graph
ggboxplot(newdat, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat) # ns 

#----------- 1week-prelps
newdat2 <- gi %>%
  mutate(score = feb1agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, feb1agg) %>%
  filter(abx == "C")%>%
  na.exclude()
head(newdat2)

# graph
ggboxplot(newdat2, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat2) # ns 

#----------- 2week-prelps
newdat3 <- gi %>%
  mutate(score = feb8agg - jan24agg) %>%
  select(score, iguanaID, abx, tx, lps, jan24agg, feb8agg) %>%
  filter(abx == "W")%>%
  na.exclude()
head(newdat3)

# graph
ggboxplot(newdat3, x = "lps", y = "score")

# test
kruskal.test(score ~ lps, data = newdat3) #  water is 0.0018 and P is 0.077

newdat3 %>%
  group_by(lps) %>%
  get_summary_stats(score, type = "mean_se")


#------- Compare Abx effect at time points alone, not taking difference----------

# Baseline
kruskal.test(data = gi, jan24agg~abx) # ns
kruskal.test(data = gi, jan24agg~lps) # ns
kruskal.test(data = gi, jan24lys~tx) # ns
# 24 hour
kruskal.test(data = gi, jan26agg~abx) # p=0.0937, post hoc not sig
kruskal.test(data = gi, jan26agg~lps) # ns
kruskal.test(data = gi, jan26agg~tx) # ns

pairwise.wilcox.test(gi$jan26agg, gi$abx) 

# 1 week
kruskal.test(data = gi, feb1agg~abx) # ns
kruskal.test(data = gi, feb1agg~lps) # ns
kruskal.test(data = gi, feb1agg~tx) # ns
# 2 week
kruskal.test(data = gi, feb8agg~abx) # ns
kruskal.test(data = gi, feb8agg~lps) # p= 0.0079
kruskal.test(data = gi, feb8agg~tx) # p = 0.0031
pairwise.wilcox.test(gi$feb8agg, gi$tx) 
gi %>%
  group_by(lps) %>%
  get_summary_stats(feb8agg, type = "mean_se")


