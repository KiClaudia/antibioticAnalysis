# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - OSI? 
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
  select(jan20osi, jan24osi, feb8osi, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "OSI", jan20osi, jan24osi, feb8osi) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "OSI", color = "time", facet.by = "lps")


# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(OSI, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(OSI) #----ID#42

data <- data %>%
  filter(!iguanaID %in% c("42"))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(OSI) 

ggqqplot(data, "OSI", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  OSI ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# ns
# not to self: tried with all three time points (jan20 24 feb8), tried with two time points (jan20, feb8), NS both times
###################################################################################
###################################################################################
###################################################################################
# Convert to wide
data <- gi %>%
  select(jan24osi, feb8osi, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "OSI", jan24osi, feb8osi) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)
# add new column
data <- data %>%
  unite(abxtime, abx, time, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "abxtime", y = "OSI", color = "lps")#, facet.by = "lps")

# outliers
data %>%
  group_by(abxtime, lps) %>%
  identify_outliers(OSI) #42
data <- data %>%
  filter(!iguanaID %in% c(42))
data
# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(abxtime, lps) %>%
  shapiro_test(OSI) 

ggqqplot(data, "OSI", ggtheme = theme_bw()) +
  facet_grid(lps + abxtime ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  OSI ~ abxtime * lps,
  wid = iguanaID
)
get_anova_table(sat)
