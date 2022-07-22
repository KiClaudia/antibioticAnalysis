# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - glucose? 
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
  select(jan20glu, jan24glu, feb8glu, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "glucose", jan20glu, jan24glu, feb8glu) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "glucose", color = "time", facet.by = "lps")


# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(glucose, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(glucose) #----ID# 3, 70

data <- data %>%
  filter(!iguanaID %in% c("3", '70'))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(glucose) 

ggqqplot(data, "glucose", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  glucose ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# main effect of time
data %>%
  pairwise_t_test(glucose ~ time, paired = TRUE, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic)

data %>%
  group_by(time) %>%
  get_summary_stats(glucose, type = "mean_se")

# post hoc --> jan20 lower than jan24 and feb8, feb8 and jan24 same i.e. glucose increased throughout the trial

# not to self: tried with all three time points (jan20 24 feb8), tried with two time points (jan20, feb8), same results for both