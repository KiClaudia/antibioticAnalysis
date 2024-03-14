# 7/22/2022 
# Question: Effect of antibiotics and LPS on phys variables - totri? 
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
  select(jan20totri, jan24totri, feb8totri, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "totri", jan20totri, jan24totri, feb8totri) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(data)

# visualization
ggboxplot(data, x = "abx", y = "totri", color = "lps", facet.by = "time")


# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(totri, type = "mean_se")

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(totri) #----ID# 57, 64, 65

data <- data %>%
  filter(!iguanaID %in% c("57", '64', "65"))

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(totri) 

ggqqplot(data, "totri", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  totri ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# main effect of time, interaction effect abx*lps

# post hoc time --> jan20 lower than feb8, jan24 lower than feb8, rest is the same
data %>%
  pairwise_t_test(totri ~ time, paired = FALSE, p.adjust.method = "bonferroni") 

data %>%
  group_by(time) %>%
  get_summary_stats(totri, type = "mean_se")

# post hoc abx*lps --> 
ggboxplot(data, x = "abx", y = "totri", color = "lps")

data %>% 
  filter (abx == "C") %>%
  aov(totri ~ lps, data=.) %>%
  summary() # When iguanas are given clindimycin, there are no effects on totri response to LPS

data %>% 
  filter (abx == "P") %>%
  aov(totri ~ lps, data=.) %>%
  summary() # when iguanas are given pennicillin, there is an effect on totri response to LPS p = 0.0028 with the LPS group having higher totri
data %>%
  group_by(abx, lps) %>%
  get_summary_stats(totri, type = "mean_se")



data %>% 
  filter (abx == "W") %>%
  aov(totri ~ lps, data=.) %>%
  summary() # When iguanas are given water, there are no effects on totri response to LPS



# not to self: tried with all three time points (jan20 24 feb8), tried with two time points (jan20, feb8), same results for both