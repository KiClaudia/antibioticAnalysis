# 4/19/2023
# Question: Effect of antibiotics and LPS on phys variables - igy? 
# Method: RM - three way ANOVA, compare across treatment (clind, pen, ctrl), LPS (L or P) from time period (jan24, jan26, feb1, feb8)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_igy.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")

str(gi)
gi$iguanaID <- as.character(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

# convert to wide
data <- gi %>%
  select(jan24igy, jan26igy, feb1igy, feb8igy, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "igy", jan24igy, jan26igy, feb1igy, feb8igy) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()

# outliers
data %>%
  group_by(time, abx, lps) %>%
  identify_outliers(igy) #----ID# 15, 29, 56 (have to get rid of all since it is rm)

data <- data %>%
  filter(!iguanaID %in% c("15", '29', '56'))

# visualization
ggboxplot(data, x = "abx", y = "igy", color = "time", facet.by = "lps")
ggboxplot(data, x = "lps", y = "igy", color = "time", facet.by = "abx")
ggboxplot(data, x = "time", y = "igy", color = "abx")
# summary stats
data %>%
  group_by(time, abx, lps) %>%
  get_summary_stats(igy, type = "mean_se")

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(time, abx, lps) %>%
  shapiro_test(igy) 

ggqqplot(data, "igy", ggtheme = theme_bw()) +
  facet_grid(lps + abx ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  igy ~ abx  * time * lps,
  wid = iguanaID
)
get_anova_table(sat)

# main effect of abx, time, lps
data %>%
  pairwise_t_test(igy ~ abx, p.adjust.method = "bonferroni") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(abx) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "abx", y = "igy")

data %>%
  pairwise_t_test(igy ~ time, p.adjust.method = "bonferroni") # control is different from both antibiotics (antibiotics higher)
data %>%
  group_by(time) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "time", y = "igy")
#######################
data$time <- factor(data$time, levels = c('jan24igy', 'jan26igy','feb1igy','feb8igy'))
ggboxplot(data, x = "time", y = "igy")

#######################
data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")
ggboxplot(data, x = "lps", y = "igy")

# post hoc time:lps  (we are going to isloate time first and then isolate LPS)------------------
ggboxplot(data, x = "lps", y = "igy", color = "time")

dayjan24 <- data %>% 
  filter (time == "jan24igy") 
intxnjan24 <- aov(igy ~ lps
                  , data = dayjan24)
summary(intxnjan24) # on Jan24, there is no diff between LPS and Ctrl group in terms of igy

dayjan26 <- data %>% 
  filter (time == "jan26igy") 
intxnjan26 <- aov(igy ~ lps
                  , data = dayjan26)
summary(intxnjan26) # on Jan26, there is no diff between LPS and Ctrl group in terms of igy

dayfeb1 <- data %>% 
  filter (time == "feb1igy") 
intxnfeb1 <- aov(igy ~ lps
                  , data = dayfeb1)
summary(intxnfeb1) # on feb1, SIGNIFICANT LPS group has higher Igy than PBS group
dayfeb1 %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")

dayfeb8 <- data %>% 
  filter (time == "feb8igy") 
intxnfeb8 <- aov(igy ~ lps
                 , data = dayfeb8)
summary(intxnfeb8) # on feb8, SIGNIFICANT LPS group has higher Igy than PBS group
dayfeb8 %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")
###
data %>% 
  filter (lps == "L") %>%
  aov(igy ~ time, data=.) %>%
  summary() # In  just the LPS group, january days are less than february days

data %>%
  group_by(lps,time) %>%
  get_summary_stats(igy, type = "mean_se")

data %>% 
  filter (lps == "P") %>%
  aov(igy ~ time, data=.) %>%
  summary() # In  just the PBS group, january days are less than february days

data %>%
  group_by(lps,time) %>%
  get_summary_stats(igy, type = "mean_se")

###########################################################
###########################################################
###########################################################
# convert to wide
data <- gi %>%
  select(jan24igy, jan26igy, feb1igy, feb8igy, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "igy", jan24igy, jan26igy, feb1igy, feb8igy) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()

# add new column
data <- data %>%
  unite(abxtime, abx, time, sep = "", remove = FALSE)
View(data)

# visualization
ggboxplot(data, x = "abxtime", y = "igy", color = "lps")#, facet.by = "lps")

# outliers
data %>%
  group_by(abxtime, lps) %>%
  identify_outliers(igy) #15, 29, 56
data <- data %>%
  filter(!iguanaID %in% c('15', '29', '56'))
data
# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data %>%
  group_by(abxtime, lps) %>%
  shapiro_test(igy) 

ggqqplot(data, "igy", ggtheme = theme_bw()) +
  facet_grid(lps + abxtime ~ time, labeller = "label_both")    # normal enough

# ANOVA
sat = anova_test(
  data = data, 
  igy ~ abxtime * lps,
  wid = iguanaID
)
get_anova_table(sat)

model <- lm(igy ~ abxtime*lps, data = data)
summary(model)
emmeans(model, list(pairwise ~ abxtime), adjust = "tukey")

data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")


