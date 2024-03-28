gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_with_OSI.csv")
View(gi) 

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("ggrepel")

str(gi)
gi$iguanaID <- as.numeric(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$abx <- as.factor(gi$abx)
gi$lps <- as.factor(gi$lps)
str(gi)

# convert to wide
data <- gi %>%
  select(jan24osi, feb8osi, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "OSI", jan24osi, feb8osi) %>%
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
data$iguanaID <- as.numeric(data$iguanaID)
model1 <- lm(data = data, OSI ~ abx * lps * time + (1|iguanaID))
Anova(model1)
# ns
###################################################################################
###################################################################################
###################################################################################
# add new column
data2 <- data %>%
  unite(txtime, abx, lps, time, sep = "", remove = FALSE)
View(data2)

# visualization
ggboxplot(data2, x = "txtime", y = "OSI", color = "lps")#, facet.by = "lps")

# outliers
data2 %>%
  group_by(txtime) %>%
  identify_outliers(OSI) 

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data2 %>%
  group_by(txtime, lps) %>%
  shapiro_test(OSI) 

ggqqplot(data2, "OSI", ggtheme = theme_bw()) +
  facet_grid(txtime ~ time, labeller = "label_both")    # normal enough

# ANOVA
model2 <- lm(data = data2, OSI ~ txtime + (1|iguanaID))
Anova(model2)
