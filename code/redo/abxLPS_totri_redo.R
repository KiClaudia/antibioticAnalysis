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
  select(jan24totri, feb8totri, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "totri", jan24totri, feb8totri) %>%
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

data$iguanaID <- as.numeric(data$iguanaID)
# ANOVA
mod1 <- lm(data = data, totri~abx*lps*time+(1|iguanaID))
Anova(mod1)
summary(mod1)
# main effect of time

data %>%
  group_by(time) %>%
  get_summary_stats(totri, type = "mean_se")

###########################################################
###########################################################
###########################################################

# add new column
data2 <- data %>%
  unite(txtime, abx, lps, time, sep = "", remove = FALSE)
View(data2)

# visualization
ggboxplot(data2, x = "txtime", y = "totri", color = "lps")#, facet.by = "lps")

ggplot(data2=data2, aes(x=time, y=totri)) +
  geom_line()+
  geom_point()
# outliers
data2 %>%
  group_by(txtime, lps) %>%
  identify_outliers(totri) #57, 64, 65

# normality (use shapiro test because sample size is smaller than 50, n = ~24)
data2 %>%
  group_by(txtime, lps) %>%
  shapiro_test(totri) 

ggqqplot(data2, "totri", ggtheme = theme_bw()) +
  facet_grid(txtime ~ time, labeller = "label_both")    # normal enough

# ANOVA

model2 <- lm(totri ~ txtime + (1|iguanaID), data = data2)
Anova(model2)
emmeans(model2, list(pairwise ~ txtime), adjust = "tukey", data = data2)

data %>%
  group_by(abxtime) %>%
  get_summary_stats(totri, type = "mean_se")

