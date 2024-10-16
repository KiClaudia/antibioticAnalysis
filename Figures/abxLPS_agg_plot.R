#Antibiotics IgY line graph
library(ggplot2)
library(tidyverse)
library(rstatix)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_igy.csv")
df <- gi %>%
  select(jan24agg, jan26agg, feb1agg, feb8agg, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA",jan24agg, jan26agg, feb1agg, feb8agg) %>%
  convert_as_factor(iguanaID, time) %>%
  na.exclude()
View(df)

df2 <- data.frame(df %>%
                    group_by(tx, time) %>%
                    get_summary_stats(BKA, type = "mean_se")) %>%
  rename("Treatments" = tx)
head(df2)
str(df2)
df2$Treatments <- as.factor(df2$Treatments)
levels(df2$Treatments) <- list("Clindamycin-LPS" = "cl",        
                               "Clindamycin-PBS" = "cp",
                               "Penicillin-LPS" = "pl",
                               "Penicillin-PBS" = "pp",
                               "Water-LPS" = "wl",
                               "Water-PBS" = "wp")
head(df2)
# make new column that will describe the position in time of our points (slightly off to make it easier to read labels)

df2 <- df2 %>% 
  mutate(timenum = str_replace(time, "jan24agg", "0")) %>%
  mutate(timenum = str_replace(timenum, "jan26agg", "2")) %>%
  mutate(timenum = str_replace(timenum, "feb1agg", "8")) %>%
  mutate(timenum = str_replace(timenum, "feb8agg", "13"))

# tell R that it is a number so it can work on a continuous scale
View(df2)
str(df2)
df2$timenum <- as.numeric(df2$timenum)

# plot

df2 <- df2 %>%
  mutate(positionName = str_replace(time, "jan24agg", "Post-Antibiotics")) %>%
  mutate(positionName = str_replace(positionName, "jan26agg", "24hr Post-injection")) %>%
  mutate(positionName = str_replace(positionName, "feb1agg", "1week Post-injection")) %>%
  mutate(positionName = str_replace(positionName, "feb8agg", "2week Post-injection"))  


# plot


ggplot(data = df2, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2) +
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1) +
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,6), name = "Agglutination Score") +
  scale_x_continuous(breaks = df2$timenum,labels = df2$positionName, limits = c(0,14), 
                     name = "Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30,size = 8, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("#CC0000", "#CC0000", "#CC9900", "#CC9900","#33CCFF","#33CCFF"))+
  annotate("text", x = c(3), y=4, colour= "purple", label = c("LPS\nChallenge"))+
  geom_vline(xintercept = 1, col = "purple", size = 0.75) 
