#Antibiotics IgY line graph

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData_igy.csv")
df <- gi %>%
  select(jan24igy, jan26igy, feb1igy, feb8igy, iguanaID, abx, tx, lps) %>%
  gather(key = "time", value = "BKA", jan24igy, jan26igy, feb1igy, feb8igy) %>%
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
  mutate(timenum = str_replace(time, "jan24igy", "0")) %>%
  mutate(timenum = str_replace(timenum, "jan26igy", "2")) %>%
  mutate(timenum = str_replace(timenum, "feb1igy", "5")) %>%
  mutate(timenum = str_replace(timenum, "feb8igy", "7"))

# tell R that it is a number so it can work on a continuous scale
View(df2)
str(df2)
df2$timenum <- as.numeric(df2$timenum)

# plot

ggplot(data = df2, aes(x = factor(time, level=c('jan24igy', 'jan26igy', 'feb1igy', 'feb8igy')), y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 3)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.25) +
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration (mg/mL)") +
  scale_x_discrete(breaks=c('jan24igy', 'jan26igy', 'feb1igy', 'feb8igy'), 
                   name = 'Antibiotics LPS Timeline') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("#CC0000", "#CC0000", "#CC9900", "#CC9900","#33CCFF","#33CCFF"))
