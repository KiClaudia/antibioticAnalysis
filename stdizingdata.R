# standardizing OXY and dROM and creating OSI 
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Antibiotic study 2022/antibioticAnalysis/data/AbxMasterData.csv")
View(data)

library(dplyr)
# drom
jan20dromavg <- mean(data$jan20drom, na.rm = TRUE)
jan20dromstd <- sd(data$jan20drom, na.rm = TRUE)
data_fixed <- data %>%
  mutate(jan20drom_std = (jan20drom-jan20dromavg)/jan20dromstd)
View(data_fixed)

jan24dromavg <- mean(data$jan24drom, na.rm = TRUE)
jan24dromstd <- sd(data$jan24drom, na.rm = TRUE)
data_fixed <- data_fixed %>%
  mutate(jan24drom_std = (jan24drom-jan24dromavg)/jan24dromstd)
View(data_fixed)

feb8dromavg <- mean(data$feb8drom, na.rm = TRUE)
feb8dromstd <- sd(data$feb8drom, na.rm = TRUE)
data_fixed <- data_fixed %>%
  mutate(feb8drom_std = (feb8drom-feb8dromavg)/feb8dromstd)
View(data_fixed)

# OXY
jan20oxyavg <- mean(data$jan20oxy, na.rm = TRUE)
jan20oxystd <- sd(data$jan20oxy, na.rm = TRUE)
data_fixed <- data_fixed %>%
  mutate(jan20oxy_std = (jan20oxy-jan20oxyavg)/jan20oxystd)
View(data_fixed)

jan24oxyavg <- mean(data$jan24oxy, na.rm = TRUE)
jan24oxystd <- sd(data$jan24oxy, na.rm = TRUE)
data_fixed <- data_fixed %>%
  mutate(jan24oxy_std = (jan24oxy-jan24oxyavg)/jan24oxystd)
View(data_fixed)

feb8oxyavg <- mean(data$feb8oxy, na.rm = TRUE)
feb8oxystd <- sd(data$feb8oxy, na.rm = TRUE)
data_fixed <- data_fixed %>%
  mutate(feb8oxy_std = (feb8oxy-feb8oxyavg)/feb8oxystd)
View(data_fixed)
