# Eric Cartaya

library(dplyr)
library(tidyverse) #declarations
library(psych)
library(olsrr)
library(rrcov)
library(ggpubr)
library(HDtest)
library(cluster)
library(lmtest)
library(fpc)
library(ggplot2)

corona_mental <- read.csv("F:/School/Multivariate Stat/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")#getting the file
#View(corona_mental)#viewing the table

corona_mental <- corona_mental %>% select(-Quartile.range) %>% select(-State) %>% select(-Confidence.Interval) %>% select(-Week.Label)


#library(MASS)
write.csv(or_table,"F:\\School\\Multivariate Stat\\temp.csv", row.names = TRUE)
