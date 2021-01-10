# Eric Cartaya

library(tidyverse) #declarations
library(psych)
library(olsrr)
library(rrcov)
library(HDtest)
library(cluster)
library(fpc)
library(ggplot2)

corona_mental <- read.csv("F:/School/Multivariate Stat/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")#getting the file
#View(corona_mental)#viewing the table

corona_mental <- corona_mental %>% select(-Quartile.range) %>% select(-State) %>% select(-Confidence.Interval) %>% select(-Week.Label)

depression_table <- corona_mental %>% filter(Indicator == "Symptoms of Depressive Disorder") %>% select(-Indicator)# splitting each of the conditions into their own tables, we will be looking at these seperately 

# Depression
dep_nation_table <- depression_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the depression table by all of the relevant indicators
dep_age_table <- depression_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)
dep_sex_table <- depression_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
dep_race_table <- depression_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
dep_ed_table <- depression_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)
#dep_sta_table <- depression_table %>% filter(Group == "By State") %>% select(-Group, -Low.CI, -High.CI)

dep_age_table2 <- dep_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
dep_sex_table2 <- dep_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
dep_race_table2 <- dep_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
dep_ed_table2 <- dep_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
#dep_sta_table2 <- dep_sta_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

dep_age_table3 <- dep_age_table2 %>% scale()
dep_sex_table3 <- dep_sex_table2 %>% scale()
dep_race_table3 <- dep_race_table2 %>% scale()
dep_ed_table3 <- dep_ed_table2 %>% scale()

anxiety_table <- corona_mental %>% filter(Indicator == "Symptoms of Anxiety Disorder") %>% select(-Indicator)

# Anxiety
anx_nation_table <- anxiety_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the anxiety table by all of the relevant indicators
anx_age_table <- anxiety_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)# %>% mutate(age_ind = case_when(`Subgroup` == "18 - 29 years" ~ 25, `Subgroup` == "30 - 39 years" ~ 35),`Subgroup` == "40 - 49 years" ~ 45,`Subgroup` == "50 - 59 years" ~ 55,`Subgroup` == "30 - 39 years" ~ 35)
anx_sex_table <- anxiety_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
anx_race_table <- anxiety_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
anx_ed_table <- anxiety_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)
#anx_sta_table <- anxiety_table %>% filter(Group == "By State") %>% select(-Group, -Low.CI, -High.CI)

anx_age_table2 <- anx_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
anx_sex_table2 <- anx_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
anx_race_table2 <- anx_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
anx_ed_table2 <- anx_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
#anx_sta_table2 <- anx_sta_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

anx_age_table3 <- anx_age_table2 %>% scale()
anx_sex_table3 <- anx_sex_table2 %>% scale()
anx_race_table3 <- anx_race_table2 %>% scale()
anx_ed_table3 <- anx_ed_table2 %>% scale()

or_table <- corona_mental %>% filter(Indicator == "Symptoms of Anxiety Disorder or Depressive Disorder") %>% select(-Indicator)

# Depression or Anxiety
or_nation_table <- or_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the or table by all of the relevant indicators
or_age_table <- or_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)
or_sex_table <- or_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
or_race_table <- or_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
or_ed_table <- or_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)

or_age_table2 <- or_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
or_sex_table2 <- or_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_race_table2 <- or_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_ed_table2 <- or_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

anx_age_table3 <- anx_age_table2 %>% scale()
anx_sex_table3 <- anx_sex_table2 %>% scale()
anx_race_table3 <- anx_race_table2 %>% scale()
anx_ed_table3 <- anx_ed_table2 %>% scale()