anxiety_table <- corona_mental %>% filter(Indicator == "Symptoms of Anxiety Disorder") %>% select(-Indicator)

# Anxiety
anx_nation_table <- anxiety_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the anxiety table by all of the relevant indicators
anx_age_table <- anxiety_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)# %>% mutate(age_ind = case_when(`Subgroup` == "18 - 29 years" ~ 25, `Subgroup` == "30 - 39 years" ~ 35),`Subgroup` == "40 - 49 years" ~ 45,`Subgroup` == "50 - 59 years" ~ 55,`Subgroup` == "30 - 39 years" ~ 35)
anx_sex_table <- anxiety_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
anx_race_table <- anxiety_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
anx_ed_table <- anxiety_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)
#anx_sta_table <- anxiety_table %>% filter(Group == "By State") %>% select(-Group, -Low.CI, -High.CI)

describe(anx_nation_table$Value)#describe the anxiety dataset   
describe(anx_age_table$Value)
describe(anx_sex_table$Value)
describe(anx_race_table$Value)
describe(anx_ed_table$Value)

ols_test_bartlett(anx_age_table, 'Value', 'Subgroup')#heteroskedaciticity across anxiety
ols_test_bartlett(anx_sex_table, 'Value', 'Subgroup')
ols_test_bartlett(anx_race_table, 'Value', 'Subgroup')
ols_test_bartlett(anx_ed_table, 'Value', 'Subgroup')

ggplot(anx_age_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Anxiety By Age", x = "Age Groups", y = "Population Percents")#Anxiety boxplots
ggplot(anx_sex_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Anxiety By Sex", x = "Sex Groups", y = "Population Percents")
ggplot(anx_race_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Anxiety By Race", x = "Race Groups", y = "Population Percents")
ggplot(anx_ed_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Anxiety By Education", x = "Education Groups", y = "Population Percents")

ggplot(anx_age_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Age", x = "Percentages", y = "Percentage Count")#Anxiety histograms
ggplot(anx_sex_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Sex", x = "Percentages", y = "Percentage Count")
ggplot(anx_race_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Race", x = "Percentages", y = "Percentage Count")
ggplot(anx_ed_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Education", x = "Percentages", y = "Percentage Count")

ggplot(anx_age_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Anxiety Percentage Over Time By Age", x = "Week", y = "Percentage") #Anxiety Scatter
ggplot(anx_sex_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Anxiety Percentage Over Time By Sex", x = "Week", y = "Percentage")
ggplot(anx_race_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Anxiety Percentage Over Time By Race", x = "Week", y = "Percentage")
ggplot(anx_ed_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Anxiety Percentage Over Time By Education", x = "Week", y = "Percentage")
ggplot(anx_nation_table, aes(Week, Value)) + geom_point() + labs(title = "Progression of Anxiety Percentage Over Time Nationally", x = "Week", y = "Percentage")

anx_nat_model <- glm(Value ~ Week, data = anx_nation_table)
summary(anx_nat_model)

anx_age_model <- glm(Value ~ Week + Subgroup, data = anx_age_table)
summary(anx_age_model)

anx_sex_model <- glm(Value ~ Week + Subgroup, data = anx_sex_table)
summary(anx_sex_model)

anx_race_model <- glm(Value ~ Week + Subgroup, data = anx_race_table)
summary(anx_race_model)

anx_ed_model <- glm(Value ~ Week + Subgroup, data = anx_ed_table)
summary(anx_ed_model)

anx_age_table2 <- anx_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
anx_sex_table2 <- anx_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
anx_race_table2 <- anx_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
anx_ed_table2 <- anx_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
#anx_sta_table2 <- anx_sta_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

describe(anx_age_table2)#describe the combined dataset 
describe(anx_sex_table2)
describe(anx_race_table2)
describe(anx_ed_table2)
#describe(anx_sta_table2)

ols_test_bartlett(anx_age_table2, '18 - 29 years', '30 - 39 years', '40 - 49 years', '50 - 59 years', '60 - 69 years', '70 - 79 years', '80 years and above')#heteroskedaciticity across both
ols_test_bartlett(anx_sex_table2, 'Male', 'Female')
ols_test_bartlett(anx_race_table2, 'Hispanic or Latino', 'Non-Hispanic white, single race', 'Non-Hispanic black, single race', 'Non-Hispanic Asian, single race', 'Non-Hispanic, other races and multiple races')
ols_test_bartlett(anx_ed_table2, 'Less than a high school diploma', 'High school diploma or GED', 'Some college/Associate\'s degree', 'Bachelor\'s degree or higher')
#ols_test_bartlett(anx_sta_table2, 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

T2.test(anx_age_table2)# Hotelling's T2 test
T2.test(anx_sex_table2)
T2.test(anx_race_table2)
T2.test(anx_ed_table2)
#T2.test(anx_sta_table2)

cor(anx_age_table2)
cor(anx_sex_table2)
cor(anx_race_table2)
cor(anx_ed_table2)