depression_table <- corona_mental %>% filter(Indicator == "Symptoms of Depressive Disorder") %>% select(-Indicator)# splitting each of the conditions into their own tables, we will be looking at these seperately 

# Depression
dep_nation_table <- depression_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the depression table by all of the relevant indicators
dep_age_table <- depression_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)
dep_sex_table <- depression_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
dep_race_table <- depression_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
dep_ed_table <- depression_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)
#dep_sta_table <- depression_table %>% filter(Group == "By State") %>% select(-Group, -Low.CI, -High.CI)

shapiro.test(dep_age_table$Value)
shapiro.test(dep_sex_table$Value)
shapiro.test(dep_race_table$Value)
shapiro.test(dep_ed_table$Value)

dep_age_table2 <- dep_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
dep_sex_table2 <- dep_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
dep_race_table2 <- dep_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
dep_ed_table2 <- dep_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
#dep_sta_table2 <- dep_sta_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

shapiro.test(dep_age_table2$`18 - 29 years`)
shapiro.test(dep_age_table2$`30 - 39 years`)
shapiro.test(dep_age_table2$`40 - 49 years`)
shapiro.test(dep_age_table2$`50 - 59 years`)
shapiro.test(dep_age_table2$`60 - 69 years`)
shapiro.test(dep_age_table2$`70 - 79 years`)
shapiro.test(dep_age_table2$`80 years and above`)
shapiro.test(dep_sex_table2$Female)
shapiro.test(dep_sex_table2$Male)
shapiro.test(dep_race_table2$`Hispanic or Latino`)
shapiro.test(dep_race_table2$`Non-Hispanic Asian, single race`)
shapiro.test(dep_race_table2$`Non-Hispanic black, single race`)
shapiro.test(dep_race_table2$`Non-Hispanic white, single race`)
shapiro.test(dep_race_table2$`Non-Hispanic, other races and multiple races`)
shapiro.test(dep_ed_table2$`Bachelor's degree or higher`)
shapiro.test(dep_ed_table2$`High school diploma or GED`)
shapiro.test(dep_ed_table2$`Less than a high school diploma`)
shapiro.test(dep_ed_table2$`Some college/Associate's degree`)

describe(dep_nation_table$Value)#describe the depression dataset 
describe(dep_age_table$Value)
describe(dep_sex_table$Value)
describe(dep_race_table$Value)
describe(dep_ed_table$Value)
#describe(dep_sta_table$Value)

ols_test_bartlett(dep_age_table, 'Value', 'Subgroup')#heteroskedaciticity across depression
ols_test_bartlett(dep_sex_table, 'Value', 'Subgroup')
ols_test_bartlett(dep_race_table, 'Value', 'Subgroup')
ols_test_bartlett(dep_ed_table, 'Value', 'Subgroup')
#ols_test_bartlett(dep_sta_table, 'Value', 'Subgroup')

ggplot(dep_age_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Depression By Age", x = "Age Groups", y = "Population Percents")#depression boxplots
ggplot(dep_sex_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Depression By Sex", x = "Sex Groups", y = "Population Percents")
ggplot(dep_race_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Depression By Race", x = "Race Groups", y = "Population Percents")
ggplot(dep_ed_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Depression By Education", x = "Education Groups", y = "Population Percents")

ggplot(dep_age_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Age", x = "Percentages", y = "Percentage Count")#depression histograms
ggplot(dep_sex_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Sex", x = "Percentages", y = "Percentage Count")
ggplot(dep_race_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Race", x = "Percentages", y = "Percentage Count")
ggplot(dep_ed_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Education", x = "Percentages", y = "Percentage Count")
ggplot(dep_sta_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in States", x = "Percentages", y = "Percentage Count")
#ggplot(dep_sta_table, aes(Value)) + geom_histogram() + labs(title = "Distributions of Percentages in States", x = "Percentages", y = "Percentage Count")

ggplot(dep_age_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Depression Percentage Over Time By Age", x = "Week", y = "Percentage") #depression Scatter
ggplot(dep_sex_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Depression Percentage Over Time By Sex", x = "Week", y = "Percentage")
ggplot(dep_race_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Depression Percentage Over Time By Race", x = "Week", y = "Percentage")
ggplot(dep_ed_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Depression Percentage Over Time By Education", x = "Week", y = "Percentage")
ggplot(dep_nation_table, aes(Week, Value)) + geom_point() + labs(title = "Progression of Depression Percentage Over Time Nationally", x = "Week", y = "Percentage")
#ggplot(dep_sta_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Depression Percentage Over Time By State", x = "Week", y = "Percentage")
#ggplot(dep_sta_table, aes(Week, Value)) + geom_point() + labs(title = "Progression of Depression Percentage Over Time By State", x = "Week", y = "Percentage")

dep_nat_model <- glm(Value ~ Week, data = dep_nation_table)
summary(dep_nat_model)

dep_age_model <- glm(Value ~ Week + Subgroup, data = dep_age_table)
summary(dep_age_model)

dep_sex_model <- glm(Value ~ Week + Subgroup, data = dep_sex_table)
summary(dep_sex_model)

dep_race_model <- glm(Value ~ Week + Subgroup, data = dep_race_table)
summary(dep_race_model)

dep_ed_model <- glm(Value ~ Week + Subgroup, data = dep_ed_table)
summary(dep_ed_model)

#dep_sta_gmodel <- glm(Value ~ Week + Subgroup, data = dep_sta_table)
#summary(dep_sta_gmodel)
#dep_sta_model <- lm(Value ~ Week + Subgroup, data = dep_sta_table)
#summary(dep_sta_model)

Wilks.test(dep_age_table$Subgroup ~ dep_age_table$Week + dep_age_table$Value, dep_age_table)
Wilks.test(dep_sex_table$Subgroup ~ dep_sex_table$Week + dep_sex_table$Value, dep_sex_table)
Wilks.test(dep_race_table$Subgroup ~ dep_race_table$Week + dep_race_table$Value, dep_race_table)
Wilks.test(dep_ed_table$Subgroup ~ dep_ed_table$Week + dep_ed_table$Value, dep_ed_table)

describe(dep_age_table2)#describe the combined dataset 
describe(dep_sex_table2)
describe(dep_race_table2)
describe(dep_ed_table2)
#describe(dep_sta_table2)

ols_test_bartlett(dep_age_table2, '18 - 29 years', '30 - 39 years', '40 - 49 years', '50 - 59 years', '60 - 69 years', '70 - 79 years', '80 years and above')#heteroskedaciticity across both
ols_test_bartlett(dep_sex_table2, 'Male', 'Female')
ols_test_bartlett(dep_race_table2, 'Hispanic or Latino', 'Non-Hispanic white, single race', 'Non-Hispanic black, single race', 'Non-Hispanic Asian, single race', 'Non-Hispanic, other races and multiple races')
ols_test_bartlett(dep_ed_table2, 'Less than a high school diploma', 'High school diploma or GED', 'Some college/Associate\'s degree', 'Bachelor\'s degree or higher')
#ols_test_bartlett(dep_sta_table2, 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

T2.test(dep_age_table2)# Hotelling's T2 test
T2.test(dep_sex_table2)
T2.test(dep_race_table2)
T2.test(dep_ed_table2)
#T2.test(dep_sta_table2)
#HotellingsT2(dep_sta_table2)

cor(dep_age_table2) #correlation matricies
cor(dep_sex_table2)
cor(dep_race_table2)
cor(dep_ed_table2)

cov(dep_age_table2)
cov(dep_sex_table2)
cov(dep_race_table2)
cov(dep_ed_table2)

#eigen(dep_age_table2)

chisq.test(dep_age_table2)
chisq.test(dep_sex_table2)
chisq.test(dep_race_table2)
chisq.test(dep_ed_table2)

#confint(dep_age_table2)

anova(dep_age_model)
aov(Value ~ Week + Subgroup, data = dep_age_table)
summary(aov(Value ~ Week + Subgroup, data = dep_age_table))
kruskal.test(anova(dep_age_model)) 

#testCov(dep_age_table2$`18 - 29 years`, dep_age_table2$`30 - 39 years`, method = "ALL", J = 100)











#dep_age_table2a <- dep_age_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week) #Mutated dataset for multivariate analysis
#dep_sex_table2a <- dep_sex_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)
#dep_race_table2a <- dep_race_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)
#dep_ed_table2a <- dep_ed_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)

#dfa <- lda(Subgroup ~ Week + Value, data = dep_age_table)


dep_age_cor <- cor(dep_age_table2) #correlation matricies
dep_sex_cor <- cor(dep_sex_table2)
dep_race_cor <- cor(dep_race_table2)
dep_ed_cor <- cor(dep_ed_table2)

pca <- princomp(dep_age_table2, cor = TRUE)# Principal Components Analysis
summary(pca)

pca1 <- pca$scores[,1]
pca2 <- pca$scores[,2]
dep_age_cor2 <- cbind(pca1, pca2, dep_age_cor)
dep_age_cor2 <- round(cor(dep_age_cor2), digits=3)
dep_age_cor2

#plot(pca1, pca2, type="n", main="Figure 5. Principal components plot, Bumpus data.", cex.axis=1.5, cex.lab=1.5)
#text(pca1, pca2, labels=survive)




#Cluster Analysis
dep_age_table3 <- dep_age_table2 %>% scale()

# Determine number of clusters
clun <- (nrow(dep_age_table3) - 1) * sum(apply(dep_age_table3, 2, var))
for (i in 2:8) 
{
  clun[i] <- sum(kmeans(dep_age_table3, centers = i)$withinss)
}
plot(1:8, clun, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(dep_age_table3, 3) # 4 cluster solution
# get cluster means
aggregate(dep_age_table3, by = list(fit1$cluster), FUN = mean)
# append cluster assignment
dep_age_table4 <- data.frame(dep_age_table3, fit1$cluster)

# K-Means Clustering with 5 clusters
fit2 <- kmeans(dep_age_table3, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
clusplot(dep_age_table3, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(dep_age_table3, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(dep_age_table3, fit1$cluster)
plotcluster(dep_age_table3, fit2$cluster)

