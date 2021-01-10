or_table <- corona_mental %>% filter(Indicator == "Symptoms of Anxiety Disorder or Depressive Disorder") %>% select(-Indicator) %>% filter(Group != "By State")

# Depression or Anxiety
or_nation_table <- or_table %>% filter(Group == "National Estimate") %>% select(-Group, -Subgroup, -Low.CI, -High.CI)# Dividing the or table by all of the relevant indicators
or_age_table <- or_table %>% filter(Group == "By Age") %>% select(-Group, -Low.CI, -High.CI)
or_sex_table <- or_table %>% filter(Group == "By Gender") %>% select(-Group, -Low.CI, -High.CI)
or_race_table <- or_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% select(-Group, -Low.CI, -High.CI)
or_ed_table <- or_table %>% filter(Group == "By Education") %>% select(-Group, -Low.CI, -High.CI)

shapiro.test(or_age_table$Value)
shapiro.test(or_sex_table$Value)
shapiro.test(or_race_table$Value)
shapiro.test(or_ed_table$Value)

or_age_table2 <- or_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
or_sex_table2 <- or_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_race_table2 <- or_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_ed_table2 <- or_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
#or_sta_table2 <- or_sta_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

shapiro.test(or_age_table2$`18 - 29 years`)
shapiro.test(or_age_table2$`30 - 39 years`)
shapiro.test(or_age_table2$`40 - 49 years`)
shapiro.test(or_age_table2$`50 - 59 years`)
shapiro.test(or_age_table2$`60 - 69 years`)
shapiro.test(or_age_table2$`70 - 79 years`)
shapiro.test(or_age_table2$`80 years and above`)
shapiro.test(or_sex_table2$Female)
shapiro.test(or_sex_table2$Male)
shapiro.test(or_race_table2$`Hispanic or Latino`)
shapiro.test(or_race_table2$`Non-Hispanic Asian, single race`)
shapiro.test(or_race_table2$`Non-Hispanic black, single race`)
shapiro.test(or_race_table2$`Non-Hispanic white, single race`)
shapiro.test(or_race_table2$`Non-Hispanic, other races and multiple races`)
shapiro.test(or_ed_table2$`Bachelor's degree or higher`)
shapiro.test(or_ed_table2$`High school diploma or GED`)
shapiro.test(or_ed_table2$`Less than a high school diploma`)
shapiro.test(or_ed_table2$`Some college/Associate's degree`)

describe(or_nation_table$Value)#describe the combined dataset 
describe(or_age_table$Value)
describe(or_sex_table$Value)
describe(or_race_table$Value)
describe(or_ed_table$Value)

or_age_box <- ggplot(or_age_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Both Depression and Anxiety By Age", x = "Age Groups", y = "Population Percents")#both boxplots
or_sex_box <- ggplot(or_sex_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Both Depression and Anxiety By Sex", x = "Sex Groups", y = "Population Percents")
or_race_box <- ggplot(or_race_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Both Depression and Anxiety By Race", x = "Race Groups", y = "Population Percents")
or_ed_box <- ggplot(or_ed_table, aes(Subgroup, Value)) + geom_boxplot() + labs(title = "Both Depression and Anxiety By Education", x = "Education Groups", y = "Population Percents")

ggarrange(or_age_box, or_sex_box, or_race_box, or_ed_box, ncol = 2, nrow = 2)

ggplot(or_age_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Age", x = "Percentages", y = "Percentage Count")#Both histograms
ggplot(or_sex_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Sex", x = "Percentages", y = "Percentage Count")
ggplot(or_race_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Race", x = "Percentages", y = "Percentage Count")
ggplot(or_ed_table, aes(Value)) + geom_histogram(aes(color=Subgroup)) + labs(title = "Distributions of Percentages in Education", x = "Percentages", y = "Percentage Count")

or_age_scatter <- ggplot(or_age_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Both Depression and Anxiety Percentage Over Time By Age", x = "Week", y = "Percentage") #Both Scatter
or_sex_scatter <- ggplot(or_sex_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Both Depression and Anxiety Percentage Over Time By Sex", x = "Week", y = "Percentage")
or_race_scatter <- ggplot(or_race_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Both Depression and Anxiety Percentage Over Time By Race", x = "Week", y = "Percentage")
or_ed_scatter <- ggplot(or_ed_table, aes(Week, Value)) + geom_point(aes(color = Subgroup)) + labs(title = "Progression of Both Depression and Anxiety Percentage Over Time By Education", x = "Week", y = "Percentage")
#ggplot(or_nation_table, aes(Week, Value)) + geom_point() + labs(title = "Progression of Both Depression and Anxiety Percentage Over Time Nationally", x = "Week", y = "Percentage")

ggarrange(or_age_scatter, or_sex_scatter, or_race_scatter, or_ed_scatter, labels = c("Age", "Sex", "Race", "Education"), ncol = 2, nrow = 2)

or_nat_model <- glm(Value ~ Week, data = or_nation_table)
summary(or_nat_model)
layout(matrix(c(1,2,3,4),2,2))
plot(or_nat_model)
or_nat_model <- lm(Value ~ Week, data = or_nation_table)
summary(or_nat_model)
confint(or_nat_model, level = 0.95)

or_age_model <- glm(Value ~ Week + Subgroup, data = or_age_table)
summary(or_age_model)
layout(matrix(c(1,2,3,4),2,2))
plot(or_age_model)
or_age_model <- lm(Value ~ Week + Subgroup, data = or_age_table)
summary(or_age_model)
confint(or_age_model, level = 0.95)

or_sex_model <- lm(Value ~ Week + Subgroup, data = or_sex_table)
summary(or_sex_model)
layout(matrix(c(1,2,3,4),2,2))
plot(or_sex_model)
or_sex_model <- lm(Value ~ Week + Subgroup, data = or_sex_table)
summary(or_sex_model)
confint(or_sex_model, level = 0.95)

or_race_model <- glm(Value ~ Week + Subgroup, data = or_race_table)
summary(or_race_model)
layout(matrix(c(1,2,3,4),2,2))
plot(or_race_model)
or_race_model <- lm(Value ~ Week + Subgroup, data = or_race_table)
summary(or_race_model)
confint(or_race_model, level = 0.95)

or_ed_model <- glm(Value ~ Week + Subgroup, data = or_ed_table)
summary(or_ed_model)
layout(matrix(c(1,2,3,4),2,2))
plot(or_ed_model)
or_ed_model <- lm(Value ~ Week + Subgroup, data = or_ed_table)
summary(or_ed_model)
confint(or_ed_model, level = 0.95)

lrtest(or_nat_model, or_age_model)
lrtest(or_nat_model, or_sex_model)
lrtest(or_nat_model, or_race_model)
lrtest(or_nat_model, or_ed_model)

or_age_table2 <- or_age_table %>% spread(key = Subgroup, value = Value) %>% select(-Week) #Mutated dataset for multivariate analysis
or_sex_table2 <- or_sex_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_race_table2 <- or_race_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)
or_ed_table2 <- or_ed_table %>% spread(key = Subgroup, value = Value) %>% select(-Week)

describe(or_age_table2)#describe the combined dataset 
describe(or_sex_table2)
describe(or_race_table2)
describe(or_ed_table2)

ols_test_bartlett(or_age_table2, '18 - 29 years', '30 - 39 years', '40 - 49 years', '50 - 59 years', '60 - 69 years', '70 - 79 years', '80 years and above')#heteroskedaciticity across combined variables
ols_test_bartlett(or_sex_table2, 'Male', 'Female')
ols_test_bartlett(or_race_table2, 'Hispanic or Latino', 'Non-Hispanic white, single race', 'Non-Hispanic black, single race', 'Non-Hispanic Asian, single race', 'Non-Hispanic, other races and multiple races')
ols_test_bartlett(or_ed_table2, 'Less than a high school diploma', 'High school diploma or GED', 'Some college/Associate\'s degree', 'Bachelor\'s degree or higher')

T2.test(or_age_table2)# Hotelling's T2 test
T2.test(or_sex_table2)
T2.test(or_race_table2)
T2.test(or_ed_table2)

cor(or_age_table2)
cor(or_sex_table2)
cor(or_race_table2)
cor(or_ed_table2)

cov(or_age_table2)
cov(or_sex_table2)
cov(or_race_table2)
cov(or_ed_table2)

#eigen(or_age_table2)

chisq.test(or_age_table2)
chisq.test(or_sex_table2)
chisq.test(or_race_table2)
chisq.test(or_ed_table2)

#confint(or_age_table2)

anova(or_age_model)
aov(Value ~ Week + Subgroup, data = or_age_table)
summary(aov(Value ~ Week + Subgroup, data = or_age_table))
kruskal.test(anova(or_age_model)) 

anova(or_sex_model)
aov(Value ~ Week + Subgroup, data = or_sex_table)
summary(aov(Value ~ Week + Subgroup, data = or_sex_table))
kruskal.test(anova(or_sex_model)) 

anova(or_race_model)
aov(Value ~ Week + Subgroup, data = or_race_table)
summary(aov(Value ~ Week + Subgroup, data = or_race_table))
kruskal.test(anova(or_race_model)) 

anova(or_ed_model)
aov(Value ~ Week + Subgroup, data = or_ed_table)
summary(aov(Value ~ Week + Subgroup, data = or_ed_table))
kruskal.test(anova(or_ed_model)) 

#testCov(or_age_table2$`18 - 29 years`, or_age_table2$`30 - 39 years`, method = "ALL", J = 100)











#or_age_table2a <- or_age_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week) #Mutated dataset for multivariate analysis
#or_sex_table2a <- or_sex_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)
#or_race_table2a <- or_race_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)
#or_ed_table2a <- or_ed_table %>% spread(key = Subgroup, value = Value)# %>% select(-Week)

#dfa <- lda(Subgroup ~ Week + Value, data = or_age_table)


or_age_cor <- cor(or_age_table2) #correlation matricies
or_sex_cor <- cor(or_sex_table2)
or_race_cor <- cor(or_race_table2)
or_ed_cor <- cor(or_ed_table2)

pca <- princomp(or_age_table2, cor = TRUE)# Principal Components Analysis
summary(pca)

pca1 <- pca$scores[,1]
pca2 <- pca$scores[,2]
or_age_cor2 <- cbind(pca1, pca2, or_age_cor)
or_age_cor2 <- round(cor(or_age_cor2), digits=3)
or_age_cor2

pca <- princomp(or_sex_table2, cor = TRUE)# Principal Components Analysis
summary(pca)

pca1 <- pca$scores[,1]
#pca2 <- pca$scores[,2]
or_sex_cor2 <- cbind(pca1, or_sex_cor)
or_sex_cor2 <- round(cor(or_sex_cor2), digits=3)
or_sex_cor2

pca <- princomp(or_race_table2, cor = TRUE)# Principal Components Analysis
summary(pca)

pca1 <- pca$scores[,1]
pca2 <- pca$scores[,2]
or_race_cor2 <- cbind(pca1, pca2, or_race_cor)
or_race_cor2 <- round(cor(or_race_cor2), digits=3)
or_race_cor2

pca <- princomp(or_ed_table2, cor = TRUE)# Principal Components Analysis
summary(pca)

pca1 <- pca$scores[,1]
pca2 <- pca$scores[,2]
or_ed_cor2 <- cbind(pca1, pca2, or_ed_cor)
or_ed_cor2 <- round(cor(or_ed_cor2), digits=3)
or_ed_cor2

#plot(pca1, pca2, type="n", main="Figure 5. Principal components plot, Bumpus data.", cex.axis=1.5, cex.lab=1.5)
#text(pca1, pca2, labels=survive)




#Cluster Analysis
or_age_table3 <- or_age_table2
#or_age_table3$Value <- or_age_table$Value %>% scale()

# Determine number of clusters
clun <- (nrow(or_age_table3) - 1) * sum(apply(or_age_table3, 2, var))
for (i in 2:8) 
{
  clun[i] <- sum(kmeans(or_age_table3, centers = i)$withinss)
}
plot(1:8, clun, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(or_age_table3, 3) # 4 cluster solution
# get cluster means
aggregate(or_age_table3, by = list(fit1$cluster), FUN = mean)
# append cluster assignment

# K-Means Clustering with 5 clusters
fit2 <- kmeans(or_age_table3, 5)
aggregate(or_age_table3, by = list(fit2$cluster), FUN = mean)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
clusplot(or_age_table3, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(or_age_table3, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#Cluster Analysis
or_sex_table3 <- or_sex_table2
#or_sex_table3$Value <- or_sex_table$Value %>% scale()

# Determine number of clusters
clun <- (nrow(or_sex_table3) - 1) * sum(apply(or_sex_table3, 2, var))
for (i in 2:8) 
{
  clun[i] <- sum(kmeans(or_sex_table3, centers = i)$withinss)
}
plot(1:8, clun, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(or_sex_table3, 3) # 4 cluster solution
# get cluster means
aggregate(or_sex_table3, by = list(fit1$cluster), FUN = mean)
# append cluster assignment

# K-Means Clustering with 5 clusters
fit2 <- kmeans(or_sex_table3, 5)
aggregate(or_sex_table3, by = list(fit2$cluster), FUN = mean)
#Cluster Analysis
or_race_table3 <- or_race_table2
#or_race_table3$Value <- or_race_table$Value %>% scale()

# Determine number of clusters
clun <- (nrow(or_race_table3) - 1) * sum(apply(or_race_table3, 2, var))
for (i in 2:8) 
{
  clun[i] <- sum(kmeans(or_race_table3, centers = i)$withinss)
}
plot(1:8, clun, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(or_race_table3, 3) # 4 cluster solution
# get cluster means
aggregate(or_race_table3, by = list(fit1$cluster), FUN = mean)
# append cluster assignment

# K-Means Clustering with 5 clusters
fit2 <- kmeans(or_race_table3, 5)
aggregate(or_race_table3, by = list(fit2$cluster), FUN = mean)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
clusplot(or_race_table3, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(or_race_table3, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#Cluster Analysis
or_ed_table3 <- or_ed_table2
#or_ed_table3$Value <- or_ed_table$Value %>% scale()

# Determine number of clusters
clun <- (nrow(or_ed_table3) - 1) * sum(apply(or_ed_table3, 2, var))
for (i in 2:8) 
{
  clun[i] <- sum(kmeans(or_ed_table3, centers = i)$withinss)
}
plot(1:8, clun, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(or_ed_table3, 3) # 4 cluster solution
# get cluster means
aggregate(or_ed_table3, by = list(fit1$cluster), FUN = mean)
# append cluster assignment

# K-Means Clustering with 5 clusters
fit2 <- kmeans(or_ed_table3, 5)
aggregate(or_ed_table3, by = list(fit2$cluster), FUN = mean)













ggarrange(age_clus1, age_clus2, ncol = 2, nrow = 1)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(or_age_table3, fit1$cluster)
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)
plotcluster(or_age_table3, fit2$cluster)
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)
