B = 10000
seed = 26111992

data_variation$cluster_3 <- as.factor(data_variation$cluster_3)

data_variation$cluster_5 <- as.factor(data_variation$cluster_5)

#fit the model for percentage
fit <- aov(data_variation$`shift_both_%` ~ data_variation$cluster_3)
summary(fit)

fit <- aov(data_variation$`shift_males_%` ~ data_variation$cluster_3)
summary(fit)

fit <- aov(data_variation$`shift_females_%` ~ data_variation$cluster_3)
summary(fit)

#fit the model for raw variable
fit <- aov(data_variation$`shift_both` ~ data_variation$cluster_3)
summary(fit)

fit <- aov(data_variation$`shift_males` ~ data_variation$cluster_3)
summary(fit)

fit <- aov(data_variation$`shift_females` ~ data_variation$cluster_3)
summary(fit)

#fit the model for percentage with 5 clusters
fit <- aov(data_variation$`shift_both_%` ~ data_variation$cluster_5)
summary(fit)

fit <- aov(data_variation$`shift_males_%` ~ data_variation$cluster_5)
summary(fit)

fit <- aov(data_variation$`shift_females_%` ~ data_variation$cluster_5)
summary(fit)

#fit the model for raw variable with 5 clusters
fit <- aov(data_variation$`shift_both` ~ data_variation$cluster_5)
summary(fit)

fit <- aov(data_variation$`shift_males` ~ data_variation$cluster_5)
summary(fit)

fit <- aov(data_variation$`shift_females` ~ data_variation$cluster_5)
summary(fit)

#let's do a anova test on shift_both_%
fit <- aov(data_variation$`shift_both_%` ~ data_variation$cluster_3)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

T_stat <- numeric(B) 
n <- dim(data_variation)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  clust_perm <- data_variation$cluster_3[permutation]
  fit_perm <- aov(data_variation$`shift_both_%` ~ clust_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, )
abline(v=T0,col=2,lwd=2)

p_val = sum(T_stat>T0)/B
p_val
