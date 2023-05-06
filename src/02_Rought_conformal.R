rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

library(mgcv)
library(pbapply)
library(conformalInference)
library(rgl)
library(dbscan)


#Using a conformal approach to predict the smoking tendency in a OECD country
rawdata_f<-read.table("../data/semiprocessed_datasets/smoking_prevalence_f.txt",header=T)
rawdata_m<-read.table("../data/semiprocessed_datasets/smoking_prevalence_m.txt",header=T)
rawdata_b<-read.table("../data/semiprocessed_datasets/smoking_prevalence_b.txt",header=T)

years<-rawdata_f$YEAR..DISPLAY.

y_f_list=c(rawdata_b[,2:37])
y_f<-unlist(y_f_list)
n<-length(y_f)
plot(y_f)

#Selecting grid parameters
grid_factor = 1.5
n_grid = 400
alpha = 0.1

#Creating the test grid
test_grid = seq(0, +grid_factor * max(abs(y_f)),
                length.out = n_grid)
test_grid


########Utils
#Plot p-value function
plot_pval = function(test_grid, pval_fun, pred, alpha) {
  plot(
    test_grid,
    pval_fun,
    type = 'l',
    main = "p-value function",
    xlab = "Test grid",
    ylab = "p-value function"
  )
  abline(h = alpha, lty = 2)
  abline(v = pred, col = 'red')
}
########End utils




## Using T Prediction Intervals
wrapper_full = function(grid_point) {
  aug_y = c(grid_point, y_f)
  mu = mean(aug_y)
  ncm = abs(mu - aug_y)
  sum((ncm[-1] >= ncm[1])) / (n + 1)
}

pval_fun = sapply(test_grid, wrapper_full)
index_in = pval_fun > alpha
pred_t_interval = range(test_grid[index_in])
pred_t_interval 
# FEMALES 6.845729 33.868342 
# MALES 13.13045 46.32820  
# BOTH 12.58985 37.56316

plot_pval(test_grid, pval_fun, pred_t_interval, alpha)





## Using KNN distance
pval_fun = numeric(n_grid)
k = 1 ########RAGIONARE SU COME SETTARE IL K - per ora 
# selezionato uno con una discreta smoothness

wrapper_knn = function(grid_point) {
  aug_y = c(grid_point, y_f)
  ncm = kNNdist(matrix(aug_y), k * n)
  sum((ncm[-1] >= ncm[1])) / (n + 1) #### N B ??
}

pval_fun = sapply(test_grid, wrapper_knn)
index_in = pval_fun > alpha
pred_knn = test_grid[as.logical(c(0, abs(diff(index_in))))]
pred_knn 
# FEMALES 6.485427 33.147739
# MALES 15.85564 59.45865
#BOTH 13.82820 48.70827

#Plot p-value function
plot_pval(test_grid, pval_fun, pred_knn, alpha)




## Using Mahalanobis distance
pval_fun = numeric(n_grid)
wrapper_mal = function(grid_point) {
  aug_y = c(grid_point, y_f)
  ncm = mahalanobis(matrix(aug_y), colMeans(matrix(aug_y)), cov(matrix(aug_y)))
  sum((ncm[-1] >= ncm[1])) / (n + 1)
}

pval_fun = sapply(test_grid, wrapper_mal)
index_in = pval_fun > alpha
pred_mahalanobis = test_grid[as.logical(c(0, abs(diff(index_in))))]
pred_mahalanobis 
#FEMALES 6.845729 34.228643
#MALES 13.13045 46.57594
#BOTH 12.58985 37.76955

#Plot p-value function
plot_pval(test_grid, pval_fun, pred_mahalanobis, alpha)

#Plot histogram of target variable
hist(
  y_f,
  breaks = 15,
  freq = FALSE,
  main = 'Histogram of Smoking prevalence',
  xlab = 'Smoking prevalence',
  ylim= c(0,0.06),
  border = NA
)
lines(density(y_f))

abline(v = jitter(pred_t_interval, amount=0.03), col = 'red', lwd = 2)
abline(v = jitter(pred_mahalanobis, amount=0.03), col = 'orange', lwd = 2)
abline(v = jitter(pred_knn, amount=0.03), col = 'blue', lwd = 2)

legend("topright",
       legend = c("T Prediction Interval", "Mahalanobis", "KNN (k=1)"),
       fill = c("red", "orange", "blue"))

