library(lubridate)

set.seed(123)
case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
#load drivers (meteorology, soil,  streamflow and all possible variables)
drivers <- read.csv(paste0(dir, "data/drivers.csv"))
drivers$date <- as.Date(drivers$date)

#load target variables
tvar <- "fdom"
target <- read.csv(paste0(dir ,"data/",tvar,".csv"))
target$date <- as.Date(target$date)

#merge all and add julian day and dummy
data <- merge(drivers, target, by="date")
data$cyday <- cos(yday(data$date)*pi/180)
data$random <- runif(nrow(data))

nse <- function(sim, obs) {
  numerator <- sum((obs - sim)^2)
  denominator <- sum((obs - mean(obs))^2)
  nse <- 1 - (numerator / denominator)
  return(nse)
}

#ML Analysis
###############################################################
#Random forest
library(randomForest)
save_drivers <- c();save_importance <- c(); save_perc <- c()
save_rsq <- c(); save_rmse <- c(); save_nse <- c()
train_perc <- 0.85 #percentage for training 
training_number <- round(dim(data)[1]*train_perc)
total_front <- dim(data)[1]-training_number
number_test <- dim(data)[1]-total_front
for (i in 0:(number_test-1)){
  
  #if (i < total_front){
  m <- (1+i):(1+i+total_front)
  #}else{
  #  break 
  #}

  traindata <- data[-m,]
  testdata <- data[m,]
  
  #start training 
  set.seed(123)
  formula <- as.formula(paste(tvar, "~ . - date"))
  RFfit <- randomForest(formula, data = traindata, ntree = 1000)
  
  #testing: check with data not used in training 
  set.seed(123)
  predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
  rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
  rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
  nse_test <- round(nse(testdata[tvar][,1], predRF),2)
  save_rsq <- c(save_rsq, rsq_test)
  save_rmse <- c(save_rmse, rmse_test)
  save_nse <- c(save_nse, nse_test)
  importance_random <- importance(RFfit); importance_random
  importance_perc <- importance_random/sum(importance_random)*100
  
  save_drivers <- cbind(save_drivers, rownames(importance_perc))
  save_importance <- cbind(save_importance, importance_random)
  save_perc <- cbind(save_perc, importance_perc)

}

write.csv(save_rsq, file=paste0(dir, "output/rsq_movingtest.csv"), quote = F, row.names = F)
write.csv(save_rmse, file=paste0(dir, "output/rmse_movingtest.csv"), quote = F, row.names = F)
write.csv(save_nse, file=paste0(dir, "output/nse_movingtest.csv"), quote = F, row.names = F)
write.csv(save_importance, file=paste0(dir, "output/save_importance.csv"), quote = F)
write.csv(save_perc, file=paste0(dir, "output/save_perc.csv"), quote = F)

which(save_nse==max(save_nse, na.rm=T))
#[1] 437
which(save_rmse==min(save_rmse, na.rm=T))
#[1] 51 52 58
which(save_rsq==max(save_rsq, na.rm=T))
#[1] 433

#greater than 5% which correspond approx. to greater than yday

save_perc_limit <- save_perc>5
save_perc_filter <- save_perc_limit[apply(save_perc_limit, 1, function(row) all(row == TRUE)), ]

filtered_data <- save_perc[rownames(save_perc_filter), ]

# Compute the row-wise mean
most_important <- rowMeans(filtered_data)
most_important
write.csv(most_important, file=paste0(dir, "drivers_RF.csv"))
