library(lubridate)

case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
number_test <- 1000
#load drivers (meteorology, soil,  streamflow and all possible variables)
drivers <- read.csv(paste0(dir, "data/drivers.csv"))
drivers$date <- as.Date(drivers$date)
drivers$light <- NULL
drivers$q <- NULL

#load target variables
tvar <- "fdom"
target <- read.csv(paste0(dir ,"data/",tvar,".csv"))
target$date <- as.Date(target$date)

#merge all and add julian day and dummy
data <- merge(drivers, target, by="date")
#data$yday <- yday(data$date)
data$cyday <- cos(yday(data$date)/365)
#data$syday <- sin(yday(data$date)/365)
data$random <- runif(nrow(data))

#ML Analysis
###############################################################
#Random forest
library(randomForest)
save_drivers <- c();save_importance <- c(); save_perc <- c()
for (i in 1:number_test){
  #1. TRAINING WITH ALL DATA AND TESTING WITH SAMPLES OOB 
  #in theory this is wrong because the trianing data is autocorrelated
  #train RF
  formula <- as.formula(paste(tvar, "~ . - date"))
  RFfit <- randomForest(formula, data = data, ntree = 1000)
  #plot(RFfit) #check that the error is reduced!
  
  #testing: check with data not used in training but still autocorrelated (Out-of-Bag data)
  predRF<- predict(RFfit) # without data, give the prediction with OOB samples
  rsqOOB <- round((cor(predRF, data[tvar]))^2,2) ; rsqOOB
  rmseOOB <- round(sqrt(mean((data[tvar][,1] - predRF)^2)), 2); rmseOOB
  importance_random <- importance(RFfit); importance_random
  importance_perc <- importance_random/sum(importance_random)*100
  
  save_drivers <- cbind(save_drivers, rownames(importance_perc))
  save_importance <- cbind(save_importance, importance_random)
  save_perc <- cbind(save_perc, importance_perc)

}
#greater than 5% which correspond approx. to greater than yday
save_perc_limit <- save_perc>5
save_perc_filter <- save_perc_limit[apply(save_perc_limit, 1, function(row) all(row == TRUE)), ]

filtered_data <- save_perc[rownames(save_perc_filter), ]

# Compute the row-wise mean
most_important <- rowMeans(filtered_data)
most_important
write.csv(most_important, file=paste0(dir, "drivers_RF.csv"))
