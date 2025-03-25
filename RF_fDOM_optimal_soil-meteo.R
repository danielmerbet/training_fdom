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

#select best parameters
data <- data[,c("t", "rh", "sp", "cc", "ws","sr", "tp", "pev", "v",
         "st7","st28","st100","st255",
         "sm7", "sm28","sm100", "sm255", "cyday", "fdom", "date")]

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
i <- 437 #best fitting using NSE
train_perc <- 0.85 #percentage for training 
training_number <- round(dim(data)[1]*train_perc)
total_front <- dim(data)[1]-training_number
number_test <- dim(data)[1]-total_front
m <- (1+i):(1+i+total_front)
traindata <- data[-m,]
testdata <- data[m,]

#start training 
set.seed(123)
formula <- as.formula(paste(tvar, "~ . - date"))
RFfit <- randomForest(formula, data = traindata, ntree = 1000)
#plot(RFfit)

#check resulting stats with OOB data
set.seed(123)
predRF_OOB<- predict(RFfit) # without data, give the prediction with OOB samples
rsqOOB <- round((cor(predRF_OOB, traindata[tvar]))^2,2) ; rsqOOB
rmseOOB <- round(sqrt(mean((traindata[tvar][,1] - predRF_OOB)^2)), 2); rmseOOB
maeOOB <- mean(abs(traindata[tvar][,1] - predRF_OOB));maeOOB
importance_random <- importance(RFfit); importance_random
importance_perc <- importance_random/sum(importance_random)*100
importance_perc

#testing: check with data not used in training 
set.seed(123)
predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
nse_test <- round(nse(testdata[tvar][,1], predRF),2); nse_test
  importance_random <- importance(RFfit); importance_random
importance_perc <- importance_random/sum(importance_random)*100
importance_perc

png(paste0(dir, "output/fig4_opt_soil-meteo.png"),width = 800, height = 600, bg=NA)
#now the stats are NOT as good, but they are REAL (not affected by autocorrelation)
plot(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", type="l")
points(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", xaxt='n')
points(traindata$date, predRF_OOB, col="blue", pch = 19, cex=0.5)

#plot(testdata$date, testdata[tvar][,1], ylim=c(5,52), xlab="Dates", ylab="fDOM (QSU)")
points(testdata$date, predRF, col="red", pch = 19, cex=0.5)
#abline(v = as.numeric(as.Date(testdata$date[1])), col="darkgrey", lwd=2, lty=2)
title(main="Optimal training period with meteorological and soil data")
text(x = min(traindata$date), y = max(data[tvar][,1]-10),
     labels = paste0("R² = ", rsq_test, "\nNSE = ", nse_test, "\nRMSE = ", rmse_test), pos = 4, col = "black")
dev.off()

plot(testdata[tvar][,1],predRF, xlab="Obs", ylab="Sim", ylim=c(5,52),xlim=c(5,52))
abline(0,1, col="red")
