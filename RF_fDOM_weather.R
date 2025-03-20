library(lubridate)

case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
#load drivers (meteorology, soil,  streamflow and all possible variables)
drivers <- read.csv(paste0(dir, "data/drivers.csv"))
drivers$date <- as.Date(drivers$date)

#load target variables
tvar <- "fdom" #"doc"#"fdom"
target <- read.csv(paste0(dir ,"data/",tvar,".csv"))
target$date <- as.Date(target$date)

#merge all and add julian day and dummy
data <- merge(drivers, target, by="date")
data$cyday <- cos(yday(data$date)*pi/180)
data$random <- runif(nrow(data))

#ML Analysis
###############################################################
#Random forest
library(randomForest)
save_rsq <- c()
save_rmse <- c()
#1. TRAINING WITH ALL DATA AND TESTING WITH SAMPLES OOB 
#in theory this is wrong because the trianing data is autocorrelated
#train RF
formula <- as.formula(paste(tvar, "~ . - date"))
RFfit <- randomForest(formula, data = data, ntree = 1000)
#plot(RFfit) #check that the error is reduced!

#testing: check with data not used in training but still autocorrelated (Out-of-Bag data)
predRF<- predict(RFfit) # without data, give the prediction with OOB samples
rsqOOB = round((cor(predRF, data[tvar]))^2,2) ; rsqOOB
rmseOOB <- round(sqrt(mean((data[tvar][,1] - predRF)^2)), 2); rmseOOB
importance_random <- importance(RFfit); importance_random
importance_random/sum(importance_random)*100

#stats are quite good, it seems promising
plot(data$date, data[tvar][,1], xlab="Dates", ylab="fDOM (QSU)")
points(data$date, predRF, col="darkgrey")

plot(data[tvar][,1],predRF, xlab="Obs", ylab="Sim") #ylim=c(5,52),xlim=c(5,52)
abline(0,1, col="red")

#2. TRAINING AND TESTING WITH NON AUTOCORRELATED SAMPLES
#OOB samples are independent,
#but, RF assumes observations are not autocorrelated
#For time series, this assumption might not hold
#Let's train and test with samples taken from different time ranges
train_perc <- 0.8 #percentage for training 
m <- 1:(dim(data)[1]*train_perc)
traindata <- data[m,]
testdata <- data[-m,]

#start training 
formula <- as.formula(paste(tvar, "~ . - date"))
RFfit <- randomForest(formula, data = traindata, ntree = 1000)
#plot(RFfit)

#check resulting stats with OOB data
predRF_OOB<- predict(RFfit) # without data, give the prediction with OOB samples
rsqOOB <- round((cor(predRF_OOB, traindata[tvar]))^2,2) ; rsqOOB
rmseOOB <- round(sqrt(mean((traindata[tvar][,1] - predRF_OOB)^2)), 2); rmseOOB
maeOOB <- mean(abs(traindata[tvar][,1] - predRF_OOB));maeOOB

#testing: check with data not used in training 
predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
save_rsq <- c(save_rsq, rsq_test)
save_rmse <- c(save_rmse, rmse_test)
maeOOB <- mean(abs(testdata[tvar][,1] - predRF));maeOOB
importance_random <- importance(RFfit); importance_random

#now the stats are NOT as good, but they are REAL (not affected by autocorrelation)
plot(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", type="l")
points(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", xaxt='n')
points(traindata$date, predRF_OOB, col="blue", pch = 19, cex=0.5)

#plot(testdata$date, testdata[tvar][,1], ylim=c(5,52), xlab="Dates", ylab="fDOM (QSU)")
points(testdata$date, predRF, col="red", pch = 19, cex=0.5)
abline(v = as.numeric(as.Date(testdata$date[1])), col="darkgrey", lwd=2, lty=2)

plot(testdata[tvar][,1],predRF, xlab="Obs", ylab="Sim", ylim=c(5,52),xlim=c(5,52))
abline(0,1, col="red")

# 3. Repeat, but selecting data only with high importance
# select variable greater importance than Julian day
data <- data[c("swt", "v", "light","t", "st7", "st28", 
               "sm100","sm255", "doc_gwlf","fdom", "date")]

train_perc <- 0.8 #percentage for training 
m <- 1:(dim(data)[1]*train_perc)
traindata <- data[m,]
testdata <- data[-m,]

#start training 
formula <- as.formula(paste(tvar, "~ . - date"))
RFfit <- randomForest(formula, data = traindata, ntree = 1000)
#plot(RFfit)

#check resulting stats with OOB data
predRF_OOB<- predict(RFfit) # without data, give the prediction with OOB samples
rsqOOB <- round((cor(predRF_OOB, traindata[tvar]))^2,2) ; rsqOOB
rmseOOB <- round(sqrt(mean((traindata[tvar][,1] - predRF_OOB)^2)), 2); rmseOOB
maeOOB <- mean(abs(traindata[tvar][,1] - predRF_OOB));maeOOB

#testing: check with data not used in training 
predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
save_rsq <- c(save_rsq, rsq_test)
save_rmse <- c(save_rmse, rmse_test)
maeOOB <- mean(abs(testdata[tvar][,1] - predRF));maeOOB
importance_random <- importance(RFfit); importance_random

#now the stats are NOT as good, but they are REAL (not affected by autocorrelation)
plot(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", type="l")
points(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", xaxt='n')
points(traindata$date, predRF_OOB, col="blue", pch = 19, cex=0.5)

#plot(testdata$date, testdata[tvar][,1], ylim=c(5,52), xlab="Dates", ylab="fDOM (QSU)")
points(testdata$date, predRF, col="red", pch = 19, cex=0.5)
abline(v = as.numeric(as.Date(testdata$date[1])), col="darkgrey", lwd=2, lty=2)

plot(testdata[tvar][,1],predRF, xlab="Obs", ylab="Sim", ylim=c(5,52),xlim=c(5,52))
abline(0,1, col="red")

# 4. Repeat, but selecting data only from soil and meteorology
data <- data[c("t", "st7", "st28", 
               "sm100","sm255", "fdom", "date")]

train_perc <- 0.8 #percentage for training 
m <- 1:(dim(data)[1]*train_perc)
traindata <- data[m,]
testdata <- data[-m,]

#start training 
formula <- as.formula(paste(tvar, "~ . - date"))
RFfit <- randomForest(formula, data = traindata, ntree = 1000)
#plot(RFfit)

#check resulting stats with OOB data
predRF_OOB<- predict(RFfit) # without data, give the prediction with OOB samples
rsqOOB <- round((cor(predRF_OOB, traindata[tvar]))^2,2) ; rsqOOB
rmseOOB <- round(sqrt(mean((traindata[tvar][,1] - predRF_OOB)^2)), 2); rmseOOB
maeOOB <- mean(abs(traindata[tvar][,1] - predRF_OOB));maeOOB

#testing: check with data not used in training 
predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
save_rsq <- c(save_rsq, rsq_test)
save_rmse <- c(save_rmse, rmse_test)
maeOOB <- mean(abs(testdata[tvar][,1] - predRF));maeOOB
importance_random <- importance(RFfit); importance_random

#now the stats are NOT as good, but they are REAL (not affected by autocorrelation)
plot(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", type="l")
points(data$date, data[tvar][,1], ylab="fDOM (QSU)", xlab = "", xaxt='n')
points(traindata$date, predRF_OOB, col="blue", pch = 19, cex=0.5)

#plot(testdata$date, testdata[tvar][,1], ylim=c(5,52), xlab="Dates", ylab="fDOM (QSU)")
points(testdata$date, predRF, col="red", pch = 19, cex=0.5)
abline(v = as.numeric(as.Date(testdata$date[1])), col="darkgrey", lwd=2, lty=2)

plot(testdata[tvar][,1],predRF, xlab="Obs", ylab="Sim", ylim=c(5,52),xlim=c(5,52))
abline(0,1, col="red")

#final rsq and rmse:
save_rsq
save_rmse
