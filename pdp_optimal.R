library(lubridate); library(pdp)
library(gridExtra) # for arranging multiple plots

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
#data <- data[,c("v", "st255","sm100", "sm255","doc_gwlf", "cyday", "fdom", "date")]

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
#set.seed(123)
#predRF_OOB<- predict(RFfit) # without data, give the prediction with OOB samples
#rsqOOB <- round((cor(predRF_OOB, traindata[tvar]))^2,2) ; rsqOOB
#rmseOOB <- round(sqrt(mean((traindata[tvar][,1] - predRF_OOB)^2)), 2); rmseOOB
#maeOOB <- mean(abs(traindata[tvar][,1] - predRF_OOB));maeOOB
#importance_random <- importance(RFfit); importance_random
#importance_perc <- importance_random/sum(importance_random)*100
#importance_perc

#testing: check with data not used in training 
#set.seed(123)
#predRF<- predict(RFfit, testdata) # without data, give the prediction with OOB samples
#rsq_test <- round((cor(predRF, testdata[tvar]))^2,2) ; rsq_test
#rmse_test <- round(sqrt(mean((testdata[tvar][,1] - predRF)^2)), 2); rmse_test
#nse_test <- round(nse(testdata[tvar][,1], predRF),2); nse_test
#importance_random <- importance(RFfit); importance_random
#importance_perc <- importance_random/sum(importance_random)*100
#importance_perc

#Partial dependence plots
var <- "v"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)
var <- "st255"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)
var <- "sm100"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)
var <- "sm255"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)
var <- "doc_gwlf"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)
var <- "cyday"
partial(RFfit, pred.var = var, plot = T, plot.engine = "ggplot2", rug=T)

#ggplot nice
#Partial dependence plots
# Define the variables
vars <- c("v", "st255", "sm100", "sm255", "doc_gwlf", "cyday")

# Generate the plots
plots <- lapply(vars, function(var) {
  ggplot(partial(RFfit, pred.var = var), aes_string(x = var, y = "yhat")) +
    geom_point() +
    geom_smooth(span = 0.2) +
    theme_bw() +
    labs(x = var, y = "Average fDOM predicted")
})

# Save to PDF
pdf("partial_dependence_plots.pdf", width = 10, height = 6)
grid.arrange(grobs = plots, ncol = 3, nrow = 2)
dev.off()


