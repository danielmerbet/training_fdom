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

#correlation plot
data <- data[,2:ncol(data)]
numeric_data <- data[, sapply(data, is.numeric)]
#compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
#plot the correlation matrix
library(corrplot)
pdf(paste0(dir, "output/_sfig_correlation.pdf"), width = 7, height = 7)
corrplot(cor_matrix, method = "circle", type="upper")
dev.off()