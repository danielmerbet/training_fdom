#fdom data correction: temperature effect
#table from manufacturer (YSI)
temp_effect <- c(30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8)
qsu_effect <- c(289.2, 291.9, 294.6, 297.3, 300, 302.7, 305.4, 308.1, 310.8, 313.8, 316.5, 319.2)

library(lubridate)

case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
#load drivers (meteorology, soil,  streamflow and all possible variables)
drivers <- read.csv(paste0(dir, "data/drivers.csv"))
drivers$date <- as.Date(drivers$date)

#surface water temperature
min_depth <- 0
max_depth <- 5
swt <- read.table(paste0(dir, "data/SAU_TEMP_PROFILER.csv"), header = F)
swt <- swt[,c(3:5,7)]
names(swt) <- c("date","time","depth","swt")
# Filter data for depths between 0 and 5
filtered_data <- swt %>%
  filter(depth >= min_depth & depth <= max_depth)

# Create a new column for date and hour
filtered_data <- filtered_data %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         day = as.Date(datetime),
         hour = format(datetime, "%H"))

# Group by day and hour, and calculate mean swt
target_daily <- filtered_data %>%
  #group_by(day, hour) %>% #to get hourly
  group_by(day) %>%
  summarise(swt = mean(swt, na.rm = TRUE))

swt <- target_daily
names(swt) <- c("date", "swt")

#load target variables
tvar <- "fdom" #"doc"#"fdom"
target <- read.csv(paste0(dir ,"data/",tvar,"_uncorrected.csv"))
target$date <- as.Date(target$date)

#merge all
data <- merge(swt, target, by="date")

#set regresion manufacturer to get slope
regression <- lm(qsu_effect ~ temp_effect)
slope_effect <- regression$coefficients[2]

plot(temp_effect, qsu_effect, 
     pch = 16, col = "blue", 
     xlab = "Temperature Effect", 
     ylab = "QSU Effect")
abline(regression, col = "red", lwd = 2)

#set regresion own data by using maufacturer slope
#intercept must be found
plot(data$swt, data$fdom, 
     pch = 16, col = "blue", 
     xlab = "SWT", 
     ylab = "fDOM", ylim=c(0, 80))
#correct fdom: 22 degrees correspond to 300 QSU
data$fdom_cor <- (-1)*(data$swt-22)*slope_effect + data$fdom
points(data$swt, data$fdom_cor, col="red")
     
plot(data$date, data$fdom, ylim=c(0,80))
points(data$date, data$fdom_cor, col="red")

data <- data[!data$fdom_cor<0,]

data <- data[c(1,4)]
names(data) <- c("date", "fdom")
write.csv(data, file="sau/data/fdom.csv", quote=F, row.names = F)
