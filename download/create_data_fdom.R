#Target variable
tvar <- "fdom"
min_depth <- 0
max_depth <- 5
target <- read.csv("~/Documents/intoDBP/ATL_data/profiles_torre/fDOM_2017-2020", header = F)
target <- target[,c(2:4,6)]
names(target) <- c("date","time","depth","fdom")

# Filter data for depths between 0 and 5
filtered_data <- target %>%
  filter(depth >= min_depth & depth <= max_depth)

# Create a new column for date and hour
filtered_data <- filtered_data %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         day = as.Date(datetime),
         hour = format(datetime, "%H"))

# Group by day and hour, and calculate mean fdom
target_daily <- filtered_data %>%
  #group_by(day, hour) %>% #to get hourly
  group_by(day) %>%
  summarise(mean_fdom = mean(fdom, na.rm = TRUE))

names(target_daily) <- c("date", tvar)
target_daily$date <- as.Date(target_daily$date, "%Y-%m-%d")

write.csv(target_daily, 
          paste0("~/Documents/intoDBP/HydroC_model/data/target_fDOM_",
                 min_depth,"-",max_depth,".csv"), 
          row.names = F, quote = F)

# Group by hour, and calculate mean fdom
target_hourly <- filtered_data %>%
  #group_by(day, hour) %>% #to get hourly
  group_by(day, hour) %>%
  summarise(fdom = mean(fdom, na.rm = TRUE))

target_hourly <- target_hourly %>%
  mutate(hour = paste0(hour, ":00:00"))

# Combine day and hour into a single datetime column
target_hourly <- target_hourly %>%
  ungroup() %>% # Remove grouping by 'day'
  mutate(date = ymd_hms(paste(day, hour))) %>%
  select(date, fdom) # Reorder or select relevant columns

write.csv(target_hourly, 
          paste0("~/Documents/intoDBP/HydroC_model/data/hourly_target_fDOM_",
                 min_depth,"-",max_depth,".csv"), 
          row.names = F, quote = F)
