library(lubridate); library(dplyr)

#soil and meteo data from Open Meteo
case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
soil_meteo <- read.csv(paste0(dir, "download/meteo-soil_data.csv"))
soil_meteo$date <- as.Date(soil_meteo$date)
soil_meteo_temp <- aggregate(. ~ date, data=soil_meteo, FUN = function(x) c(mean = mean(x, na.rm = TRUE)))
soil_meteo_temp$precipitation <- NULL
rain_temp <- aggregate(precipitation ~ date, data=soil_meteo, FUN = function(x) c(sum = sum(x, na.rm = TRUE)))
soil_meteo <- merge(soil_meteo_temp, rain_temp, by="date")
names(soil_meteo) <- c("date","t", "rh", "sp", "cc", "pev",
                       "ws","st7", "st28", "st100","st255",
                       "sm7", "sm28", "sm100","sm255","tp")
soil_meteo_temp <- NULL
rain_temp <- NULL

write.csv(soil_meteo, paste0(dir, "data/meteo-soil_daily_data.csv"), 
          row.names = F, quote = F)

#streamflow data from ATL
#stream <- read.csv(paste0(dir, "data/river.csv"), header = F)
#stream <- data.frame(date=as.Date(stream$V1, "%m/%d/%Y"), q=stream$V2)

#Load lake data (GLM output)
glm_out <- read.csv(paste0(dir, "data/lake.csv"))
lake_data <- data.frame(date=as.Date(glm_out$time),
                        swt=glm_out$Surface.Temp,
                        v=glm_out$Volume,
                        lh=glm_out$Daily.Qe,
                        sh=glm_out$Daily.Qh,
                        #light=glm_out$Light,
                        strat=glm_out$Max.dT.dz)

#merge all possible drivers
#drivers <- merge(stream, lake_data, by="date")
#drivers <- merge(drivers, soil_meteo, by="date")
drivers <- merge(lake_data, soil_meteo, by="date")

#load GWLF: discharge and DOC
GWLF <- read.csv(paste0(dir, "data/GWLF.csv"), header = T)
GWLF <- GWLF[,2:ncol(GWLF)]
colnames(GWLF) <- c("date", "q_gwlf", "doc_gwlf")
GWLF$date <- as.Date(GWLF$date, "%m/%d/%Y")
drivers <- merge(drivers, GWLF, by="date")

#load DOC
#DOC <- read.csv("sau/data/DOC_SAU_C1.csv")
#DOC <- data.frame(date=DOC$Fecha, depth=DOC$Depth, doc=DOC$Valor)
#DOC <- subset(DOC, depth==5)
#DOC <- subset(DOC, depth >= 0 & depth <= 5)
#DOC$date <- as.Date(DOC$date)

#drivers <- merge(drivers, DOC, by="date")
write.csv(drivers, paste0(dir,"data/drivers.csv"), row.names = F, quote = F)
