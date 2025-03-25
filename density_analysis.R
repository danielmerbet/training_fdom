
set.seed(123)
case_study <- "sau"
dir <- paste0("~/Documents/intoDBP/training_fdom/",case_study, "/")
perc <- read.csv(paste0(dir, "output/save_perc.csv"))
i <- 437


#most important predictors: v, st255, sm100, sm255, doc_gwlf, cyday
pdf(paste0(dir, "output/density_analysis.pdf"))
par(mar=c(5,4,4,8))  # Adds space on the right
row <- which(perc$X=="v")
plot(density(as.numeric(perc[row,2:ncol(perc)])), xlim=c(0,40), ylim=c(0,0.5),
     main="Density Plot", xlab="Node Purity (%)", ylab="Density", bty="n")
abline(v = perc[row,i], lwd=1, lty=2)
row <- which(perc$X=="st255")
lines(density(as.numeric(perc[row,2:ncol(perc)])), col="salmon2")
abline(v = perc[row,i], lwd=1, lty=2, col="salmon2")
row <- which(perc$X=="sm100")
lines(density(as.numeric(perc[row,2:ncol(perc)])), col="forestgreen")
abline(v = perc[row,i], lwd=1, lty=2, col="forestgreen")
row <- which(perc$X=="sm255")
lines(density(as.numeric(perc[row,2:ncol(perc)])), col="steelblue")
abline(v = perc[row,i], lwd=1, lty=2, col="steelblue")
row <- which(perc$X=="doc_gwlf")
lines(density(as.numeric(perc[row,2:ncol(perc)])), col="magenta4")
abline(v = perc[row,i], lwd=1, lty=2, col="magenta4")
row <- which(perc$X=="cyday")
lines(density(as.numeric(perc[row,2:ncol(perc)])), col="darkgrey")
abline(v = perc[row,i], lwd=1, lty=2, col="darkgrey")
# Add legend
legend("topright", legend=c("Volume", "Soil temperature 255cm", 
                            "Soil moisture 100cm", "Soil moisture 255cm",
                            "DOC hydrologyc model", "Julian day"), 
       col=c("black", "salmon2", "forestgreen", "steelblue", "magenta4", "darkgrey")
       ,lty=1, lwd=2, xpd=TRUE, inset=c(-0.3,0), bty="n",y.intersp=0.7)
dev.off()

plot(as.numeric(perc[19,]), as.numeric(perc[18,]))
plot(as.numeric(perc[23,]), as.numeric(perc[15,]))

nse <- read.csv("~/Documents/intoDBP/training_fdom/sau/output/nse_movingtest.csv")

plot(density(as.numeric(unlist(nse)), na.rm=T))
