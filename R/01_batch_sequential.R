##-----------------------------
## Sequential parametric tests live coding session
## CM: 04/10/2017
## EPA tipping points workshop
##-----------------------------

## load the cpm library
library(cpm)

## read in the data
wind <- read.csv("../data/wind_speed.csv")

year <- wind$year
y <- wind$speed

## plot the data
plot(year, y, type = "o", col = "purple", ylab = "Speed (knots)", pch = 19, bty = "l")

##---------------------------------------
## BATCH PROCESSING - SINGLE CHANGEPOINT 
##---------------------------------------

## Student t-test
resultsStudent <- detectChangePointBatch(y, cpmType = "Student", alpha = 0.05)
## check if change detected
## plot out the test statistic
plot(year, resultsStudent$Ds)
abline(h = resultsStudent$threshold, lty = 2)
points(year[resultsStudent$changePoint], resultsStudent$Ds[resultsStudent$changePoint], col = "red", cex = 2)

## Mann-Whitney
resultsMW <- detectChangePointBatch(y, cpmType = "Mann-Whitney", alpha = 0.05)
## check if change detected
## plot out the test statistic
plot(year, resultsMW$Ds)
abline(h = resultsMW$threshold, lty = 2)
points(year[resultsMW$changePoint], resultsMW$Ds[resultsMW$changePoint], col = "red", cex = 2)

## plot the data
plot(year, y, type = "o", col = "purple", ylab = "Speed (knots)", pch = 19, bty = "l", main = "Batch processing changepoints")
abline(v = year[resultsStudent$changePoint], lty = 2, col = "magenta" )
abline(v = year[resultsMW$changePoint], lty = 2, col = "forestgreen")
legend("topright", legend = c("Student t", "Mann-Whitney"), lty = 2, col = c("magenta", "forestgreen"), bty = "n")

##-----------------------------------------------------------
## SEQUENTIAL PROCESSING - POTENTIALLY MULTIPLE CHANGEPOINTS 
##-----------------------------------------------------------
## Student t
processStudent <- processStream(y, cpmType = "Student", startup = 10)
## 
## plot out the test statistic
plot(year, processStudent$Ds)
abline(h = processStudent$threshold, lty = 2)
points(year[processStudent$changePoint], processStudent$Ds[processStudent$changePoint], col = "red", cex = 2)

k <- c(0, processStudent$changePoints, n)
cols <- c("blue", "green2", "magenta")

pdf("../tex/figures/multiple_cps.pdf", height = 6, width = 8)
plot.base()
for(i in 1:3){
    idx <- (k[i] + 1):k[i + 1]
    points(time.plot[idx], y[idx], pch = 19, col = cols[i])
    curve(mean(y[idx]) + 0 * x, from = time.plot[k[i] + 1], to = time.plot[k[i+1]], add = TRUE, col = cols[i])
}
abline(v = time.plot[processStudent$changePoints], col = "red")
abline(v = time.plot[processStudent$detectionTimes], col = "red", lty = 2)
legend("topleft", legend = c("Estimated change point", "Detection time"), col = "red", lty = 1:2, bty = "n")
dev.off()
