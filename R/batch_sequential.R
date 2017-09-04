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
plot(year, y, type = "l", col = "purple", ylab = "Speed (knots)")

##---------------------------------------
## BATCH PROCESSING - SINGLE CHANGEPOINT 
##---------------------------------------

## Student t-test
resultsStudent <- detectChangePointBatch(y, cpmType = "Student", alpha = 0.05)
resultsMW <- detectChangePointBatch(y, cpmType = "Mann-Whitney", alpha = 0.05)

## can we run this ourselves?
D <- numeric(n)
for(i in 2:(n-2)){
    test <- t.test(y[1:i], y[(i+1):n], var.equal = TRUE)
    D[i] <- as.numeric(test$statistic)
}

## example for visualisation
k <- 10
idx1 <- 1:k
idx2 <- (k + 1):n

D10 <- as.numeric(t.test(y[idx1], y[idx2], var.equal = TRUE)$statistic)
D10 <- round(D10, 2)

pdf("../tex/figures/batch_example.pdf", height = 6, width = 8)
plot.base()
points(time.plot[idx1], y[idx1], pch = 19, col = "blue")
points(time.plot[idx2], y[idx2], pch = 19, col = "green2")
curve(mean(y[idx1]) + 0 * x, from = time.plot[1], to = time.plot[k], add = TRUE, col = "blue")
curve(mean(y[idx2]) + 0 * x, from = time.plot[k+1], to = time.plot[n], add = TRUE, col = "green2")
legend("topright", legend = c(paste("D =", D10)), cex = 1.2, bty = "n")
dev.off()


pdf("../tex/figures/D_statistic_1.pdf", height = 6, width = 8)
plot(time.plot, resultsStudent$Ds, bty = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 19, col = "purple")
axis(side = 1, cex.axis = 1.2)
axis(side = 2, cex.axis = 1.2)
mtext(side = 1, text = "Year", line = 2.5, cex = 1.2)
mtext(side = 2, text = expression(D[t]), line = 2.5, cex = 1.2)
abline(h = resultsStudent$threshold, lty = 2)
points(time.plot[resultsStudent$changePoint], resultsStudent$Ds[resultsStudent$changePoint], pch = 1, col = "red", cex = 2.5)
dev.off()

## best one
k <- resultsStudent$changePoint
idx1 <- 1:k
idx2 <- (k + 1):n

D10 <- resultsStudent$Ds[k]
D10 <- round(D10, 2)

pdf("../tex/figures/batch_example_best.pdf", height = 6, width = 8)
plot.base()
points(time.plot[idx1], y[idx1], pch = 19, col = "blue")
points(time.plot[idx2], y[idx2], pch = 19, col = "green2")
curve(mean(y[idx1]) + 0 * x, from = time.plot[1], to = time.plot[k], add = TRUE, col = "blue")
curve(mean(y[idx2]) + 0 * x, from = time.plot[k+1], to = time.plot[n], add = TRUE, col = "green2")
legend("topright", legend = c(paste("D =", D10)), cex = 1.2, bty = "n")
dev.off()
