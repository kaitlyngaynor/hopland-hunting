library(overlap)

buck_records <- read.csv("Data/legal-buck-during-hunting.csv")
head(buck_records)

# Plot diel activity
bwA <- getBandWidth(buck_records$Time_Radians, kmax = 3)

xsc <- 24/(2 * pi)
xxRad <- seq(0, 2 * pi, length = 128)
xx <- xxRad * xsc
densA <- densityFit(buck_records$Time_Radians, xxRad, bwA)/xsc

plot(0, 0, type = "n", ylim = c(0, 0.03), xlim = c(6,18), 
     xlab = "Time", 
     ylab = "Activity", 
     xaxt = "n")

axis(1, at = c(6, 12, 18), labels = c("Sunrise", "Noon", "Sunset"))
polygon(c(c(2 * pi, 0), xx,24), c(0, 0, densA,0), border = NA, 
        col = "lightgrey")
lines(xx, densA, lty = 1, col = "black")
