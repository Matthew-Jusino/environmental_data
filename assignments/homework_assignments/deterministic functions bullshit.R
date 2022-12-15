require(here)

dat.habitat <- read.csv(here("data","hab.sta.csv"))
# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

png(filename = "3 terrain scatterplots.png",
    width = 1500, height = 1600, res = 180)
par(mfrow = c(3, 1))
plot(y = dat.habitat$ba.tot, x = dat.habitat$slope, pch = 20, cex = 0.7, xlab = "Slope")
curve(line_point_slope(x, x1 = 3.5, y1 = 75, slope = -0.4), add = TRUE, col = "dodgerblue4")
plot(y = dat.habitat$ba.tot, x = dat.habitat$elev, pch = 20, cex = 0.7, xlab = "Elevation")
curve(line_point_slope(x, x1 = 3.5, y1 = 105, slope = -0.1), add = TRUE, col = "darkolivegreen")
plot(y = dat.habitat$ba.tot, x = dat.habitat$aspect, pch = 20, cex = 0.7, xlab = "Aspect")
curve(line_point_slope(x, x1 = 3.5, y1 = 70, slope = 0.03), add = TRUE, col = "magenta4")
dev.off()

png(filename = "3 terrain histograms.png",
    width = 1500, height = 1600, res = 180)
par(mfrow = c(1, 3))
hist(dat.habitat$slope, pch = 20)
hist(x = dat.habitat$elev, pch = 20)
hist(dat.habitat$aspect, pch = 20)
dev.off()

