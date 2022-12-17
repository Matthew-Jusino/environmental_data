q1_sd = 2.4
q1_mean = 10.4

norm_17 = rnorm(n = 17, mean = q1_mean, sd = q1_sd)
norm_30 = rnorm(n = 30, mean = q1_mean, sd = q1_sd)
norm_300 = rnorm(n = 300, mean = q1_mean, sd = q1_sd)
norm_3000 = rnorm(n = 3000, mean = q1_mean, sd = q1_sd)

par(mfrow = c(2, 2))
hist(norm_17, main = "17 Random Points Histogram")
hist(norm_30, main = "30 Random Points Histogram")
hist(norm_300, main = "300 Random Points Histogram")
hist(norm_3000, main = "3000 Random Points Histogram")
dev.off()
png(filename = "lab_04_hist_01.png",
    width = 1500, height = 1600, res = 180)

# Generate a vector of x-values
x = seq(0.8, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Normal PDF: mean = 10.4 sd = 2.4", type = "l", xlim = c(0.4, 20.4))
abline(h = 0)
dev.off()
svg(filename = "norm_1.svg",
    width = 10, height = 12)
set.seed(99)

n_pts = 20
x_min = 1
x_max = 10

x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)

dat_ran = data.frame(x = x_random, y = y_random)

par(mfrow = c(2,2))
plot(y ~ x, data = dat_ran, pch = 20, col = "goldenrod1")
plot(y ~ x, data = dat_ran, pch = 73, col = "plum3")
plot(y ~ x, data = dat_ran, pch = 3, col = "powderblue")
plot(y ~ x, data = dat_ran, pch = 99, col = "chartreuse3")

png(filename = "happy_fun_times_plotting.png",
    width = 1500, height = 1600, res = 180)
dev.off()

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
set.seed(99)



par(mfrow =c(1,1))
n_pts = 30
slope = 0.7
intcp = 0.2

guess_x = 9
guess_y = -1
guess_slope = -0.15

x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)

plot(y ~ x, data = dat_ran, pch = 70, col = "chartreuse3")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

png(filename = "line_of_best_f.png",
    width = 1500, height = 1600, res = 180)
dev.off()

line_point_slope(dat_ran$x, guess_x, guess_y, guess_slope)

dat_ran$y_predicted <- line_point_slope(dat_ran$x, guess_x, guess_y, guess_slope)
dat_ran$resids <- dat_ran$y - dat_ran$y_predicted
dat_ran$resids
sum(dat_random$resids)
abs(sum(dat_ran$resids))
plot(x = dat_ran$y_predicted, y = dat_ran$resids, pch = 70, col = "chartreuse3",
     main = "Scatterplot of Predicted Y Values and Residuals", xlab = "Predicted Y Values", ylab = "Residuals")
hist(dat_ran$resids, main = "Histogram of Residuals", col = "chartreuse3")

png(filename = "q12_scatterplot.png",
    width = 1500, height = 1600, res = 180)
plot(x = dat_ran$y_predicted, y = dat_ran$resids, pch = 70, col = "chartreuse3",
     main = "Scatterplot of Predicted Y Values and Residuals", xlab = "Predicted Y Values", ylab = "Residuals")
dev.off()
