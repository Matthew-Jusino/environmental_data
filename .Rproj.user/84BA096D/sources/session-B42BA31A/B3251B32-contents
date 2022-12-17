ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b) {
  return(a*exp(-b*x))
}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

require(here)
dispdata <- read.csv(here("data", "dispersal.csv"))
dispdata

par(mfrow = c(1,1))
curve(
  exp_fun(x, 1.9, 0.1), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()
curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = 3); box()
curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red"); box()
curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red", lty = 3); box()

png(filename = "salamander_figure1.png",
    width = 1500, height = 1600, res = 180)
dev.off()

curve(
  ricker_fun(x, 25, 0.2), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", ylim = c(0,100))
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", ylim = c(0,100), lty = 3)
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", ylim = c(0,100), lty = 3)
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", col = "red", ylim = c(0,100))
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", col = "red", ylim = c(0,100), lty = 3)
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 35, add = TRUE,
  ylab = "f(x)", xlab = "x", col = "red", ylim = c(0,100), lty = 3)

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

plot(
  dispdata$dist.class,
  dispdata$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")
curve(line_point_slope(x, -500, 0.6, -0.0003), add = TRUE)

plot(
  dispdata$dist.class,
  dispdata$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")
curve(
  exp_fun(x, 2, 0.004), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

plot(
  dispdata$dist.class,
  dispdata$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")
curve(
  ricker_fun(x, 0.02, 0.008), 
  from = 0, to = 1500, add = TRUE)

dispdata$y_pred_lin <- line_point_slope(dispdata$dist.class, -500, 0.6, -0.0003)
dispdata$resids_lin <- dispdata$disp.rate.ftb - dispdata$y_pred_lin
dispdata$resids_lin

dispdata$y_pred_exp <- exp_fun(dispdata$dist.class, 2, 0.004)
dispdata$resids_exp <- dispdata$disp.rate.ftb - dispdata$y_pred_exp
dispdata$resids_exp

dispdata$y_pred_rick <- ricker_fun(dispdata$dist.class, 0.02, 0.008)
dispdata$resids_rick <- dispdata$disp.rate.ftb - dispdata$y_pred_rick
dispdata$resids_rick

resids_dataframe <- data.frame(dispdata$resids_lin, dispdata$resids_exp, dispdata$resids_rick)
resids_dataframe

par(mfrow = c(3,1))
plot(resids_dataframe$dispdata.resids_lin, main = "Linear Residuals")
plot(resids_dataframe$dispdata.resids_exp, main = "Exponential Residuals", pch = 15)
plot(resids_dataframe$dispdata.resids_rick, main = "Ricker Residuals", pch = 5)
