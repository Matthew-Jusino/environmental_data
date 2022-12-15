rnorm(0, mean = 0, sd = 1)

x = seq(-3, 3, length.out = 1000)
y = dnorm(x)
plot(x, y, main = "Normal PDF", type = "1")
abline(h=0)

plot(x, y, main = "Normal PDF", type = "l")
plot(x, y)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)

par(mfrow = c(1, 1))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

dat_unif = runif(n = 27000, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max =4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1,1))
hist(dat_unif_1)
hist(dat_unif_2)

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

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

set.seed(22)
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot( y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
dat_random$resids <- dat_random$y - dat_random$y_predicted
dat_random$resids
sum(dat_random$resids)
abs(sum(dat_random$resids))
plot(dat_random$resids, col = "royalblue1")
hist(dat_random$resids)
