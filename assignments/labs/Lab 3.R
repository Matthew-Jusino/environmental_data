require(psych)
pairs.panels(iris)

names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1

my_vec = dat_all$CEWA
my_vec > 0
waxwings = my_vec > 0
waxwings

as.numeric(my_vec > 1)
cewa_present_absent = as.numeric(waxwings)
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)


# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent, pch = 20, col = "orange")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.tot")])

my_vec1 = dat_all$BUSH
my_vec1 > 0
bushtits = my_vec1 > 0
my_vec2 = dat_all$CORA
my_vec2 > 0
ravens = my_vec2 > 0
bush_p_a = as.numeric(bushtits)
cora_p_a = as.numeric(ravens)

plot(x = dat_all$ba.tot, y = bush_p_a, main = "Bushtit Presence/Absence in Response to Basal Area", xlab = "Basal Area", ylab = "Presence/Absence")
curve(logistic_midpoint_slope(x, midpoint = 90, slope = -0.8), add = TRUE)

plot(x = dat_all$ba.tot, y = cora_p_a, main = "Common Raven Presence/Absence in Response to Basal Area", xlab = "Basal Area", ylab = "Presence/Absence")
curve(logistic_midpoint_slope(x, midpoint = 95, slope = -0.85), add = TRUE)

dat_all$CORA
dat_all$BUSH


sum(dat_all$GRJA)
jay_vec = dat_all$GRJA > 0
sum(jay_vec)
