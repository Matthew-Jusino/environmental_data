require(here)
dat_birds <- read.csv(here("data", "bird.sub.csv"))
dat_habs <- read.csv(here("data", "hab.sub.csv"))
birdhab = merge(
  dat_birds,
  dat_habs,
  by = c("basin", "sub"))
dim(birdhab)

plot(birdhab$ls, birdhab$BRCR,
     xlab = "late-successional forest extent",
     ylab = 'Brown Creeper abundance',
     pch = 20)
abline(fit_1)

fit_1 = lm(BRCR ~ ls, data = birdhab)


linear = function(x, y_int, slope)
{
  return(y_int + slope*x)
}
linear(x=1,y_int=1,slope=1)
linear(x=3:5,y_int=1,slope=1)
linear(x=3:5,y_int=-1,slope=1)
linear(x=3:5,y_int=-1,slope=0.01)

linear_simulator = function(x, y_int, slope, st_dev)
{
 return(rnorm(n, mean = (y_int + slope*x), sd = st_dev ))
}

n=30
par(mfrow=c(2,2),mar=c(1,1,1,1))
for (i in 1:4)
{
  x = runif(n=n)
  plot(
    x,
    linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

sd_obs = fit_1_summary$sigma
int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2]


plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))


y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
sum_1 = summary(fit_sim)
sum_1$coefficients


n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

#Simulating Effect Sizes Section
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size =
  data.frame(
    effect_size = effect_sizes_1,
    power = effect_size_powers)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab ='Effect Size', ylab = 'Power')
abline(v=slope_obs, lty =2, col = 'red')

#Simulating Sample Sizes Section
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)