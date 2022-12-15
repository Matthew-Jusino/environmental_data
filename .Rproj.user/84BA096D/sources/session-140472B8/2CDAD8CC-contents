dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
apply(dat, MARGIN = 2, FUN = mean)

require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)

hist(moths$anst, breaks = 0:16-0.5)

#Calculating the Parametric CI
alpha = 0.05
n = sum(!is.na(moths$anst))
sse = sd(moths$anst, na.rm = TRUE) / sqrt(n)
t_crit = abs(qt(alpha / 2, df = n - 1))
ci_radius = sse * t_crit
anst_ci = c(
  lower = mean(moths$anst) - ci_radius,
  upper = mean(moths$anst) + ci_radius)
print(round(anst_ci, 4))

#Calculating the Boostrap CI
m = 10000
result = numeric(m)
head(result)
for(i in 1:m)
{
  result[i] = mean(sample(dat_gentoo$bill_length_mm, replace=TRUE))
}
mean(result)
quantile(result,c(0.025,0.975))

install.packages("boot")
require(boot)
#Boot formula is boot(data, statistic, R)

boot_mean = function(x,i)
{
  return(mean(x[i], na.rm = TRUE))
}

gentboot = 
  boot(
    data = dat_gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 342)
print(gentboot)
str(gentboot)
mean(dat_gentoo$bill_length_mm)
gentboot$t0
mean(gentboot$t) - gentboot$t0
sd(gentboot$t)
quantile(
  gentboot$t,
  C)

#Rarefaction Curve
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat)
m = 100
moth_result = matrix(
  nrow = m,
  ncol = n)
for(i in 1:m)
{
  for(j in 1:n)
  {
    rows_j = sample(n, size = j, replace = TRUE)
    t1 = moth_dat[rows_j, ]
    t2 = apply(t1, 2, sum)
    moth_result[i, j] = sum(t2 > 0)
  }
}
head(moth_result)


rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

rarefact = rarefaction_sampler(moths[, -1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of Sampling Plots',
  ylab='Species Richness',
  main = "Matt's Mothy Rarefaction Curve",
  lty = c(1,4,8), col = c("forestgreen","orangered3","slateblue3"))

legend(
  'bottomright',
  legend=c('Mean Species Richness', 'Lower Confidence Interval (2.5%)', 'Upper Confidence Interval (97.5%)'),
  lty=c(1,4,8),col=c("forestgreen","orangered3","slateblue3"),inset=c(.1,.1))

polygon(x = c(1:24, 24:1), y = c(rare[,2], rev(rare[,3])) , col = adjustcolor("slategray3", 0.25), border = NA)

summary(penguins)
dat_pen = droplevels(subset(penguins, species != "Adelie"))
dat_gentoo = droplevels(subset(penguins, species = "Gentoo"))
#plot(dat_pen$bill_length_mm ~ dat_pen$species)

alpha = 0.05
dat_gentoo$bill_length_mm = !is.na(dat_gentoo$bill_length_mm)
n = sum(dat_gentoo$bill_length_mm, na.rm = TRUE)
sse = sd(dat_gentoo$bill_length_mm, na.rm = TRUE) / sqrt(n)
t_crit = abs(qt(alpha / 2, df = n - 1))
ci_radius = sse * t_crit
anst_ci = c(
  lower = mean(dat_gentoo$bill_length_mm, na.rm = TRUE) - ci_radius,
  upper = mean(dat_gentoo$bill_length_mm, na.rm = TRUE) + ci_radius)
print(round(anst_ci, 4))
