#Set-up
require(here)
dat_veg <- read.csv(here("data", "vegdata.csv"))
dat_birds <- read.csv(here("data", "bird.sub.csv"))
dat_habs <- read.csv(here("data", "hab.sub.csv"))
require(palmerpenguins)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

require(simpleboot)
require(boot)

#Section 1 (Penguins), Questions 1-8
dat_adelie = droplevels(subset(penguins, species == "Adelie"))
dat_chin = droplevels(subset(penguins, species == "Chinstrap"))

pen_boot = two.boot(na.omit(dat_adelie$flipper_length_mm), na.omit(dat_chin$flipper_length_mm), FUN = mean, R = 10000, student = FALSE, weights = NULL)

str(pen_boot)
hist(pen_boot$t, main = "Histogram of 10,000 Bootstrap Differences in Mean Flipper Length",
        xlab = "Difference in mean flipper length (mm)
     Adelie and Chinstrap Penguins")
sd(pen_boot$t)

quantile(pen_boot$t, probs = c(0.025, 0.975))
summary(pen_boot$t)

pen_ecdf = ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)
pen_ecdf(-8)

#Section 2 (Trees), Questions 9-11
head(dat_tree)
boxplot(pine ~ treatment, dat = dat_veg)
dat_tree = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
table(dat_tree$treatment)

wilcox.test(dat_tree$pine)

pine_cont = droplevels(subset(dat_veg, treatment == "control"))
pine_clip = droplevels(subset(dat_veg, treatment == "clipped"))
wilcox.test(pine_cont$pine, pine_clip$pine)

tree_boot = two.boot(na.omit(pine_clip$pine), na.omit(pine_cont$pine), FUN = mean, R = 10000, student = FALSE, weights = NULL)
str(tree_boot)
boot.ci(tree_boot)
hist(tree_boot$t)
quantile(tree_boot$t, probs = c(0.025, 0.975))
mean(pine_clip$pine) - mean(pine_cont$pine)

#Section 3 (Birds & Habs), Questions 12-20
dall = merge(
  dat_birds,
  dat_habs,
  by = c("basin", "sub"))
head(dall[, c("b.sidi", "s.sidi")])

b_sidi_mean = mean(dall$b.sidi, na.rm = TRUE)
b_sidi_sd = sd(dall$b.sidi, na.rm = TRUE)
dall$b.sidi.standardized = (dall$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dall$b.sidi.standardized)

s_sidi_mean = mean(dall$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dall$s.sidi, na.rm = TRUE)
dall$s.sidi.standardized = (dall$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dall$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dall,
  main = "Simpson's Diversity Indices",
  xlab = "Vegetation Cover Diversity",
  ylab = "Bird Diversity", pch = 70
)
abline(fit_1)

fit_1 = lm(b.sidi ~ s.sidi, data = dall)
coef(fit_1)
slope_observed = coef(fit_1)[2]

dat_1 =
  subset(
    dall,
    select = c(b.sidi, s.sidi))

#Section 3.2 (Monte Carlo)
set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]
print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

m = 10000
result_mc = numeric(m)
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  dat_resampled_i = data.frame(b.sidi = dat_1$b.sidi[index_1], s.sidi = dat_1$s.sidi[index_2])
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result_mc[i] = coef(fit_resampled_i)[2]
}

hist(
  result_mc,
  main = "Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed,lty = 2, col = "red", lwd = 2)

crit_value_MC = quantile(result_mc, c(.05))
print(slope_observed)
print(crit_value_MC)
abline(v = crit_value_MC,lty = 2, col = "green", lwd = 2)

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)
dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
coef(fit_bs1)
coef(fit_1)

m = 10000
result_bs = numeric(m)
for(i in 1:m)
{
  bs_index_1 = sample(nrow(dat_1), replace = TRUE)
  dat_boot = dat_1[bs_index_1, ]
  fit_bs1_i = lm(b.sidi ~ s.sidi, data = dat_boot)
  result_bs[i] = coef(fit_bs1_i)[2]
}

hist(
  result_bs,
  main = "Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)

plot(
  density(result_mc),
  main = "Null Distribution Density Plot",
  xlab = "Slope Coefficient")
plot(
  density(result_bs),
  main = "Alternative Disrtibution Density Plot",
  xlab = "Slope Coefficient")

plot(
  density(result_mc),
  main = "Null & Alt Distribution Density Plots",
  xlab = "Slope Coefficient",
  xlim = c(-0.06, 0.04),
  ylim = c(0, 70))
lines(density(result_bs), col = 2)
legend(
  'topright',
  legend=c('Null', 'Alt'),
  lty=c(1,1),col=c(1,2))


