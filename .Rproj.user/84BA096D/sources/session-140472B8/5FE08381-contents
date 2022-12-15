sse_mean = function(x, n, na.rm = TRUE) {
  return(sd(x, na.rm = na.rm)/sqrt(n))
}

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm, 344)

boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")

dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  boxplot(
    flipper_length_mm ~ species, data = dat_pen,
    ylab = "Flipper length (mm)")
}

# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 1))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}

par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
print(c(observed = diff_observed, simulated = diff_simulated))

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

two_group_resample_diff = function(x, n_1, n_2){
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_1, replace = TRUE)
  diff_simulated = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  print(diff_simulated)
}

set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)

n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sum(abs(mean_differences) >= diff_observed)
t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)
t_test$estimate

n = 10000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
max(abs(mean_differences))

sse_mean(penguins$bill_length_mm, 344)

boxplot(
  bill_length_mm ~ species, data = penguins,
  ylab = "Bill length (mm)")


dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  boxplot(
    bill_length_mm ~ species, data = dat_pen,
    ylab = "Bill length (mm)")
}

agg_means = aggregate(
  bill_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

set.seed(123)

bill_shuffled = sample(
  penguins$bill_length_mm, replace = TRUE)

t.test(dat_pen$bill_length_mm ~ dat_pen$species)
set.seed(1)
bill_shuffled = sample(dat_pen$bill_length_mm)
boxplot(bill_shuffled ~ dat_pen$species)

t_test_1 = t.test(bill_shuffled ~ dat_pen$species)
t_test_1
t_test = t.test(dat_pen$bill_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
t_test
t_test_1
diff_crit = diff_observed
diff_crit

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$bill_length_mm, 68, 152)
  )
}
hist(mean_differences, main = "Histogram of the Mean Differences in 1000 Repetitions", xlab = "Mean Differences")
max(abs(mean_differences))