require(here)
dat_birbs <- read.csv(here("data", "bird.sta.csv"))
dat_habs <- read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_birbs, dat_habs)

summary(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 0:7-0.5)


par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

par(mfrow = c(1, 1))
hist(dat_all$WIWR, breaks = 0:(max(dat_all$WIWR)+1)-0.5, main = " Histogram of Winter Wren Counts", xlab = "Bird Counts", col = "lightblue2")
sum(log(dpois(x = dat_all$WIWR, lambda = 1.456)))

sum(log(dbinom(x = dat_all$WIWR, size = 6, prob = 0.24)))
summary(dat_all$WIWR)

set.seed(1)
vec_rnorm = rnorm(n=10, mean=0, sd=1)
sum(log(dnorm(vec_rnorm, 0.13, 0.74)))
