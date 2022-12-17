require(here)

dat_birds <- read.csv(here("data", "bird.sta.csv"))
dat_habs<- read.csv(here("data", "hab.sta.csv"))
dat_all <- merge(dat_birds, dat_habs)

hist(dat_all$WIWA, xlab = "Number of Birds Counted", main = "Wilson's Warbler Abundance", breaks = 0:7 - 0.5, col = "gold2")

hist(dat_all$WIWA)

apply(dat_birds[,-(1:2)], 2, sum)

max(dat_birds$WIWA)
