head(catrate)
summary(catrate)

hist(catrate$cat.rate, main = "Histogram of Catastrophe Rates", xlab = "Catastrophe Rate")

shapiro.test(catrate$cat.rate)

summary(catrate$cat.rate)
t.test(catrate$cat.rate, mu = 2/7)
t.test(catrate$cat.rate, alternative = "greater", mu = 0)

wilcox.test(catrate$cat.rate, mu = 2/7)

require(palmerpenguins)
pen_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(pen_dat)
adelie = subset(pen_dat, species == "Adelie")
chinstrap = subset(pen_dat, species == "Chinstrap")
shapiro.test(adelie$flipper_length_mm)
shapiro.test(chinstrap$flipper_length_mm)

par(mfrow = c(2,1))
hist(adelie$flipper_length_mm, main = "Histogram of Adelie Flipper Lengths", xlab = "Flipper Length (mm)")
hist(chinstrap$flipper_length_mm, main = "Histogram of Chinstrap Flipper Lengths", xlab = "Flipper Length (mm)")

t.test(adelie$flipper_length_mm, chinstrap$flipper_length_mm)
