require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))

hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "Body Mass (g)")

boxplot(dat_ade$body_mass_g ~ dat_ade$sex, main = "Adelie Body Mass by Sex", xlab = "Sex", ylab = "Body Mass (g)", col = "magenta2")


Adelie_f <- droplevels(subset(penguins, sex == "female"))
t.test(Adelie_f$body_mass_g)

Adelie_m <- droplevels(subset(penguins, sex == "male"))
t.test(Adelie_m$body_mass_g, alternative = "greater", mu = 4000)

t.test(x = Adelie_f$body_mass_g, y = Adelie_m$body_mass_g, alternative = "two.sided")
t.test(Adelie_m$body_mass_g, Adelie_f$body_mass_g, alternative = "less")
