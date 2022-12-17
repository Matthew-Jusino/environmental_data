require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#pond: the ID of the pond
#success: The number of years in which successful reproduction occurred at the pond
#years: The total number of years that the pond was observed.
#cat.rate: the ratio of successes to total observation years.

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate
)

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative = "less"
)
 
t.test(catrate$cat.rate, mu = 2/7)
 
 veg = read.csv(here("data", "vegdata.csv"))
head(veg) 
boxplot(pine ~ treatment, data = veg)

#Variance Test Time
veg2 = droplevels(
  subset(
 veg, treatment %in% c('control', 'clipped')   
  ))
veg2$treatment = factor(veg2$treatment)

var.test(
  pine ~ treatment,
  data = veg2)

#Testing for Normality

shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

#Non-parametric test of homogeneity of variances

fligner.test(
  pine~treatment,
  data = veg2
)

#Tests for multiple variances with an n-sample parametric test
#Testing for homogeneity of variances among all four levels

bartlett.test(pine ~ treatment, data = veg)
#Fligner-Killeen (non-parametric) can also test n variances
fligner.test(pine ~ treatment, data = veg)

#T-Testing for sample means
t.test(
  pine ~ treatment,
  data = veg2)
#This result is suspect, we already know the means are different, AND we know that the "clipped" sample is non-normally disrtributed
wilcox.test(
  pine~treatment,
  data = veg2
)

#Time to work with a new data set
install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)

t.test(mice2$before, mice2$after, paired = TRUE)
wilcox.test(mice2$before, mice2$after, paired = TRUE)

t.test(mice2$before, mice2$after, paired =FALSE)

#Over to the Salamanders again
disp = read.csv(here("data", "dispersal.csv"))
disp

#dist.class = distance class, based on 100 m intervals
#disp.rate.ftb = standardized dispersal rate for first-time breeders, which can be interpreted as a relative dispersal probability
#disp.rate.eb =standardized dispersal rate for experienced breeders, which can be interpreted as a relative dispersal probability

plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue")
#Test the significance of the correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use = 'complete.obs')

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use = 'complete.obs',
  method = "spearman")

plot(
  ecdf(disp$disp.rate.ftb),
  verticals = TRUE,
  main = "Plot of Marbled Salamanders\nFirst Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1,3),
  legend = c("First-Time", "Experienced"),
  title = "Breeder Class")

#Testing for Differences in Distributions
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#Proportion comparisons
#Sex-linked killing
# I feel the need to qualify the above topic (we are talking about salamander mortality rates at a road crossing)

prop.test(
  x = c(4,16),
  n = c(40,250))
prop.test(
  x = c(8,32),
  n = c(80,500))

#Contingency: Chi-square test
owls = matrix(c(16, 9, 4, 11), nrow = 2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

round(chisq_owls$expected, 1)
chisq_owls$observed
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)

#Fisher's Exact Test
fisher.test(owls)

# Moving on to the government surveillance drones
birds = read.csv(here("data", "bird.sta.csv"))
hab = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by = c("basin", "sub", "sta"))
br_creeper_table = table(
                     birdhab$s.edge,
                     birdhab$BRCR > 0)[, 2:1]
br_creeper_table
colnames(br_creeper_table) = c("Present", "Absent")
rownames(br_creeper_table) = c("Exterior", "Interior")

#Production Time
#Q1-2
chi_creep = chisq.test(br_creeper_table)
chi_creep

#Q3-5
require(palmerpenguins)
fit_fl_sp =
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)
fit_species =
  lm(
    formula = body_mass_g ~ species,
    data = penguins)
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
fit_both =
  lm(
    formula = body_mass_g ~ species*sex,
    data = penguins)

#Q6-9
boxplot(formula = body_mass_g ~ species, data = penguins,
        main = "Plot of Body Mass as Predicted by Species",
        xlab = "Species",
        ylab = "Body Mass (g)")
boxplot(formula = body_mass_g ~ sex, data = penguins,
        main = "Plot of Body Mass as Predicted by Sex",
        xlab = "Sex",
        ylab = "Body Mass (g)")
boxplot(formula = body_mass_g ~ species*sex, data = penguins,
        main = "Doubly Conditional Plot of\nBody Mass as predicted by Sex and Species",
        xlab = "Species & Sex",
        ylab = "Body Mass (g)",
        names = c("Adelie\nFemale","Chinstrap\nFemale","Gentoo\nFemale","Adelie\nMale", "Chinstrap\nMale", "Gentoo\nMale"),
        col = c("firebrick4","steelblue3", "forestgreen", "firebrick4", "steelblue3", "forestgreen"))

#Q10-12
bartlett.test(body_mass_g ~ species, data = penguins)
bartlett.test(body_mass_g ~ sex, data = penguins)

#Q13-14
dat_groups = aggregate(
  body_mass_g ~ species*sex,
  data = penguins,
  FUN = c
)
dat_groups
bartlett.test(dat_groups$body_mass_g)

#Q15
dat_fl = read.csv(here("data", "trees_FL.csv"))
head(dat_fl)

barplot(table(dat_fl$ProbabilityofFailure),
        main = "Barplot of Probability of Failure",
        xlab = "Ranked Probability of Failure",
        ylab = "Number of Trees Per Rank")
barplot(table(dat_fl$Failure_Standardized),
        main = "Barplot of Standardized Failure",
        xlab = "Type of Failure in Tree",
        ylab = "Number of Trees Per Rank")
hist(dat_fl$DBH_in,
     main = "Histogram of Tree Diameter at Breast Height",
     xlab = "DBH (in)")
plot(dat_fl$HeighttoTop_ft ~ dat_fl$DBH_in,
     main = "Scatterplot of DBH vs Tree Height",
     xlab = "Diameter at Breast Height (in)",
     ylab = "Tree Height (ft)")

#Q16-17
whole = droplevels(subset(dat_fl, Failure_Standardized == "whole"))
intact = droplevels(subset(dat_fl, Failure_Standardized == "none"))
ks.test(whole$DBH_in,intact$DBH_in)

#Q18-20
cor.test(
  dat_fl$DBH_in,
  dat_fl$HeighttoTop_ft,
  use = 'complete.obs',
  method = "spearman")

#Q21-25
failure_table = table(dat_fl$ProbabilityofFailure,
      dat_fl$fail)
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")
levels(dat_fl$fail) = c("No Fail", "Fail")
failure_table
chifail = chisq.test(failure_table)
chifail

chifail$observed - chifail$expected

