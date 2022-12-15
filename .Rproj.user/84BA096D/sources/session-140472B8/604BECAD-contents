rm(list = ls())
require(here)
rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)


rope$rope.type
summary(rope$rope.type)
n_obs = 121
n_groups = 6
boxplot(p.cut ~ rope.type, data = rope)

grand_mean = mean(rope$p.cut)
grand_resids = rope$p.cut - grand_mean
sum_grand_resids = sum( (rope$p.cut - grand_mean)^2)
ss_tot = sum_grand_resids
df_total = n_obs - 1

#partitioning variance

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function (x) mean(x))

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x){
    (x - mean(x))
  })
str(agg_resids)

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function (x){
    sum( (x - mean(x))^2)
  })
str(agg_sum_sq_resids)

agg_sum_sq_resids$x
ss_within = sum(agg_sum_sq_resids$x)
ss_within
df_within = n_obs - n_groups

ss_among = ss_tot - ss_within
df_among = n_groups - 1

ms_among = ss_among / df_among
ms_within = ss_within / df_within

f_ratio = ms_among / ms_within
p_value = pf(f_ratio, df_among, df_within, lower.tail = FALSE)

fit_1 = lm(p.cut ~ rope.type, data = rope)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$`Sum Sq`

#Tukey HSD Test
rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

fit_rope_2 = lm(p.cut ~ rope.type, data = rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)
round(rope2_hsd$rope.type, digits =4)

bartlett.test(rope$p.cut, g = rope$rope.type)

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

fit_BLAZE = lm(p.cut ~ rope.type == "BLAZE", data = rope)
fit_BS = lm(p.cut ~ rope.type == "BS", data = rope)
fit_PI = lm(p.cut ~ rope.type == "PI", data = rope)
fit_SB = lm(p.cut ~ rope.type == "SB", data = rope)
fit_VEL = lm(p.cut ~ rope.type == "VEL", data = rope)
fit_XTC = lm(p.cut ~ rope.type == "XTC", data = rope)

resids_BLAZE = residuals(fit_BLAZE)
resids_BS = residuals(fit_BS)
resids_PI = residuals(fit_PI)
resids_SB = residuals(fit_SB)
resids_VEL = residuals(fit_VEL)
resids_XTC = residuals(fit_XTC)

shapiro.test(resids_BLAZE)
shapiro.test(resids_BS)
shapiro.test(resids_PI)
shapiro.test(resids_SB)
shapiro.test(resids_VEL)
shapiro.test(resids_XTC)

#Penguin Dataset Time

require(palmerpenguins)
fpens = droplevels(subset(penguins, sex == "female"))

boxplot(
    body_mass_g ~ species,
    data = fpens,
    xlab = "Species",
    ylab = "Body Mass (g)",
    main = "Female Penguins Body Mass vs Species Comparison"
)

bartlett.test(fpens$body_mass_g ~ fpens$species)

fit_fpens = lm(body_mass_g ~ species, data = fpens)
summary(fit_fpens)
resids_fpens = residuals(fit_fpens)
shapiro.test(resids_fpens)

fpens2 = droplevels(
  subset(
    fpens,
    species %in% c("Adelie", "Chinstrap", "Gentoo"))
)


fpens_hsd = TukeyHSD(aov(fit_fpens))
class(fpens_hsd)
round(fpens_hsd$species, digits =4)
