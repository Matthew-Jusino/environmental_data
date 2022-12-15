install.packages("here")
require(here)
dat_catrate <- read.csv(here("data", "catrate.csv"))
dat_delomys <- read.csv(here("data", "delomys.csv"))
dat_rope <- read.csv(here("data", "rope.csv"))

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

boxplot(body_mass ~ status, data = dat_delomys, 
        main = "Delomys Body Mass in Living and Dead Subjects", 
          ylab = "Body Mass", xlab = "Status",
          col = "lightsalmon4", pch = 20)
