#insert all neccessary libraries

library(multcomp)
library(daewr)
library(gmodels)
library(agricolae)
library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(MASS)

soln <- as.factor(rep(1:3, each = 4)); 
days <- as.factor(rep(c(1,2,3,4), times = 3));
rp <- c(13,22,18,39,16,24,17,44,5,4,1,22);
bg <- data.frame(rp, soln, days)
bg

table(soln); table(days); summary(rp)

mod1 <- aov(rp ~ soln + days); mod1
mod2 <- aov(rp ~ soln + Error(days)); mod2

summary(mod1)
summary(mod2)

par(mfrow = c(2,2))
plot(mod1, which = 5)
plot(mod1, which = 1)
plot(mod1, which = 2)
plot(residuals(mod1) ~ soln + days, main = "Residuals vs. Exp. Unit", font.main=1, data = bg)

fit.contrast(mod1, "soln", c(1, -1, 0), conf=0.95)
lsmeans(mod1, pairwise ~ soln)
mod1bg.tukey <- TukeyHSD(mod1, "soln", ordered=T)
mod1bg.tukey

compare <- SNK.test(mod1, "soln", alpha=0.05)

print(compare)