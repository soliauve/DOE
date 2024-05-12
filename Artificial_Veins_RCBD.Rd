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

PSI <- as.factor(rep(c(8500,8700,8900,9100),times=6));
Batch <- as.factor(rep(1:6,each=4))
Y <- c(90.3,92.5,85.5,82.5,89.2,89.5,90.8,89.5,98.2,90.6,89.6,85.6,93.9,94.7,86.2,87.4,87.4,87.0,88.0,78.9,97.9,95.8,93.4,90.7)
Vascular <- data.frame(Y,PSI,Batch) 

head(Vascular) 

table(PSI);table(Batch);summary(Y) 
RCBD1 <- aov(Y~PSI+Batch); RCBD1 
RCBD2 <- aov(Y~PSI+Error(Batch)); RCBD2 
summary(RCBD1) 
summary(RCBD2) 
par(mfrow = c(2,2)); 
plot(RCBD1, which=5); 
plot(RCBD1, which=1); plot(RCBD1, which=2); 
plot(residuals(RCBD1) ~ PSI+Batch, main="Residuals vs Exp. Unit",font.main=1,data=bread) 
fit.contrast(RCBD1, "PSI", c(1, -1,0,0),conf=0.95 ) 
lsmeans(RCBD1, pairwise ~ PSI) 
mod1.tukey <- TukeyHSD(RCBD1,"PSI", ordered = T ); mod1.tukey 
compare <- SNK.test(RCBD1, "PSI", alpha = 0.05 ); print(compare) 
