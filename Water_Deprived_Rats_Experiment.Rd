#insert all neccessary libraries

library(multcomp) #multiple comparieson
library(daewr) #design and experiment with r
library(gmodels) #graph models.
library(agricolae)
library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(MASS)

#Water-Deprived Rats (Randomized Complete Block Design)

#accessing built-in data called drug
table(drug$dose); table(drug$rat); summary(drug$rate)

#creating models using anova and displaying summary of findings.
mod1 <- aov(rate ~ rat + dose, data = drug); summary(mod1)
mod2 <- aov(rate ~ dose + Error(rat), data = drug ); summary(mod2)

#creating graphs of the findings

par( mfrow = c(2,2) ) #creates a 2*2 window of 4 graphs (plots) in it. 
plot(mod1, which=5)  #plots the 5th digagnostic plot for mod1. 
plot(mod1, which=1) #plots the 1st digagnostic plot for mod1. 
plot(mod1, which=2) #plots the 2nd digagnostic plot for mod1. 
plot(residuals(mod1) ~ dose, main="Residuals vs Exp. Unit",font.main=1,data=drug)  #generates a scatterplot of residuals of mod 1 vs. the experimental unit (dose) from the dataset drug.

#I DON'T UNDERSTAND THIS PART YET.

fit.contrast( mod1, "dose", c(1, -1,0,0,0),conf=0.95 ) 
lsmeans(mod1, pairwise ~ dose) 
mod1.tukey <- TukeyHSD( mod1,"dose", ordered = T ); mod1.tukey 
compare <- SNK.test( mod1, "dose", alpha = 0.05 ); print(compare) 
