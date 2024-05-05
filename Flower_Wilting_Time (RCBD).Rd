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

% Flower Wilting Time Experiment (Randomized Complete Block Design)

treat <- c(1,2,3,4) # store treatment in a vector named treat
outdesign <- design.rcbd(treat, 4, seed = 11)  # format: treatment, no. of blocks, sets the random seed for reproducibility, ensuring that the randomization process is consistent across runs.
rcb <- outdesign$book 
levels(rcb$block) <- c("carnation", "daisy", "rose", "tulip") 
rcb #display rcb

#print tables 
table(rcb$block); table(rcb$treat); summary(rcb$plots)

#perform anova test using the model plots = treat + block
mod1 <- aov(plots ~ treat + block, data = rcb ) 
summary(mod1)

#perform 2nd anova test using another model.
mod2 <- aov(plots ~ treat + Error(block), data = rcb ) 
summary(mod2)










