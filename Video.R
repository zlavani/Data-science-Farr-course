rm(list=ls())
ls()


# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
rm(list=ls())
ls()
# First we will import and format data and then you can write some code to beautify the graphs :).
# Your working directory, set to the folder you just downloaded from Github, e.g.:
setwd("D:\\Lorenzo\\Cursos\\R_markdown\\CD")

######################################
#                                    #
#   Mixed effects modeling in R      #
#                                    #
######################################

## authors: Gabriela K Hajduk, based on workshop developed by Liam Bailey
## contact details: gkhajduk.github.io; email: gkhajduk@gmail.com
## date: 2017-03-09
##

###---- Explore the data -----###

## load the data and have a look at it
load("dragons.RData")

head(dragons)
dragons <- subset( dragons, select = -X )

summary(dragons)

str(dragons)

## W want to know how the body length affects test scores.

## Have a look at the data distribution:

hist(dragons$testScore)  # seems close to normal distribution - good!

## It is good practice to standardise your explanatory variables before proceeding - you can use scale() to do that:

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Our question: is test score affected by body length??

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, ignoring the sites and the mountain ranges for now.

################################ Modelo Lineal Sin tener en cuanta Ef Aleatorios

library(lme4)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)  # load the package

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(basic.lm, which = 1)  # not perfect, but look alright

rlm <- residuals(basic.lm, type = "pearson") # = estandarizados.
predlm <- fitted(basic.lm) 
plot(x = predlm,
     y = rlm,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")
abline(h = 0, lty = 2)
library('car')
leveneTest(rlm, bodyLength2, center="median")

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad
qqnorm(rlm, cex.main = 0.8)
qqline(rlm)
shapiro.test(rlm)



## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here
ggplot(data=dragons,(aes(x=mountainRange, y=testScore, fill=mountainRange))) + geom_boxplot() + ggtitle("")
## We could also plot it colouring points by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
  theme(legend.position = "none")


ggplot(sales, aes(x = SickDaysTaken, y = Revenue, colour = Region)) +
  geom_point(size=1) +
  geom_line(aes(y = predict(mixed.lmer)),size=1) + scale_y_continuous(labels = comma) + ggtitle("Figure D")

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?

#######################################----- Run multiple analyses -----###


## We could run many separate analyses and fit a regression for each of the mountain ranges.

## Lets have a quick look at the data split by mountain range
## We use the facet_wrap to do that

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
  facet_wrap(~ mountainRange) +
  xlab("length") + ylab("test score")

# That's eight analyses. Oh wait, we also have different sites in each mountain range, which similarly to mountain ranges aren't independent. So we could run an analysis for each site in each range separately.
# 
# To do the above, we would have to estimate a slope and intercept parameter for each regression. That's two parameters, three sites and eight mountain ranges, which means 48 parameter estimates (2 x 3 x 8 = 48)! Moreover, the sample size for each analysis would be only 20 (dragons per site).
# 
# This presents problems: not only are we hugely decreasing our sample size, but we are also increasing chances of a Type I Error (where you falsely reject the null hypothesis) by carrying out multiple comparisons. Not ideal!

##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges

## let's add mountain range as a fixed effect to our basic.lm

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)



ggplot(dragons, aes(x = bodyLength2, y = testScore, colour = mountainRange, group = mountainRange)) +
  geom_point(size = 1)    + ggtitle("Model 2")+ #geom_smooth(method="lm", se = FALSE)+
  geom_line(aes(y = predict(mountain.lm)),size=1)
## now body length is not significant

###----- Mixed effects models -----###
# A mixed model is a good choice here: it will allow us to use all the data we have (higher sample size) and account for the correlations between data coming from the sites and mountain ranges. We will also estimate fewer parameters and avoid problems with multiple comparisons that we would encounter while using separate regressions.
# 
# We are going to work in lme4, so load the package (or use install.packages if you don't have lme4 on your computer).
# 
AIC(mountain.lm)
AIC(mixed.lmer)

anova(mixed.lmer2,mixed.lmer,mountain.lm,basic.lm)

###################################################----- First mixed model -----##

### model
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
### plots
plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!
### summary

ggplot(dragons, aes(x = bodyLength2, y = testScore, colour = mountainRange)) +
  geom_point(size=1) +
  geom_line(aes(y = predict(mixed.lmer)),size=1)  + ggtitle("Figure D")

### variance accounted for by mountain ranges
# Simple Pie Chart
slices <- c(339.7, 223.8)
lbls <- c("Montaña", "Residual")
pie(slices, labels = lbls, main="Variance components")


##-- implicit vs explicit nesting --##

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

### create new "sample" variable


##----- Second mixed model -----##

### model
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)

dragons <- within(dragons, sample <- factor(mountainRange:site))
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)
AIC(mixed.lmer2)
### summary

### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

  ##----- Model selection for the keen -----##

### full model
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)
### reduced model

### comparison







