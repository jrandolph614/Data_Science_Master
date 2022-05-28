#SETTING WORK DIRECTORY

setwd("P:\\Regression analysis\\Session 8")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

tstfr=readxl::read_xlsx("Data_HW_8.xlsx",1) # READING AND CREATING DATA 

tstfr


mdl1= glm ( y ~ x, data = tstfr, family = binomial)

summary(mdl1) ##### PARAMETER ESTIMATES 

library(ResourceSelection)

hoslem.test(tstfr$y,fitted(mdl1), g=8) ###### NOT TESTED

OR=exp(coef(mdl1)) ######## ODDS RATIO OF THE VARIABLES
OR


##########  FITTING QUADRATIC MODEL  ##############

tstfr$xsq=tstfr$x^2


mdl1b= glm ( y ~ x + xsq, data = tstfr, family = binomial)

summary(mdl1b) ##### PARAMETER ESTIMATES 



################ PROBLEM 2 ########################

library(xlsx)

vcl=readxl::read_xlsx("Data_HW_8.xlsx",2) # READING AND CREATING DATA 

vcl


mdl2= glm ( y ~ x1 + x2, data = vcl, family = binomial)

summary(mdl2) ##### PARAMETER ESTIMATES 

# FOR B

library(ResourceSelection)

hoslem.test(vcl$y,fitted(mdl2), g=8) ###### NOT TESTED

# FOR C

OR=exp(coef(mdl2)) ######## ODDS RATIO OF THE VARIABLES
OR


# FOR D

nwdt=with(vcl, data.frame(x1=45000, x2=5)) ##### NEW DATA POINT
nwdt

pct=0.95

nwdt2=subset(cbind(nwdt,predict(mdl2,newdata=nwdt, type="link", se=TRUE)),select = -c(residual.scale))
nwdt2

nwdt3=within(nwdt2,{PredictedProb <- plogis(fit)
    LL <- plogis(fit - (qnorm((1+pct)/2) * se.fit))
    UL <- plogis(fit + (qnorm((1+pct)/2) * se.fit))})
nwdt3


# FOR E

vcl$x12=vcl$x1*vcl$x2

mdl2b= glm ( y ~ x1 + x2 + x12, data = vcl, family = binomial)

summary(mdl2b) ##### PARAMETER ESTIMATES 

# FOR F 

pct2=0.95

CI_lower1 = coefficients(mdl2)[2] - (qnorm((1+pct2)/2))*summary(mdl2)$coefficients[2,2]
CI_upper1 = coefficients(mdl2)[2] + (qnorm((1+pct2)/2))*summary(mdl2)$coefficients[2,2]

CI_lower1
CI_upper1

CI_lower2 = coefficients(mdl2)[3] - (qnorm((1+pct2)/2))*summary(mdl2)$coefficients[3,3]
CI_upper2 = coefficients(mdl2)[3] + (qnorm((1+pct2)/2))*summary(mdl2)$coefficients[3,3]

CI_lower2
CI_upper2