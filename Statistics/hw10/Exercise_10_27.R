
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2020\\Lectures\\Lecture_10_Time_Series")

############################# TIME SERIES ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE (.RData) IN WORK DIRECTORY

library(foreign)
library(xtable)

load("GOLDMON.RData")

head(GOLDMON)

plot.ts(GOLDMON$PRICE, main="Gold Price by Month - Year 2001-2008")


######## CTEATING TIME VARIABLES ############

gld=data.frame(GOLDMON); gld$tm=time(gld$PRICE);gld$tm2=gld$tm^2


######## SETTING THE MLR MODEL #########

mdl1=lm(PRICE ~ tm + tm2, data=gld)

summary(mdl1)
anova(mdl1)

ar=acf(resid(mdl1),lag.max=1) ##### AUTO-CORRELATION
ar

library(car)
durbinWatsonTest(mdl1, max.lag=1, simulate=TRUE, reps=10000,
    method="normal", alternative="positive")



#########  ESTIMATING USING DIFFERENT METHODS #########

library(nlme)

mdl2A = arima(gld$PRICE, xreg=cbind(gld$tm, gld$tm2), order=c(1,0,0),method="ML") # MAXIMUM LIKELIHOOD
mdl2A

coef(mdl2A)

mdl2B = arima(gld$PRICE, xreg=cbind(gld$tm, gld$tm2), order=c(1,0,0),method="CSS") # CONDITIONAL SUM OF SQUARES METHID
mdl2B

mdl3=gls(PRICE ~ tm + tm2, data=gld, correlation=corARMA(p=1,q=0))
mdl3

########  USING COCHRAN ORCUTT ###############

library(orcutt)

mdl4= cochrane.orcutt(mdl1)
#residuals(cochorct)
summary(mdl4)
#predict(mdl4) ####### PREDICTION WITHOUT THE ERROR TERM

gld2=cbind(gld,predict(mdl4))
gld2




