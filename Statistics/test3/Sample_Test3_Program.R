
 
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2020\\Tests\\Final\\Sample")


library(foreign)
library(xtable)

############################# Problem - 2 - TIME SERIES ###############################

library(xlsx)

dtts=read.xlsx("Sample_Test3_Data.xlsx", sheetName="TimeSeries")
dtts


plot.ts(dtts$yt, ylab="Shipment",main="Shipment Data over Time")

####### FITTING SLR MODEL ############

mdl2 = lm(yt ~ t, data = dtts)
summary(mdl2)

res=resid(mdl2)
res

plot(dtts$t, mdl2$res, ylab="Residuals", xlab="Time", main="Residual Plot over Time - Problem-2", type="o") 
abline(0, 0)  

library(car)
durbinWatsonTest(mdl2, max.lag=1, simulate=TRUE, reps=10000,
    method="normal", alternative="positive")


#### ADDING QUADRATIC TERMS ############


dtts$t2=dtts$t^2

mdl2b = lm(yt ~ t + t2, data = dtts)
summary(mdl2b)

res2=resid(mdl2b)
res2

plot(dtts$t, mdl2b$res, ylab="Residuals", xlab="Time", main="Residual over Time - Problem-2 - Quadratic", type="o") 
abline(0, 0)  

library(car)
durbinWatsonTest(mdl2b, max.lag=1, simulate=TRUE, reps=10000,
    method="normal", alternative="positive")



#########  Problem - 3 - LOGISTIC REGRESSION ############


library(xlsx)

cgr=read.xlsx("Sample_Test3_Data.xlsx", sheetName="Logistic")
cgr

###############    Part(a)  ###################

mdl3=glm(Fail ~ Temp, family = binomial(logit), data=cgr)
mdl3

summary(mdl3)

######### Part(b)     PREDICTION AT THE NEW DATA POINT #######

nwdt=with(cgr, data.frame(Temp=31)) ##### NEW DATA POINT
nwdt

pct=0.97

nwdt2=subset(cbind(nwdt,predict(mdl3,newdata=nwdt, type="link", se=TRUE)),select = -c(residual.scale))
nwdt2

nwdt3=within(nwdt2,{PredictedProb <- plogis(fit)
    LL <- plogis(fit - (qnorm((1+pct)/2) * se.fit))
    UL <- plogis(fit + (qnorm((1+pct)/2) * se.fit))})
nwdt3

#########     Part(c)  ###################

OR=exp(coef(mdl3)) ######## ODDS RATIO OF THE VARIABLES
OR


#########   Part (d)    ADDING QUADRATIC #############

cgr$Temp2=cgr$Temp^2

mdl3b=glm(Fail ~ Temp + Temp2, family = binomial(logit), data=cgr)
mdl3b

summary(mdl3b)

#library(xtable)
#xtable(anova(mdl3b), type="latex")
#xtable(summary(mdl3b), type="latex")



