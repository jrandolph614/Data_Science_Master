

#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Lectures\\Lecture_5")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY


attach("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Book\\Mendenhall_Sinich\\CD\\data\\Data_sets\\R\\R\\Exercises&Examples\\GFCLOCKS.Rdata")

gfc=GFCLOCKS

names(gfc)[names(gfc)=="AGE"] = "x1"
names(gfc)[names(gfc)=="NUMBIDS"] = "x2"
names(gfc)[names(gfc)=="PRICE"] = "y"


gfc

# MULTIPLE PLOTS

pairs(~ y + x1 + x2 , data=gfc, cex.labels=2, cex.axis=1,cex=1,
main = 'Multiple Plots')


#SETTING TO SEND OUTPUTS TO A TEXT FILE AND ON SCREEN

#sink("trt.txt", append=TRUE, split=TRUE) 

# SETTING UP THE MLR MODEL

mdl1 = lm(y ~  x1  + x2, data=gfc)

summary(mdl1)  # SUMMARY CALCULATION OF THE MODEL

coef=mdl1$coefficients # COEFFICIENTS OF THE MODEL

coef # PRINTING THE COEFFICIENTS 

#sink() # CLOSING TO SEND IT IN TEXT FILE

# COMPUTING PREDICTED VALUES AND PLOTING AGAINST OBSERVED 

obsy=gfc$y  # OBSERVED VALUES
obsy

yhat=mdl1$fit
yhat 

yhat_y=cbind(predict(mdl1), gfc$y) # COMBINING PREDICTED AND OBSERVED 

head(yhat_y) # PRINTING PART OF IT

plot(gfc$y, predict(mdl1), pch = 20, type = 'p', las = 1,
 xlab="Observed", ylab="Predicted", main = 'Observed vs Predicted')

abline(0,1)

library(MASS)

### PROBABILITY PLOTS ######

hii=hatvalues(mdl1)             	## LEVERAGE POINTS 
cd=cooks.distance(mdl1)  		## COOKS DISTANCE 
dfts=dffits(mdl1)        		## DFFITS
e=residuals(mdl1) 			## RESIDUAL
std_e=stdres(mdl1)			## STANDARDIZED RESIDUAL
r=studres(mdl1)   			## STUDENTIZED RESIDUAL 
t = rstudent(mdl1) 			## COMPUTING R-Student



pr=e/(1-hii)                        ## PRESS RESIDUAL
pr

PRS_Stat=sum(pr^2)         		## PRESS STATISTIC
PRS_Stat

SST = sum((gfc$y - mean(gfc$y))^2) ## TOTAL SUM OF SQUARES 
SST

RSq_Prediction = 1 - (PRS_Stat/SST)  ## R-Square - Prediction
RSq_Prediction

# NORMAL PROBAILITY PLOT 

qqnorm(t)
abline(0,1)

# PLOTTING R-Student against Y-HATS

plot(yhat, t, pch = 20, type = 'p', las = 1,
 xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

#abline(0,1)


# PLOTTING R-Student against X1

plot(gfc$x1, t, pch = 20, type = 'p', las = 1,
 xlab="X1", ylab="R-Student",  main = 'X1 vs R-Student')


# PLOTTING R-Student against X2

plot(gfc$x2, t, pch = 20, type = 'p', las = 1,
 xlab="X2", ylab="R-Student",  main = 'X2 vs R-Student')


### SECOND MODEL ONLY WITH X2= TEMPERATURE ONLY ####


mdl2 = lm(y ~  x2 , data=gfc)

hii2=hatvalues(mdl2)             	## LEVERAGE POINTS 
e2=residuals(mdl2) 			## RESIDUALS

pr2=e2/(1-hii2)                        ## PRESS RESIDUAL
pr2

PRS_Stat2=sum(pr2^2)         		## PRESS STATISTIC
PRS_Stat2

SST = sum((gfc$y - mean(gfc$y))^2) ## TOTAL SUM OF SQUARES 
SST

RSq_Prediction2 = 1 - (PRS_Stat2/SST)  ## R-Square - Prediction
RSq_Prediction2


