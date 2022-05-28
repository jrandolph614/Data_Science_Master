
 
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2019\\Lectures\\Lecture_8_GLM")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

load("DISCRIM.RData")

head(DISCRIM)

########## RUNNING THE MODEL ###########

#sink("Example_DISCRIM.txt")

mdl1= glm ( HIRE ~ EDUC + EXP + GENDER, data = DISCRIM, family = binomial)

summary(mdl1) ##### PARAMETER ESTIMATES  (Part (b))

anova(mdl1) #### SEQUENTIAL ANOVA 

confint(mdl1, level=0.98) ##### 98% CONFIDENCE INTERVAL FOR PARAMETERS

#######  TESTING  OVERALL MODEL ###### (Part (a))

chiSq_stat=mdl1$null.deviance-mdl1$deviance
chiSq_stat

dif_df= mdl1$df.null-mdl1$df.residual
dif_df

pvalue=1-pchisq(mdl1$null.deviance-mdl1$deviance, mdl1$df.null-mdl1$df.residual)
pvalue

####### OTHER MODEL CHRACTERISTIC ###########

PearSonRes=sum(residuals(mdl1, type = "pearson")^2)
PearSonRes

pchisq(PearSonRes, df=24, lower.tail=FALSE)  # COMPUTING P-VALUE

DevianceRes=sum(residuals(mdl1, type = "deviance")^2)
DevianceRes

pchisq(DevianceRes, df=24, lower.tail=FALSE)  # COMPUTING P-VALUE


############   PREDICTIONS AT THE OBSERVED POINT ###########

dscrm2=data.frame(DISCRIM)
dscrm2$phat=mdl1$fit

dscrm2


######### PREDICTION AT THE NEW DATA POINT #######

nwdt=with(dscrm2, data.frame(EDUC=4,EXP=0,GENDER=1)) ##### NEW DATA POINT
nwdt

pct=0.95

nwdt2=subset(cbind(nwdt,predict(mdl1,newdata=nwdt, type="link", se=TRUE)),select = -c(residual.scale))
nwdt2

nwdt3=within(nwdt2,{PredictedProb <- plogis(fit)
    LL = plogis(fit - (qnorm((1+pct)/2) * se.fit))
    UL = plogis(fit + (qnorm((1+pct)/2) * se.fit))})
nwdt3


#sink()


######### INFERENCE ABOUT THE PARAMETERS #########

OR=exp(coef(mdl1)) ######## ODDS RATIO OF THE VARIABLES
OR

CI.OR=exp(cbind(coef(mdl1) ,confint(mdl1, level=0.97)))######## CI FOR ODDS RATIO OF THE VARIABLES
CI.OR

library(ResourceSelection)
 
hoslem.test(DISCRIM$HIRE,fitted(mdl1), g=10) ###### NOT TESTED


#########3 VARIANCE COVARIANCE MATRIX OF THE PARAMETERS 

library(MASS)

vcov(mdl1) 


########### JUST ANOTHER WAY TO INCLUDE THE STANDARD ERROR #######

dscrm2b=subset(data.frame(DISCRIM,predict(mdl1, se.fit=TRUE, type='response')),select = -c(residual.scale))
dscrm2b


########## INTERVAL AT THE OBSERVED POINTS #########

dscrm3=subset(data.frame(DISCRIM,predict(mdl1, se.fit=TRUE, type='link')),select = -c(residual.scale))
dscrm3

yhat=exp(dscrm3$fit)/(1+exp(dscrm3$fit)) ####### PREDICTION OF Y
yhat

pct=0.95
LCL.lgodds=dscrm3$fit - (qnorm((1+pct)/2))*dscrm3$se.fit
LCL.lgodds

UCL.lgodds=dscrm3$fit + (qnorm((1+pct)/2))*dscrm3$se.fit
UCL.lgodds


LCL.yhat=exp(LCL.lgodds)/(1+exp(LCL.lgodds))
LCL.yhat

UCL.yhat=exp(UCL.lgodds)/(1+exp(UCL.lgodds))
UCL.yhat


dscrm4=cbind(dscrm3,yhat,LCL.yhat,UCL.yhat)
dscrm4


sink()