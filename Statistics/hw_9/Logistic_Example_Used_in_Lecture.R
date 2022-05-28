
 
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2019\\Lectures\\Lecture_8_GLM")



# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

pnmc=read.xlsx("data_used_in_lecture.xlsx",2) # READING AND CREATING DATA 

pnmc

lstc=glm(cbind(Severe.Cases, Total.Miners - Severe.Cases) ~ Years.of.Exposure, 
family = binomial(logit), data=pnmc)

lstc
summary(lstc)

######  GRAPHICAL PLOTS ##########

pred_prob = lstc$fit
pred_prob

plot(Severe.Cases/Total.Miners  ~ Years.of.Exposure, data=pnmc)
lines(Severe.Cases/Total.Miners  ~ Years.of.Exposure, data=pnmc)
lines(pnmc$Years.of.Exposure, lstc$fit,  type="l", col="red") 
title(main="Fitted Logistic Regression Line")



#######   COMPUTING CONFIDENCE INTERVAL ##########

library(MASS)

exp(cbind(coef(lstc), confint(lstc,level=0.95))) ## CI FOR EXP(BETA) 

confint(lstc,level=0.95) ###### CI FOR BETA

########## PREDICTING FOR EACH GROUPS IN THE DATA ########

pnmc$pihat=exp(predict(lstc, se.fit=FALSE))/(1 + exp(predict(lstc, se.fit=FALSE)) )
pnmc$prd =pnmc$Total.Miners*pnmc$pihat


pnmc

########## VARIANCE - COVARIANCE MATRIX 

vcov(lstc)   


########## DIAGNOSTICS #######

PearSonRes=sum(residuals(lstc, type = "pearson")^2)
PearSonRes

pchisq(PearSonRes, df=6, lower.tail=FALSE)  # COMPUTING P-VALUE

DevianceRes=sum(residuals(lstc, type = "deviance")^2)
DevianceRes

pchisq(DevianceRes, df=6, lower.tail=FALSE)  # COMPUTING P-VALUE

pnmc$HL=(pnmc$Severe.Cases - pnmc$prd)^2/(pnmc$prd*(1-pnmc$pihat))
pnmc$HL
HL_stat=sum(pnmc$HL)
HL_stat

pchisq(HL_stat, df=5, lower.tail=FALSE)  # COMPUTING P-VALUE


 
########  	QUADRATIC MODEL  #######################

pnmc2=data.frame(pnmc)
pnmc2$x2=pnmc2$Years.of.Exposure*pnmc2$Years.of.Exposure
pnmc2

pnmc


lstc2=glm(cbind(Severe.Cases, Total.Miners - Severe.Cases) ~ Years.of.Exposure + x2, 
family = binomial(logit), data=pnmc2)

lstc2
summary(lstc2)


pnmc2$pihat2=exp(predict(lstc2, se.fit=FALSE))/(1 + exp(predict(lstc2, se.fit=FALSE)) )
pnmc2$prd2 =pnmc2$Total.Miners*pnmc2$pihat2

pnmc2

########## DIAGNOSTICS #######

pred_prob2 = lstc2$fit
pred_prob2

plot(Severe.Cases/Total.Miners  ~ Years.of.Exposure, data=pnmc2)
lines(Severe.Cases/Total.Miners  ~ Years.of.Exposure, data=pnmc2)
lines(pnmc$Years.of.Exposure, lstc2$fit,  type="l", col="red") 
title(main="Fitted Logistic Regression Quadratic Curve")

####### COMPUTING DEVIANCE AND PEARSON CHISQUARE #####


DevianceRes2=sum(residuals(lstc2, type = "deviance")^2)
DevianceRes2

pchisq(DevianceRes2, df=5, lower.tail=FALSE)  # COMPUTING P-VALUE


PearSonRes2=sum(residuals(lstc2, type = "pearson")^2)
PearSonRes2

pchisq(PearSonRes2, df=5, lower.tail=FALSE)  # COMPUTING P-VALUE


pred_prob2 = lstc2$fit
pred_prob2



pnmc2$HL2=(pnmc2$Severe.Cases - pnmc2$prd2)^2/(pnmc2$prd2*(1-pnmc2$pihat2))
pnmc2$HL2
HL_stat2=sum(pnmc2$HL2)
HL_stat2

pchisq(HL_stat2, df=5, lower.tail=FALSE)  # COMPUTING P-VALUE













##########  CODE FOR Problem 13.3 #################

ftnr=read.xlsx("data_used_in_lecture.xlsx",1) # READING AND CREATING DATA 

ftnr

mdl1=glm(cbind(r,n-r) ~ x, family = binomial(logit), data=ftnr)

mdl1

pchisq(0.3719, df=8, lower.tail=FALSE)  # COMPUTING P-VALUE

summary(mdl1)

library(MASS)

exp(cbind(coef(mdl1), confint(mdl1,level=0.9))) ## CI FOR EXP(BETA) 

########  	QUADRATIC MODEL  #######################

ftnr2=data.frame(ftnr)
ftnr2$x2=ftnr2$x*ftnr2$x
ftnr2


mdl2=glm(cbind(r,n-r) ~ x + x2, family = binomial(logit), data=ftnr2)

mdl2
summary(mdl2)


pnmc2$pihat2=exp(predict(lstc2, se.fit=FALSE))/(1 + exp(predict(lstc2, se.fit=FALSE)) )
pnmc2$prd2 =pnmc2$Total.Miners*pnmc2$pihat2

pnmc2



