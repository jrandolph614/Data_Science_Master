

#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2021\\Lectures\\Lecture_6_Model_Building")

#library(xtable)

attach("CLERICAL.Rdata")

head(CLERICAL)
#xtable(head(CLERICAL))

##########   RUNNING STEPWISE #############

library(MASS)

null=lm(Y~1, data=CLERICAL)
null

full=lm(Y~X1 + X2 + X3 +X4 +X5 + X6 + X7, data=CLERICAL)
full


#xtable(summary(full))
#xtable(anova(full))

###########   PACKAGE ######################

library(mixlm)

forward(full, alpha = 0.05, full = TRUE)
forward(full, alpha = 0.10, full = TRUE)

backward(full, alpha = 0.10, full = TRUE, hierarchy = TRUE)
backward(full, alpha = 0.15, full = TRUE, hierarchy = TRUE)

#stepWise(full, alpha.enter = 0.10, alpha.remove = 0.10, full = TRUE)


#xtable(forward(full, alpha = 0.05, full = TRUE))
#xtable(backward(full, alpha = 0.15, full = TRUE, hierarchy = TRUE))
#xtable(stepWise(full, alpha.enter = 0.10, alpha.remove = 0.10, full = TRUE))


#step(null, scope=list(lower=null, upper=full), direction="forward")
#step(full, scope=list(lower=null, upper=full), direction="backward")
#step(null, scope = list(upper=full), data=heat, direction="both")

############  COMPUTING FOR ALL MODELS ############

library(leaps)

tmp =regsubsets(Y~ X1 + X2 + X3 +X4 +X5 + X6 + X7 ,data=CLERICAL,nbest=10,really.big=T, intercept=T)
names(summary(tmp))

almdl=summary(tmp)[[1]]
RSQ=summary(tmp)[[2]]
SSE=summary(tmp)[[3]]
adjR2=summary(tmp)[[4]]
Cp=summary(tmp)[[5]]
BIC=summary(tmp)[[6]]

fnl=cbind(almdl,SSE,RSQ,adjR2,Cp,BIC)
fnl2 = fnl[order(-adjR2),]
fnl2

#xtable(fnl2)




########## PLOTTING ALL MODELS ##################

plot(tmp,scale="adjr2")
plot(tmp,scale="Cp")

library(car)
subsets(tmp, statistic="cp")
#subsets(tmp, statistic="adjr2") 
#subsets(tmp, statistic="bic") 

