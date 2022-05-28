


#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Lectures\\Lecture_6_Model_Building")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

heat=read.xlsx("data_table_B21.xlsx",1) # READING AND CREATING DATA 

heat

############  A LOOK AT THE CORRELATION MATRIX ########

round(cor(heat),2)

mdl1=lm(y~x1+x2+x3+x4,data=heat)
mdl1
AIC(mdl1)
BIC(mdl1)

############  COMPUTING FOR ALL MODELS ############

library(leaps)

tmp =regsubsets(y~x1+x2+x3+x4,data=heat,nbest=10,really.big=T, intercept=T)
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

########## PLOTTING ALL MODELS ##################

plot(tmp,scale="adjr2")
plot(tmp,scale="Cp")

library(car)
subsets(tmp, statistic="adjr2") 
subsets(tmp, statistic="bic") 
subsets(tmp, statistic="cp")


##########   RUNNING STEPWISE #############

library(MASS)

null=lm(y~1, data=heat)
null

full=lm(y~x1+x2+x3+x4 , data=heat)
full

step(null, scope=list(lower=null, upper=full), direction="forward")
step(full, scope=list(lower=null, upper=full), direction="backward")
step(null, scope = list(upper=full), data=heat, direction="both")


######### SUMMARY OF THE FINAL MODEL (MAY NOT BE ALL SIGNIFICANT) #####

mdl1 = lm(y~x1+x2+x4,data=heat)

summary(mdl1) # display results 

#################  COMPUTING PRESS STATISTIC  ###########

library(MPV)

PRESS(mdl1)