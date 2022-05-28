
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2020\\Tests\\Test-2\\Sample")


#sink("Sol.rtf")
############  PROBLEM 3 ####################

library(xlsx)

dt3=read.xlsx("Sample_Test2_Dataset_xr17073.xlsx", sheetName="Sheet1") # READING AND CREATING DATA 

#dt3
########### COMPUTING CORRELATION MATRIX #########

round(cor(dt3),2)

##########   RUNNING SELECTION USING ALPHA #############

library(mixlm)

full=lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 , data=dt3)
full

forward(full, alpha=0.05,full = TRUE)
#summary(forward(full, alpha=0.05,full = FALSE))
#anova(forward(full, alpha=0.05,full = FALSE))

# FINAL MODEL BY FORWARD SELECTION IS:   Y = X5 +  X10 + X7 + X3 + ERROR


backward(full, alpha = 0.1, full = TRUE, hierarchy = TRUE)

# FINAL MODEL BY BACKWARD SELECTION IS:   Y = X5 +  X10 + X7 + X3 + ERROR

stepWise(full, alpha.enter = 0.05, alpha.remove = 0.1, full = TRUE)

#stepWiseBack(full, alpha.remove = 0.1, alpha.enter = 0.05, full = FALSE)

#################  COMPUTING PRESS STATISTIC  ###########

library(MPV)

pr_st=PRESS(stepWise(full, alpha.enter = 0.05, alpha.remove = 0.1, full = TRUE))

pr_st
#SST<-sum(anova(full)$`Sum Sq`)
SST = sum((dt3$Y - mean(dt3$Y))^2)
SST

R2_Pred= 1 - (pr_st/SST)
R2_Pred


#################### SELECTION USING AIC   ##################

library(MASS)

null=lm(Y~1, data=dt3)
null

full=lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 , data=dt3)
full


step(null, scope=list(lower=null, upper=full), direction="forward",alpha=0.05)
step(full, scope=list(lower=null, upper=full), direction="backward", alpha= 0.10)
step(null, scope = list(upper=full), data=heat, direction="both", alpha.remove = 0.1, alpha.enter = 0.05)

mdls=lm(Y ~ X3 + X5 + X9 + X7 + X10, data=dt3)

summary(mdls)
anova(mdls)


#sink()