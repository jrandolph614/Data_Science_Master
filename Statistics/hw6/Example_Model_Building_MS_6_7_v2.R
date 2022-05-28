

#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2021\\Lectures\\Lecture_6_Model_Building")

#library(xtable)

attach("CLERICAL.Rdata")

head(CLERICAL)
#xtable(head(CLERICAL))




#################  ANOTHER PACKAGE - BASED ON p-VALUES & OTHERS ##################

library(olsrr)

#ols_step_backward_aic, ols_step_backward_p, ols_step_best_subset, 
#ols_step_both_aic, ols_step_forward_aic, ols_step_forward_p

mdl2=lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7  , data=CLERICAL)
anova(mdl2)
summary(mdl2)


###### ALL POSSIBLE SUBSETS ############


K=ols_step_all_possible(mdl2)
K
plot(K)

#xtable(K)

####  TRYING TO IDENTIFY BEST SUBSETS ##########

K2=ols_step_best_subset(mdl2) ### CHOOSING BEST SUBSETS ######
plot(K2)



#####  FORWARD SELECTION USING p-VALUES #####


## progress=TRUE will give every step of the prcedure
## details=TRUE will provide regression result at every step of the prcedure

K3=ols_step_forward_p(mdl2,pent=0.1,progress=TRUE,details=TRUE)   
K3


K4=ols_step_forward_aic(mdl2)   ### BASED ON AIC ###
K4


#####  BACKWARD ELIMINATION USING p-VALUES #####


K5=ols_step_backward_p(mdl2,prem=0.10)   ### BASED ON P-VALUES ###
K5
plot(K5)

K6=ols_step_backward_aic(mdl2)   ### BASED ON AIC ###
K6

#####  STEPWISE SELECTION USING p-VALUES #####


K7=ols_step_both_p(mdl2, details = TRUE, pent=0.1, prem=0.1)
K7





