
#SETTING WORK DIRECTORY

library(xtable)

setwd("C:\\Dkushary\\Course\\Stat_Rgression_NB\\Tests\\Test_3\\Test-3_2021\\Sample")

library(xlsx)


#################    PROBLEM - 1 ####################

lstc=read.xlsx("Sample_Test3_Appl_Reg_v2.xlsx", sheetName="Logistic")
lstc


mdl1= glm ( y ~ x , data = lstc, family = binomial)

summary(mdl1) ##### PARAMETER ESTIMATES  (Part (b))
#xtable(summary(mdl1), type = "latex")

anova(mdl1) #### SEQUENTIAL ANOVA 
#xtable(anova(mdl1), type = "latex")

CI.OR=exp(50*cbind(coef(mdl1) ,confint.default(mdl1, level=0.98)))######## CI FOR ODDS RATIO OF THE VARIABLES
CI.OR


DevianceRes=sum(residuals(mdl1, type = "deviance")^2)
DevianceRes

pchisq(DevianceRes, df=23, lower.tail=FALSE)  # COMPUTING P-VALUE



lstc$x2=lstc$x^2
mdl1b= glm ( y ~ x + x2, data = lstc, family = binomial)

summary(mdl1b)
xtable(summary(mdl1b), type = "latex")



cgr$Temp2=cgr$Temp^2

mdl3b=glm(Fail ~ Temp + Temp2, family = binomial(logit), data=cgr)
mdl3b

summary(mdl3b)








chiSq_stat=mdl1$null.deviance-mdl1$deviance
chiSq_stat

dif_df= mdl1$df.null-mdl1$df.residual
dif_df

pvalue=1-pchisq(mdl1$null.deviance-mdl1$deviance, mdl1$df.null-mdl1$df.residual)
pvalue






#################    PROBLEM - 3 ####################

library(xlsx)

library(foreign)


tsrs=read.xlsx("Sample_Test3_Appl_Reg_v2.xlsx", sheetName="Timeseries")
tsrs

plot.ts(tsrs$Yt, ylab="Cost",main="Inventary Carrrying Cost over Time")

####### FITTING SLR MODEL ############

mdl2 = lm(Yt ~ t, data = tsrs)
summary(mdl2)
#xtable(summary(mdl2), type = "latex")

res=resid(mdl2)
res

plot(tsrs$t, mdl2$res, ylab="Residuals", xlab="Time", main="Residual Plot over Time - Problem-2", type="o") 
abline(0, 0)  

library(car)

durbinWatsonTest(mdl2, max.lag=2, simulate=TRUE, reps=10000,
    method="normal", alternative="positive")



library(orcutt)
cochorct = cochrane.orcutt(mdl2,convergence = 6)
#residuals(cochorct)
summary(cochorct)


##### PERFORMING COCHRAN-ORCUTT USING REGULAR FORMULA #######


res=resid(mdl2)
res

n= length(res)
rho = sum(res[1:(n-1)]*res[2:n])/sum(res^2)
rho

#plot(vndr$time, mdl2$res, ylab="Residuals", xlab="Time", main="Residual Plot over Time - Problem-2", type="o") 
#abline(0, 0) 

library(zoo) ###### CREATING LAG VARIABLE  TO PERFORM COCHRAN-ORCUTT ###########

tsrs2=merge(zoo(tsrs),Yt_1=lag(zoo(tsrs)$t,k=-1),t_1=lag(zoo(tsrs)$t,k=-1))

# rho=0.681758  USED ON THE WEBSITE, rhohat IS ESTIMATED BY REGRESSION AMONG THE ERRORS WO INTERCEPT


tsrs2$yprime= tsrs2$Yt - rho*tsrs2$Yt_1
tsrs2$xprime= tsrs2$t - rho*tsrs2$t_1
tsrs2

mdl2b = lm(yprime ~ xprime , data = tsrs2, na.action=na.omit)
mdl2b

summary(mdl2b)

dw2=durbinWatsonTest(lm(yprime ~ xprime,data = tsrs2) , max.lag=4, alternative="positive")
dw2


#xtable(summary(mdl2b), type = "latex")








