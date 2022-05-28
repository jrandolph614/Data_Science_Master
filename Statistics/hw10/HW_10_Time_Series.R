
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2020\\Lectures\\Lecture_10_Time_Series\\HW10")

############################# TIME SERIES ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY


#######  Problem - 1 #########

library(xlsx)

emply=read.xlsx("HW10_data.xlsx",sheetName="Problem1") # READING AND CREATING DATA 

emply


# 1(a)  - REGRESSION AND PLOTS WITH LS-LINE WITH INTERCEPT

mod1 = lm(metal ~ vendor, data = emply)
mod1

res=resid(mod1)
res


res=resid(mod1)
res

plot(emply$time, mod1$res, ylab="Residuals", xlab="Month", main="Residual Plot over Time - Model-1", type="o") 
abline(0, 0)  


##### 1(b) -  CALCULATING RHOHAT USING FORMULA ############

n= length(res)
rho = sum(res[1:(n-1)]*res[2:n])/sum(res^2)
rho

library(car)

dw1a=durbinWatsonTest(lm(metal ~ vendor, data = emply) , max.lag=1, alternative="positive")
dw1a



####### USING LIBRARY FOR COCHRAN-ORCUTT MODEL  #########

library(orcutt)

#data(pst, package="orcutt")
mod1= lm(metal ~ vendor, data=emply)
cochorct = cochrane.orcutt(mod1)
#residuals(cochorct)
summary(cochorct)




#######  Problem - 2 #########

library(xlsx)

shr=read.xlsx("HW10_data.xlsx",sheetName="Problem2") # READING AND CREATING DATA 

shr

# (a) REGRESSION AND PLOTS WITH LS-LINE WITH INTERCEPT

mod2 = lm(Share ~ Price, data = shr)
mod2

res2=resid(mod2)
res2

plot(shr$t, mod2$res, ylab="Residuals", xlab="Month", main="Residual Plot over Time - Model-2", type="o") 
abline(0, 0)  

##### (b) CALCULATING RHOHAT USING FORMULA ############

n= length(res2)
rho2 = sum(res2[1:(n-1)]*res2[2:n])/sum(res2^2)
rho2


library(car)

dw2a=durbinWatsonTest(lm(Share ~ Price, data = shr) , max.lag=1, alternative="positive")
dw2a



####### (c) USING LIBRARY FOR COCHRAN-ORCUTT MODEL  #########

library(orcutt)

#data(pst, package="orcutt")
mod2c= lm(Share ~ Price, data=shr)
cochorct = cochrane.orcutt(mod2c)
#residuals(cochorct)
summary(cochorct)



