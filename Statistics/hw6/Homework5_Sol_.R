############################################################# Functions to Use #############################################################
#Please load all these functions that I wrote before you run the code

press.resid = function(model_ex){
  e.1=resid(model_ex)/(1 - lm.influence(model_ex)$hat)
  return(e.1)
}

press.statistic=function(model_ex){
  press.stat=sum(press.resid(model_ex)^2)
  SST=sum(anova(model_ex)$'Sum Sq')
  R2.p=1-(press.stat/SST)
  full.analysis=data.frame(press.stat,SST,R2.p)
  colnames(full.analysis)=c("Press Statistic","Total Sum of Squares (SST)", "R2 prediction")
  return(full.analysis)
}

dfbetas.analysis=function(MLR,dataset){
  n=length(dataset[[1]]) 
  model.dfbetas=dfbetas(MLR) #Calculate DFBETAS
  threshold_dfbetas=2/sqrt(n) #Calculate Threshold (Formula in Module 5, Lecture 1, page 15)
  dfbetas_pb1=data.frame(model.dfbetas)
  dfbetas_pb1_b0=dfbetas_pb1$X.Intercept.
  dfbetas_b0_outliers=which(abs(dfbetas_pb1_b0)>threshold_dfbetas) #Get index of observations above threshold for B0
  examine_dfbeta_b0=data.frame(dfbetas_b0_outliers,dfbetas_pb1_b0[dfbetas_b0_outliers],rep(threshold_dfbetas,length(dfbetas_b0_outliers))) #Create table w/ observations, DFBETAS value, threshold
  colnames(examine_dfbeta_b0)=c("Name of Outlier(s)","DFBETAS","Threshold for DFBETAS")
  dfbetas_pb1_b1=dfbetas_pb1$x1 #repeat, (same as B0)
  dfbetas_b1_outliers=which(abs(dfbetas_pb1_b1)>threshold_dfbetas)
  examine_dfbeta_b1=data.frame(dfbetas_b1_outliers,dfbetas_pb1_b1[dfbetas_b1_outliers],rep(threshold_dfbetas,length(dfbetas_b1_outliers)))
  colnames(examine_dfbeta_b1)=c("Name of Outlier(s)","DFBETAS","Threshold for DFBETAS")
  dfbetas_pb1_b2=dfbetas_pb1$x2 #repeat, (same as B0)
  dfbetas_b2_outliers=which(dfbetas_pb1_b2>threshold_dfbetas)
  dfbetas_b2_outliers=which(abs(dfbetas_pb1_b2)>threshold_dfbetas)
  examine_dfbeta_b2=data.frame(dfbetas_b2_outliers,dfbetas_pb1_b2[dfbetas_b2_outliers],rep(threshold_dfbetas,length(dfbetas_b2_outliers)))
  colnames(examine_dfbeta_b2)=c("Name of Outlier(s)","DFBETAS","Threshold for DFBETAS")
  full.analysis=list(examine_dfbeta_b0,examine_dfbeta_b1,examine_dfbeta_b2,threshold_dfbetas) #Place all tables as elements in a list
  names(full.analysis)=c("B0","B1","B2","Threshold")
  return(full.analysis) #return list
}


dffits.analysis=function(MLR,dataset) {
  n=length(dataset[[1]])
  k=summary(MLR)$df[1]
  model.dffits=dffits(MLR)
  threshold_dffits=2*sqrt(k/n) # Calculate threshold (formula in Module 5, Lecture 1, page 15)
  dffits_outliers=which(data.frame(abs(model.dffits))>threshold_dffits) #Find observations above specified threshold
  examine_dffits=data.frame(dffits_outliers,model.dffits[dffits_outliers],rep(threshold_dffits,length(dffits_outliers)))
  colnames(examine_dffits)=c("Name of Outlier(s)","DFFITS","Threshold for DFFITS")
  full.analysis=list(examine_dffits,threshold_dffits)
  names(full.analysis)=c("DFFITS","Threshold")
  return(full.analysis)
}

stand.analysis=function(MLR){
  stand_thresh=3
  model.standard=residuals(MLR)/summary(MLR)$sigma # Get standardized residuals
  standard_outliers=which(abs(model.standard)>stand_thresh) #Find observations above specified thresholds
  examine_standard=data.frame(standard_outliers,model.standard[standard_outliers],rep(stand_thresh,length(standard_outliers)))
  colnames(examine_standard)=c("Name of Outlier(s)","STANDARD RESID.","Threshold for STANDARD Residuals")
  full.analysis=list(examine_standard,stand_thresh)
  names(full.analysis)=c("STANDARD_RESID.","Threshold")
  return(full.analysis)
}



rstudent.analysis=function(MLR,dataset,alpha){
  n=length(dataset[[1]])
  k=summary(MLR)$df[1]
  t.i_thresh=qt(alpha/2,(n-k-1),lower.tail=FALSE) #determines t value based on user's inputted alpha (Formula on page 10, Module 5, Lecture 1)
  model.rstudent=rstudent(MLR)
  rstudent_outliers=which(abs(model.rstudent)>t.i_thresh)
  examine_rstudent=data.frame(rstudent_outliers,model.rstudent[rstudent_outliers],rep(t.i_thresh,length(rstudent_outliers)))
  colnames(examine_rstudent)=c("Name of Outlier(s)","RSTUDENT","Threshold for RSTUDENT Residuals")
  full.analysis=list(examine_rstudent,t.i_thresh)
  names(full.analysis)=c("RSTUDENT","Threshold")
  return(full.analysis)
}

table.residuals=function(MLR,dataset){
  t.r=data.frame(row(dataset)[,1],dataset$y,MLR$fitted.values,residuals(MLR),lm.influence(MLR)$hat,residuals(MLR)/summary(MLR)$sigma,rstandard(MLR),press.resid(MLR),rstudent(MLR),cooks.distance(MLR),dffits(MLR))
  colnames(t.r)=c("Observations","Y","YHAT","Residual","Leverage","Stand_Res","STUD_RES","PRESS","R_STUDENT","COOKD","DFFITS")
  return(t.r)
}

influence.analysis=function(MLR,dataset,alpha.cook){
  yvalues=data.frame(dataset$y)
  leverage=data.frame(lm.influence(MLR)$hat)
  cooksd=data.frame(cooks.distance(MLR))
  DFFITS.Model2=data.frame(dffits(MLR))
  DFBETAS.Model2=data.frame(dfbetas(MLR))
  
  # Set Threshold for Cook's Distance Using F Distribution
  k=summary(MLR)$df[1]
  n=length(dataset[[1]])
  
  threshold_dfbetas=dfbetas.analysis(MLR,dataset)$Threshold
  threshold_dffits=dffits.analysis(MLR,dataset)$Threshold
  
  cookd_thresh=qf(alpha.cook,k,n-k)
  #This code will tell you what Observations to Inspect Based on the given threshold
  Inspect_CookD=ifelse(abs(cooksd)>cookd_thresh,"Inspect","0")
  Inspect_DFFIT=ifelse(abs(DFFITS.Model2)>threshold_dffits,"Inspect","0")
  Inspect_DFBETAS_B0=ifelse(abs(DFBETAS.Model2$X.Intercept.)>threshold_dfbetas,"Inspect","0")
  Inspect_DFBETAS_B1=ifelse(abs(DFBETAS.Model2$x1)>threshold_dfbetas,"Inspect","0")
  Inspect_DFBETAS_B2=ifelse(abs(DFBETAS.Model2$x2)>threshold_dfbetas,"Inspect","0")
  
  influence_analysis=data.frame(yvalues,
                               leverage,
                               cooksd,
                               DFFITS.Model2,
                               DFBETAS.Model2,
                               Inspect_CookD,
                               Inspect_DFFIT,
                               Inspect_DFBETAS_B0,
                               Inspect_DFBETAS_B1,
                               Inspect_DFBETAS_B2)
  
  colnames(influence_analysis)=c("Y Value","Leverage","CookD","DFFIT","DFBETAS_B0","DFBETAS_B1","DFBETAS_B2","Inspect Cook's Distance","Inspect DFFIT","Inspect DFBETAS_B0","Inspect DFBETAS_B1","Inspect DFBETAS_B2")
  return(influence_analysis)
}

cooksd.analysis=function(choice=c("method1","method2"),MLR,dataset,alpha=0){
  if(choice=="method1"){ #based on user's choice of method
    k=summary(MLR)$df[1]
    n=length(dataset[[1]])
    threshold_cookd=qf(alpha,k,n-k)
    barplot(cooks.distance(MLR),xlab="Observation Number",ylab="Cook's Distance")
    abline(h=threshold_cookd,col='red',lty=2,lwd=2)
    title("Cook's Distance for Model")
    model.cookd=cooks.distance(MLR)
    cookd_outliers=which(data.frame(model.cookd)>threshold_cookd)
    examine_cookd=data.frame(cookd_outliers,model.cookd[cookd_outliers],rep(threshold_cookd,length(cookd_outliers)))
    colnames(examine_cookd)=c("Name of Outlier(s)","Cook's Distance","Threshold for Cook's Distance")
    full.analysis=list(examine_cookd,threshold_cookd)
    names(full.analysis)=c("CooksDistance","Threshold")
  }
  else if(choice=="method2"){ #based on user's choice of method, this method requires olsrr library
    n=length(dataset[[1]])
    threshold_cookd=4/n
    ols_plot_cooksd_bar(MLR)
    model.cookd_olsrr=data.frame(ols_plot_cooksd_bar(MLR)$data["color"])
    model.cookd=cooks.distance(MLR)
    cookd_outliers=which(model.cookd_olsrr=="outlier")
    examine_cookd=data.frame(cookd_outliers,model.cookd[cookd_outliers],rep(threshold_cookd,length(cookd_outliers)))
    colnames(examine_cookd)=c("Name of Outlier(s)","Cook's Distance","Threshold for Cook's Distance")
    full.analysis=list(examine_cookd,threshold_cookd)
    names(full.analysis)=c("CooksDistance","Threshold")
  }
  return(full.analysis)
}

############################################################# PROBLEM 1 #############################################################
rm(list=setdiff(ls(),c("dfbetas.analysis","dffits.analysis","press.resid","rstudent.analysis","table.residuals","influence.analysis","cooksd.analysis","press.statistic","stand.analysis")))
#Clearing everything from my workspace EXCEPT for functions defined above.
library(readxl)
library(olsrr) #Use this library for completing Influence Analysis (DFFITS, COOK'S D, DFBETAS)
B8 <- read_excel("Desktop/Spring2022/TeachingAssistant/Data/data-table-B8.xlsx")


#1) a
model1=lm(y~x1+x2,data=B8)
summary(model1)
n=length(B8[[1]])
k=summary(model1)$df[1]

#1) b
plot(model1,which=2,col="blue") #Normality Plot of Residuals

#1) c
plot(model1,which=1) #Residuals vs. Predicted Response

#1) d
# INFLUENCE ANALYSIS USING COOK'S DISTANCE #

# METHOD 1 Using F-Distribution (qf(alpha,k,n-k)), where k is number of parameters --> k=summary(model1)$df[1]
#In this case, I selected an alpha = 0.1, typically we select an alpha at 0.5 or lower.
plot(model1,which=4) #From this plot, it looks like 28,34, and 35 is an outlier
cooksd.analysis("method1",model1,B8,0.1) #28 is a serious outlier,using Method 1


#Method 2, Using threshold as 4/N
cooksd.analysis("method2",model1,B8)
#We can see the threshold on the graph
# We also see that 28 is a definite outlier, 34 and 35 only slightly exceed the threshold

#This function reports that 28,34, and 35 are outliers, but from observing the graphs,
# We should only consider 28 as outlier

# INFLUENCE ANALYSIS USING DFBETAS #

ols_plot_dfbetas(model1)

dfbetas.analysis(model1,B8)
#For B0
# 1 is an outlier
#For B1
# 28, 30, 34, 35, and 36 are outliers
#For B2
# 10, 17, and 28
# Threshold = 0.33

# INFLUENCE ANALYSIS USING DFFITS #
ols_plot_dffits(model1)
dffits.analysis(model1,B8)
#For DFFITS
# outliers are 28, 34, and 35


#1) e

View(table.residuals(model1,B8))

stand.analysis(model1)
#Standard Residuals Analysis
# No observations > 3 Outliers = (None)

#Looking for outliers (Example: R-Student)
rstudent.analysis(model1,B8,0.05)
#28 is an outlier, R-STUDENT Residual = -2.328851
# Threshold = 2.036933

#1) f
#For showing approx. relationship
#smoothcurve1=smooth.spline(B8$x1,B8$y) <-- Need at least four unique 'x' values, ignore
smoothcurve2=smooth.spline(B8$x2,B8$y)
par(mfrow=c(1,2))
plot(B8$x2,B8$y,xlab="X2",ylab="Y")
lines(smoothcurve2,col='red')
title("Relationship between\n X2 and Y")
plot(B8$x1,B8$y,xlab="X1",ylab="Y")
title("Relationship between\n X1 and Y")
#As we can see, there is a square root relationship between X2 and Y, thus we should try the model:
# y = B0 + B1*sqrt(X2) + e
# https://www.varsitytutors.com/assets/vt-hotmath-legacy/hotmath_help/topics/graphing-square-root-functions/graph-1.gif
#NEW MODEL
sqrt.x2=sqrt(B8$x2)
new.model1=lm(y~x1+sqrt.x2,data=B8)
summary(new.model1)





############################################################# PROBLEM 2 #############################################################
rm(list=setdiff(ls(),c("dfbetas.analysis","dffits.analysis","press.resid","rstudent.analysis","table.residuals","influence.analysis","cooksd.analysis","press.statistic","stand.analysis")))
graphics.off() #Clear all plots
B10 <- read_excel("Desktop/Spring2022/TeachingAssistant/Data/data-table-B10.xlsx")
#2 a)
model2=lm(y~x1+x2,data=B10)
summary(model2)
n=length(B10[[1]])
k=summary(model2)$df[1]

#2) b
plot(model2,which=2,col="blue") #Normality Plot of Residuals
#This plot indicates that assumptionof normality for this model is violated.

#2) c
plot(model2,which=1) #Residuals vs. Predicted Response
# The graph clearly suggests the violation of linearity. But it can also happen due to any missing important
# independent variable as well.

#2) d

View(press.statistic(model2))
#PRESS statistic is 3.11
#R2.p = 77.75%, we can say that 77.75% of the variation in viscosity can be explained

#e)
plot(model2,which=4) #From this plot, it looks like 1,2,and 40 are in question


#Let's set the threshold using the F-Distribution, choosing alpha of 0.1
cooksd.analysis("method1",model2,B10,0.1) #1 should be considered a serious outlier


#Method 2, Using threshold as 4/N
cooksd.analysis("method2",model2,B10)
#We can see the threshold on the graph
# We also see that 1 is a definite outlier, 2 and 40 only slightly exceed the threshold,
#This function reports that 1,20, and 40 are outliers, but from observing the graphs,
# we should seriously consider 1 as an outlier

ols_plot_dfbetas(model2)

dfbetas.analysis(model2,B10)
#Outliers
# For B0 (40)
# For B1 (1,2,39, and 40)
# For B2 (1,2,11, and 40)

# INFLUENCE ANALYSIS USING DFFITS #
ols_plot_dffits(model2)
dffits.analysis(model2,B10)


View(influence.analysis(model2,B10,0.5))
# "By looking at the influence analysis, it appears that point 1 seems to be an outlier. On the other hand
# point 2 and 40 are relatively high leverage points which does effect the dffit values. Hence those two are
# not considered as outliers."

#f)

View(table.residuals(model2,B10))
#Looking for outliers (Example: R-Student)
rstudent.analysis(model2,B10,0.05)
#Similar to influence analysis, it indicates that point 1 is an outlier. R-STUDENT Residual = 5.536592
stand.analysis(model2)
#Standard Residuals Analysis
# Observation 1 is an outlier
# Standard residual (3.85243) > 3