#### Lecture 5 Part 1 #####
# Example 4.1
rm(list=ls())
library(readxl)
data_table_B1 <- read_excel("Downloads/data_table_B1.xlsx")
model4.1=lm(y~x2+x7+x8,data=data_table_B1)
root.mse=summary(model4.1)$sigma # Root Mean Square Error --> sqrt(MSE)
y1=data_table_B1$y[1] #Original Y Values
y1.hat=model4.1$fitted.values[1] # Predicted Y Values
X=model.matrix(model4.1) # Model Matrix
H=X%*%(solve(t(X)%*%X)%*%t(X)) # Hat Matrix
h11=H[1,1]
#1) Standardized Residual
e1=y1-y1.hat
d1=e1/root.mse
d1


# Remark: Standardized Residuals have expected value 0 and variance approximately
#1. Hence it is a suspect if it is a close or above 3. But here it is not.

#2) Studentized Residual
MSE=(root.mse)^2
r1=e1/sqrt(MSE*(1-h11))
r1

#Remark: Studentized Residual provides similar information as the previous ones
# but if leverage is also high along with the residual then it needs more
# attention. But that's not the case for this point.

#3) PRESS Residual
e.1=e1/(1-h11)
e.1

#Function for Press
press.stat = function(model1){
  e.1=(pr <- resid(model1)/(1 - lm.influence(model1)$hat))
  return(e.1)
}

# Remark: PRESS residual is a leave out type number which is very informative
# and higher in absolute value is a concern. But sometime it can be higher due
# to higher value of leverage. Here it has a low value of leverage and yet
# the PRESS residual is high and hence it needs to closely looked at.

#4) R-Student - To compute R-student, we first need to compute the estimate of the
# variance without ith observation, Hence
n=length(data_table_B1[[1]])
p=summary(model4.1)$df[1]
var.2=((n-p)*MSE-(e1^2)/(1-h11))/(n-p-1)
t.i=e1/sqrt(var.2*(1-h11))
t.i


# R-student follow t-distribution with (n-p-1) degrees of freedom. Hence at 5% level if it is above the table value
# the point is called outlier. As it is 2.4544, it is an outlier. As it is 2.4544, it is an outlier.
#5) Cook's D
D=(r1^2)/p*h11/(1-h11)
D
#This measures influence on estimation of the coefficients and the point is a suspect if it is above 2 which is not the case.

#5) DFFITS
DFFITS.i=sqrt((h11/(1-h11)))*t.i
DFFITS.i
# It measure the influence as well and it is only a suspect if it is
# higher than 2*sqrt(p / n) = 2 * sqrt( 4 / 28) = 0.7559
# Though it is high but not higher than cutoff.

#All standardized Residuals
table.residuals=data.frame(row(data_table_B1)[,1],data_table_B1$y,model4.1$fitted.values,residuals(model4.1),diag(H),rstandard(model4.1),press.stat(model4.1),rstudent(model4.1),cooks.distance(model4.1),dffits(model4.1))
colnames(table.residuals)=c("Observations","Y","YHAT","Residual","Leverage","STUD_RES","PRESS","R_STUDENT","COOKD","DFFITS")
View(table.residuals)

#### Lecture 5 Part 2 #####
rm(list=ls())
library(qpcR)
library(readxl)
B10 <- read_excel("Downloads/data-table-B10.xlsx")

#a)
model1=lm(y~x1+x2,data=B10)
summary(model1)
anova(model1)
RMSE.mdl1=summary(model1)$sigma
Dependent_Mean=mean(B10$y)

model2=lm(y~x2,data=B10)
summary(model2)
anova(model2)
RMSE.mdl2=summary(model2)$sigma
Dependent_Mean=mean(B10$y)

# Above are the outputs from two models. Model-1 has two variables (X1 and X2)
# and Model-2 has only X2.

#b)
#plot(model1) # The second plot will show the Normal Q-Q plot
# or (Method 2)
qqnorm(model1$residuals)

# As it is not at all a straight line there is some problem with the
# normality assumption

# c)
#plot(model1) # The first plot will show the Predicted vs R-Student
# or (Method 2)
plot(model1$fitted.values,rstudent(model1),xlab="Predicted",ylab="R-Student",xlim=c(0,2),ylim=c(-1,5))
title("Predicted vs. R-Student")
#c) As discussed before, this does not show a linear relationship. Hence
# some transformation is necessary. (done in next chapter)

#d)
#Function for Press
X1=model.matrix(model1) # Model Matrix
X2=model.matrix(model2) # Model Matrix
H1=X1%*%(solve(t(X1)%*%X1)%*%t(X1)) # Hat Matrix
H2=X2%*%(solve(t(X2)%*%X2)%*%t(X2)) # Hat Matrix
press.stat = function(model1){
  e.1=(pr <- resid(model1)/(1 - lm.influence(model1)$hat))
  return(e.1)
}
model1.press=sum(press.stat(model1)^2)
model2.press=sum(press.stat(model2)^2)
anova(model1)
anova(model2)
table_d=data.frame(c(1,2),c("MODEL1","MODEL2"),c("PARMS","PARMS"),c("y","y"),c(RMSE.mdl1,RMSE.mdl2),c(model1.press,model2.press),c(summary(model1)$coefficients[1,1],summary(model2)$coefficients[1,1]),c(summary(model1)$coefficients[2,1],"NA"),c(summary(model1)$coefficients[3,1],summary(model2)$coefficients[2,1]))
colnames(table_d)=c("OBS","MODEL","TYPE","DEPVAR","RMSE","PRESS","INTERCEPT","X1","X2")
View(table_d)
LOF.test(model1)
LOF.test(model2)

#As we see in the SAS output below the PRESS statistic is much higher for Model-2 and hence
#if one out of these two needs to be chosen then Model-1 has a better prediction power.
#Also not that LOF has been computed for both models but Model-1 does not have any repeat values
# and as a result LOF stat could not be calculated
#### Lecture 5 Part 3 #####
rm(list=ls())
library(readxl)
elec <- read_excel("Downloads/data_ex_5_2(Electric_Utility).xlsx")
model1=lm(`y (kW)`~`x (kWh)`,data=elec)
plot(elec$`x (kWh)`,elec$`y (kW)`,xlab="Usage",ylab="Demand",xlim=c(0,4000),ylim=c(0,15))
title("Scatter Plot Example 5.2")
anova(model1)
summary(model1)
# An electric utility is interested in developing a model relating peak-hour
# demand (y) to total energy usage during the month (x). Data for 53 residential
# customers for the month of August are given in the book . By looking at the
# scatter diagram (in next 2 slides), a simple linear regression model is assumed,
# and the least-squares fit
# The analysis of variance Table above shows the model is highly significant.
# The model R2 = 0.7046 that is, about 70% of the variability in demand
# is accounted for by the straight-line fit to energy usage which is apparently
# reasonable. The summary statistics do not reveal any obvious problems with
# this model. Also there seems to be one high leverage point (needs to checked).
plot(model1$fitted.values,rstudent(model1),xlab="y_hat",ylab="t_i",xlim=c(0,13),ylim=c(-3,3))
title("y_hat vs. t_i - Example 5.1")
# A plot of the R-student residuals versus the fitted values shows that error
# variance may be increasing with the mean. That is to say that the error
# variance is increasing as energy consumption increases. A transformation may
# be helpful in correcting this model inadequacy.
# A common transformation to try first is square root transformation. Hence
# the new model tried is
# y* =√y = β0 + β1 x + e
# the resulting least square is
# yˆ* = 0.5822 + 0.0009529 x
# So the next step is to go through the same plotting process to check
# whether the variance has stabilized or not.
# The R-student values from this new least-squares fit are plotted against
# again (next slide). The visual inspection of this graph suggests that the vari-
#   ance is stable. Consequently, we conclude that the transformed model is ade-
#   quate.
model_sqrt=lm(sqrt(`y (kW)`)~`x (kWh)`,data=elec)
summary(model_sqrt)
plot(model_sqrt$fitted.values,rstudent(model_sqrt),xlab="y_hat",ylab="t_i",xlim=c(0,4),ylim=c(-4,2))
title("y_hat vs. t_i - Transformed Data Example 5.1")
# Note that there is one suspiciously large residual and one customer whose
# energy usage is somewhat large. The effect of these two points on the fit should
# be studied further before the model is released for use.
#### Lecture 5 Part 3 - Example 5.3 #####
rm(list=ls())
library(readxl)
wind <- read_excel("Downloads/data_ex_5_3(Windmill).xlsx")
# Example 5.3 (Original Data set is in the reference book A research engineer
#              is investigating the use of a windmill to generate electricity. He has collected
#              data on the DC output from his windmill and the corresponding wind velocity.
#              Scatter plot is as follows.
plot(wind$`Wind Velocity, xi (mph)`,wind$`DC Output, yi`,xlim=c(0,12),ylim=c(0,3),xlab="Wind Velocity, X",ylab="DC Output, Y")
title("Scatter Plot - Example 5.3")
model1=lm(`DC Output, yi`~`Wind Velocity, xi (mph)`,data=wind)
summary(model1)
# Inspection of the scatter diagram indicates that the relationship between
# DC output (y) and wind velocity (x) may be nonlinear. However, we initially
# fit a straight-line model to the data. The summary statistics for the SLR
# model are R2 = 0.8745, M SR = 0.0557, and F − Stat = 160.26 (the P value
# is less than 0.0001).
plot(model1$fitted.values,model1$residuals,xlim=c(0.4,2.6),ylim=c(-0.6,0.4),xlab="y_hat_i",ylab="e_i")
title("Residual vs. Y_hat - Example 5.3")
# This residual plot indicates model inadequacy and implies that the linear
# relationship has not captured all of the information in the wind speed variable.
# Note that the curvature that was apparent in the scatter diagram is greatly
# amplified in the residual plot.

#...
# Hence a more reasonable model for the windmill data that incorporates an
# upper asymptote would be the reciprocal model. Scatter diagram with the
# transformed variable x0 = 1/x shows almost a perfect straight line indicating
# that the reciprocal transformation is appropriate.
x_inv=(wind$`Wind Velocity, xi (mph)`)^-1
model_recip=lm(`DC Output, yi`~x_inv,data=wind)
plot(1/wind$`Wind Velocity, xi (mph)`,wind$`DC Output, yi`,xlim=c(0,0.5),ylim=c(0,3),xlab="X' = 1/x",ylab="DC Output, Y")
title("Scatter Plot - Transformed - Example 5.3")
summary(model_recip)
anova(model_recip)
# The new transformed fitted regression model summary statistics are
# R2 = 0.9800, M SR = 0.0089, F0 = 1128.43 & P − value < 0.0001. A
# plot of R-student against ŷ values does not reveal any serious problem with
# inequality of variance. Other residual plots are satisfactory, and so because
# there is no strong signal of model inadequacy, we conclude that the transformed
# model is satisfactory.
plot(model_recip$fitted.values,rstudent(model_recip),xlab="y_hat_i",ylab="t_i",xlim=c(0,3),ylim=c(-3,2))
title("Residual vs. Y_hat - Transformed - Example 5.3")