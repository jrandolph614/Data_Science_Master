

# CHECKING WORKING DIRECTORY 

getwd()

#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2019\\Lectures\\Lecture_4_MLR1")

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

nfldata=read.xlsx("data_table_B1.xlsx",1) # READING AND CREATING DATA 

nfldata

#READING WITHOUT SETTING THE WORK DIRECTORY
#NFLData=read.xlsx("C:\\DKushary\\Course\\Stat_Rgression_NB\\Lectures\\Lecture_4\\data_table_B1.xlsx",1)


# MULTIPLE PLOTS

pairs(~ y + x2 + x7 + x8, data=nfldata, cex.labels=2, cex.axis=1,cex=1)


#SETTING TO SEND OUTPUTS TO A TEXT FILE AND ON SCREEN

#sink("MLR1.txt", append=TRUE, split=TRUE) 



# SETTING UP THE MLR MODEL


mdl1 = lm(y ~  x2  + x7 + x8, data=nfldata)

summary(mdl1)  # SUMMARY CALCULATION OF THE MODEL
anova(mdl1) # ANOVA TABLE

#sink()

coef=mdl1$coefficients # COEFFICIENTS OF THE MODEL

coef # PRINTING THE COEFFICIENTS 

#sink() # CLOSING TO SEND IT IN TEXT FILE

# COMPUTING PREDICTED VALUES AND PLOTING AGAINST OBSERVED 

obsy=nfldata$y  # OBSERVED VALUES
obsy

yhat_y=cbind(predict(mdl1), nfldata$y) # COMBINING PREDICTED AND OBSERVED 

head(yhat_y) # PRINTING PART OF IT

plot(nfldata$y, predict(mdl1), pch = 20, type = 'p', las = 1,
 xlab="Observed", ylab="Predicted", main = 'Observed vs Predicted')

abline(0,1)


#FINDING STANDARD ERROR 

sigmahat= summary(mdl1)$sigma

sigmahat



############ PREDICTION AND CONFIDENCE INTERVAL FOR ANY OBSERVATION  ##############

### SUPPOSE WE WANT TO PREDICT FOR THE FIRST TEAM OF THE DATASET (LOOK AT LECTURE_4_1 SLIDE 4)
### FOR THE FIRST TEAM X2=1985, X7=59.7, X8=2205   

pred1=predict(mdl1,newdata = data.frame(x2=1985, x7=59.7, x8=2205))

pred1 # POINT PREDICTION Y-hat


pred95=predict(mdl1,newdata = data.frame(x2=1985, x7=59.7, x8=2205),interval="prediction", level=0.95)

pred95 ### 95% PREDICTION INTERVAL


conf95=predict(mdl1,newdata = data.frame(x2=1985, x7=59.7, x8=2205),interval="confidence", level=0.95)

conf95 ### 95% CONFIDENCE INTERVAL

