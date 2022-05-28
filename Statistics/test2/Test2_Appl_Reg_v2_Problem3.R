
#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Tests\\Tests_2021\\Test-2")


############  PROBLEM 3 ####################

library(xlsx)

data3=read.xlsx("Data_Test2_S20_Q3.xlsx", sheetName="Data_Q3") # READING AND CREATING DATA 

data3


library(mixlm)

allvr=lm(y ~ x1 + x2 + x3 + x4 + x5 , data=data3)
allvr

fr=forward(allvr, alpha = 0.05, full = TRUE)
bck=backward(allvr, alpha = 0.05, full = TRUE, hierarchy = TRUE)


