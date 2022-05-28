


#Multiple Linear Regression Example
#fit <- lm(y ~ x1 + x2 + x3, data=mydata)
#summary(fit) # show results

# Other useful functions
#coefficients(fit) # model coefficients
#confint(fit, level=0.95) # CIs for model parameters
#fitted(fit) # predicted values
#residuals(fit) # residuals
#anova(fit) # anova table
#vcov(fit) # covariance matrix for model parameters
#influence(fit) # regression diagnostics 






#SETTING WORK DIRECTORY

setwd("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Lectures\\Lecture_4")

###################   PLOTTING FROM Rdata ###############################

attach("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Book\\Mendenhall_Sinich\\CD\\data\\Data_sets\\R\\R\\Exercises&Examples\\NAVALBASE.Rdata")


library(foreign)
library(xtable)
#library(stargazer)

#print(xtable(summary(mdl1), type = "latex"), file = "R2latex_mdl2.tex")
xtable(NAVALBASE, type = "latex")

NAVALBASE=NAVALBASE[order(NAVALBASE$COST),]
plot(NAVALBASE$PERCENT, NAVALBASE$COST, main="Scatterplot of PERCENT vs COST", xlab="COST ", ylab="PERCENT IMPROVEMENT ", pch=19)

NAVALBASE

### FOR LATEX CODE FOR THE DATA TABLE / SENDING IT TO A FILE 

pdf("NAVALBASE1.pdf", height=6, width=6)
plot(NAVALBASE$PERCENT, NAVALBASE$COST, main="Scatterplot of PERCENT vs COST", xlab="COST ", ylab="PERCENT IMPROVEMENT ", pch=19)
dev.off() 


#library(sas7bdat)
#nvl=read.sas7bdat("C:\\DKushary\\Course\\Stat_Rgression_NB\\Spring_2018\\Book\\Mendenhall_Sinich\\CD\\data\\Data_sets\\SAS\\Exercises&Examples\\NAVALBASE.sas7bdat")

####### RUNNING SIMPLE LIEAR MODEL  ##########

library(stats)

mdl1= lm(PERCENT ~ COST, data=NAVALBASE)

smry1=summary(mdl1)
xtable(summary(mdl1), type = "latex")

summary.aov(mdl1)
xtable(summary.aov(mdl1), type = "latex")



#NAVALBASE = transform(NAVALBASE, COST2=COST*COST) #VARIABLE CAN BE ADDED 


####### GRAPH #####

pdf("NAVALBASE2.pdf", height=6, width=6)  ##### SENDING GRAPH TO A PDF FILE 

plot(NAVALBASE$COST, NAVALBASE$PERCENT , main="Scatterplot of PERCENT vs COST
\n with Least Square Line", xlab="COST ", ylab="PERCENT IMPROVEMENT ", pch=19)

abline(lm(NAVALBASE$PERCENT ~ NAVALBASE$COST), col="red") # regression line (y~x)

r2 = 100*smry1$adj.r.squared # R2 value
r2
smry1$coefficients # Coefficeints table

pvalue = smry1$coefficients[2,4] 
b0 = smry1$coefficients[1,1]
b1 = smry1$coefficients[2,1]

#ADDING LEGENDS 

rp = vector('expression',4)
rp[2] = substitute(expression(italic(Y) == b0  +  b1 * ' . X'), 
		list(b0 = format(b0, digits = 3),b1 = format(b1, digits = 4)))[2]
#rp[4] = substitute(expression( italic(Y) == b0  +  b1 * ' . X'), 
#		list(MYOTHERVALUE = format(pvalue, digits = 4)))[2]
rp[1] = substitute(expression('The LEAST SQUARE line is     '), 
		list(b0 = format(b0, digits = 3),b1 = format(b1, digits = 4)))[2]


rp[4] = substitute(expression('Adj - '* R^2 == r2 * '%'), 
		list(r2 = format(r2, digits = 4)))[2]

legend('topleft', legend = rp, bty = 'n')


dev.off() 



##############  QUADRATIC MODEL   #############

mdl2=lm(PERCENT ~ COST + I(COST^2), data=NAVALBASE)

smry2=summary(mdl2)
xtable(summary(mdl2), type = "latex")


summary.aov(mdl2)
xtable(summary.aov(mdl2), type = "latex")

anova(mdl2,mdl1)

NAVALBASE2 = transform(NAVALBASE, pred=predict(mdl2) ) #VARIABLE CAN BE ADDED 


NAVALBASE$pred=predict(mdl2)

NAVALBASE2


xtable(NAVALBAE2, type = "latex")


######  GRAPH 

#pdf("NAVALBASE3.pdf", height=6, width=6)

plot(NAVALBASE$COST, NAVALBASE$PERCENT , main="Scatterplot of PERCENT vs COST
\n with Quadratic Model", xlab="COST ", ylab="PERCENT IMPROVEMENT ", pch=19)

abline(lm(NAVALBASE$PERCENT ~ NAVALBASE$COST), col="red") # regression line (y~x)

points(NAVALBASE$COST, NAVALBASE$pred ,pch=19, col="blue")

with(NAVALBASE, lines(x = NAVALBASE$COST, y = NAVALBASE$pred ))


r2_2 = 100*smry2$adj.r.squared # R2 value
r2_2

smry2$coefficients # Coefficeints table

pvalue2 = smry2$coefficients[2,4] 
b0_2 = smry2$coefficients[1,1]
b1_2 = smry2$coefficients[2,1]
b2_2 = smry2$coefficients[3,1]

b0_2
b1_2
b2_2


#ADDING LEGENDS 


rp = vector('expression',11)

rp[1] = substitute(expression('Observed Points are in Black'),)[2]


rp[3] = substitute(expression('SLR fitted model is the Red Line (Adj - '* R^2 == r2 * '%)'),
                    list(r2 = format(r2, digits = 4)))[2]
rp[4] = substitute(expression(italic(Y) == b0  +  b1 * ' . X'), 
		list(b0 = format(b0, digits = 3),b1 = format(b1, digits = 4)))[2]


rp[6] = substitute(expression('Quadratic fitted model is the Curve Line'),)[2]

rp[7] = substitute(expression(italic(Y) == b0_2  +  b1_2 * ' . X' + b2_2 *' . ' * X^2), 
	list(b0_2 = format(b0_2, digits = 3),b1_2 = format(b1_2, digits = 4),b2_2 = format(b2_2, digits = 4)))[2]


rp[8] = substitute(expression('Adj - '* R^2 == r2_2 * '%'),
                    list(r2_2 = format(r2_2, digits = 4)))[2]


rp[10] = substitute(expression('Blue Points are Predicted Values '),)[2]
rp[11] = substitute(expression('by Quadratic model'),)[2]




#rp[4] = substitute(expression( italic(Y) == b0  +  b1 * ' . X'), 
#		list(MYOTHERVALUE = format(pvalue, digits = 4)))[2]
#rp[1] = substitute(expression('The LEAST SQUARE line is     '), 
#		list(b0 = format(b0, digits = 3),b1 = format(b1, digits = 4)))[2]

legend('topleft', legend = rp, bty = 'n')


dev.off() 


############  COMPLETE SECOND MODEL ########

mdl3= lm(PERCENT ~ COST + BASE + COST*BASE + I(COST^2), data=NAVALBASE )

summary(mdl3)

smry3=summary(mdl3)
xtable(summary(mdl3), type = "latex")


summary.aov(mdl3)
xtable(summary.aov(mdl3), type = "latex")

anova(mdl3)
anova(mdl2,mdl1)

