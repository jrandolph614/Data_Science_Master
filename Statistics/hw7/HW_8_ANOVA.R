

#SETTING WORK DIRECTORY




############################# TWOWAY ANOVA ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

hw95 <- read.xlsx("HW_8_ANOVA_Data.xlsx",sheetName="XR12095") # READING AND CREATING DATA 

hw95

library(reshape)
twoway <- melt(hw95, id=(c("Advisor")))

twoway=rename(twoway, c("variable"="Portfolio", "value" = "Return"))
twoway

mdl1 <- aov( Return ~ Advisor + Portfolio, data=twoway)

summary(mdl1)

TukeyHSD(mdl1,"Portfolio")

TukeyHSD(mdl1,"Advisor") ## TUKEY'S TEST 

with(twoway, pairwise.t.test(x=Return, g=Portfolio, p.adjust="none")) ## FISHER'S LSD 


############################# ONEWAY ANOVA ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

hw96 <- read.xlsx("HW_8_ANOVA_Data.xlsx",sheetName="XR12096") # READING AND CREATING DATA 
hw96

library(reshape)
oneway <- melt(hw96, id=hw96$rownumber)

oneway <- rename(oneway, c("variable"="Brand", "value" = "Strength"))
oneway

mdl2 <- aov( Strength ~ Brand, data=oneway)

summary(mdl2)

TukeyHSD(mdl2,"Brand")

with(oneway, pairwise.t.test(x=Strength, g=Brand, p.adjust="none")) ## FISHER'S LSD 


############################# TWOWAY ANOVA WITH INTERACTION ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

hw99 <- read.xlsx("HW_8_ANOVA_Data.xlsx",sheetName="XR12099") # READING AND CREATING DATA 

hw99

library(reshape)
twoway <- melt(hw99, id=( c(hw99$linenumber,"Style")))

twoway=rename(twoway, c("variable"="Darkness", "value" = "Time"))
twoway

mdl3 <- aov( Time ~ Style + Darkness + Style*Darkness , data=twoway)

summary(mdl3)

TukeyHSD(mdl3,"Style")

TukeyHSD(mdl3,"Darkness") ## TUKEY'S TEST 

with(twoway, pairwise.t.test(x=Time, g=Style, p.adjust="none")) ## FISHER'S LSD 

############################# LATIN SQUARE ###############################

# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

hwls1 <- read.xlsx("HW_8_ANOVA_Data.xlsx",sheetName="LtnSQ1") # READING AND CREATING DATA 

hwls1

mdl4 <- aov( Yield ~ Column + Row + Peanuts , data=hwls1)

summary(mdl4)

TukeyHSD(mdl4,"Peanuts")

TukeyHSD(mdl3,"Type") ## TUKEY'S TEST 

with(hwls1, pairwise.t.test(x=Yield, g=Peanuts, p.adjust="none")) ## FISHER'S LSD 


