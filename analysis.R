##set working directory to where file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("MasterDataset.csv", stringsAsFactors = F)
options(scipen = 999)

##summary
summary(master)

##cor.test

##ravens + ospan
cor.test(master$Ravens.Score, master$Letter.Recall)
#t = 4.5327, df = 42, p-value = 0.00004778
#CI .33 - .74
#cor .57

##ravens + tt
cor.test(master$Ravens.Score, master$TTScore)
#t = 1.5396, df = 41, p-value = 0.1313
#CI -.07 - .50
#cor .23

##ospan + tt
cor.test(master$Letter.Recall, master$TTScore)
#t = 1.4407, df = 39, p-value = 0.1577
#CI -.09 - .50
#cor .22

pwr.r.test(n = 41, r = .22, alternative = "two.sided")
pwr.r.test(n = NULL, r = .22, alternative = "two.sided",
           power = .80)

##regression (fix column names) 
model = lm(Ravens.Score ~ Letter.Recall + Math.Acc,  data = master)
summary(model)

library(lm.beta)
summary.lm.beta(model) #to get standardized coefficients
#t for raven 3.05, p for raven .0041
#t for OSPAN 2.221, p for OSPAN .032
#rsq 0.401, adjusted rsq .37
#F-statistic 13.7 (41df), p-value .00002772
library(ppcor)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score")]))

model4 = lm(TTScore ~ Letter.Recall + Math.Acc, data = master)
summary(model4)
summary.lm.beta(model4)
#t for OSPAN 0.876, p for OSPAN .39
#t for MathAcc 0.757, p for MathAcc .453
#F-statistic: 1.313 on 2 and 38 DF,  p-value: 0.2809
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "TTScore")]))

model5 = lm(TTScore ~ Letter.Recall + Ravens.Score + Math.Acc, data = master)
summary(model5)
summary.lm.beta(model5)
#t for Intercept 4.902, p for intercept 0.0000191 
#t for OSPAN 0.722, p for OSPAN0.475    
#t for Ravens 0.702, p for 0.487  
#F-statistic: 1.098 on 2 and 37 DF,  p-value: 0.3442
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score", "TTScore")]))

# model6 = lm(TTScore ~ Letter.Recall * Ravens.Score + Math.Acc, data = master)
# summary(model6)
# summary.lm.beta(model6)
