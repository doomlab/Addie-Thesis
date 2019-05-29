##set working directory to where file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("MasterDataset.csv", stringsAsFactors = F)
options(scipen = 999)

##summary
summary(master)

#Descriptives
#ravens
mean(master$Ravens.Score, na.rm = TRUE)
sd(master$Ravens.Score, na.rm = TRUE)

#OSPAN
mean(master$Letter.Recall, na.rm = TRUE)
sd(master$Letter.Recall, na.rm = TRUE)

#Math Acc
mean(master$Math.Acc, na.rm = TRUE)
sd(master$Math.Acc, na.rm = TRUE)

#Typing
mean(master$TTScore, na.rm = TRUE)
sd(master$TTScore, na.rm = TRUE)

##cor.test

##ravens + ospan
cor.test(master$Ravens.Score, master$Letter.Recall)

##ravens + tt
cor.test(master$Ravens.Score, master$TTScore)

##ospan + tt
cor.test(master$Letter.Recall, master$TTScore)

#power test
pwr.r.test(n = 41, r = .22, alternative = "two.sided")
pwr.r.test(n = NULL, r = .22, alternative = "two.sided",
           power = .80)

##regression (fix column names) 
model = lm(Ravens.Score ~ Letter.Recall + Math.Acc,  data = master)
summary(model)

library(lm.beta)
summary.lm.beta(model) #to get standardized coefficients

library(ppcor)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score")]))

model4 = lm(TTScore ~ Letter.Recall + Math.Acc, data = master)
summary(model4)
summary.lm.beta(model4)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "TTScore")]))

model5 = lm(TTScore ~ Letter.Recall + Ravens.Score + Math.Acc, data = master)
summary(model5)
summary.lm.beta(model5)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score", "TTScore")]))

# model6 = lm(TTScore ~ Letter.Recall * Ravens.Score + Math.Acc, data = master)
# summary(model6)
# summary.lm.beta(model6)
