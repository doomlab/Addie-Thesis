library(ggplot2)
library(papaja) #devtools::install_github("crsh/papaja")

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

#Typing
mean(master$TTScore, na.rm = TRUE)
sd(master$TTScore, na.rm = TRUE)

#OSPAN
mean(master$Letter.Recall, na.rm = TRUE)
sd(master$Letter.Recall, na.rm = TRUE)

#Math Acc
mean(master$Math.Acc, na.rm = TRUE)
sd(master$Math.Acc, na.rm = TRUE)

##Dataset to get means for those ONLY above 85%
OSPAN = subset(master, Math.Acc >=85)

#Ravens
mean(OSPAN$Ravens.Score, na.rm = T)
sd(OSPAN$Ravens.Score, na.rm = T)

#OSPAN
mean(OSPAN$Letter.Recall, na.rm = TRUE)
sd(OSPAN$Letter.Recall, na.rm = TRUE)

#Math Acc
mean(OSPAN$Math.Acc, na.rm = TRUE)
sd(OSPAN$Math.Acc, na.rm = TRUE)

##cor.test

##ravens + ospan
cor.test(master$Ravens.Score, master$Letter.Recall)

ggplot(master, aes(Ravens.Score, Letter.Recall)) + 
  geom_point() +
  xlab("Ravens Score") + 
  ylab("OSPAN Score") + 
  theme_apa()

##ravens + tt
cor.test(master$Ravens.Score, master$TTScore)

ggplot(master, aes(Ravens.Score, TTScore)) + 
  geom_point() +
  xlab("Ravens Score") + 
  ylab("Typing Speed") + 
  theme_apa()

##ospan + tt
cor.test(master$Letter.Recall, master$TTScore)

ggplot(master, aes(Letter.Recall, TTScore)) + 
  geom_point() +
  xlab("OSPAN Score") + 
  ylab("Typing Speed") + 
  theme_apa()

#power test
library(pwr)
pwr.r.test(n = 41, r = .22, alternative = "two.sided")
pwr.r.test(n = NULL, r = .22, alternative = "two.sided",
           power = .80)

##regression 
model = lm(Ravens.Score ~ Letter.Recall + Math.Acc,  data = master)
summary(model)

library(lm.beta)
summary.lm.beta(model) #to get standardized coefficients
lm.beta(model)

library(ppcor)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score")]))

model2 = lm(TTScore ~ Letter.Recall + Math.Acc, data = master)
summary(model2)
summary.lm.beta(model2)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "TTScore")]))

model3 = lm(TTScore ~ Letter.Recall + Ravens.Score + Math.Acc, data = master)
summary(model3)
summary.lm.beta(model3)
pcor(na.omit(master[ , c("Letter.Recall", "Math.Acc", "Ravens.Score", "TTScore")]))

# model6 = lm(TTScore ~ Letter.Recall * Ravens.Score + Math.Acc, data = master)
# summary(model6)
# summary.lm.beta(model6)


# without poor participants ------------------------------------------------------

##cor.test

##ravens + ospan
cor.test(OSPAN$Ravens.Score, OSPAN$Letter.Recall)

ggplot(OSPAN, aes(Ravens.Score, Letter.Recall)) + 
  geom_point() +
  xlab("Ravens Score") + 
  ylab("OSPAN Score") + 
  theme_apa()

##ravens + tt
cor.test(OSPAN$Ravens.Score, OSPAN$TTScore)

ggplot(OSPAN, aes(Ravens.Score, TTScore)) + 
  geom_point() +
  xlab("Ravens Score") + 
  ylab("Typing Speed") + 
  theme_apa()

##ospan + tt
cor.test(OSPAN$Letter.Recall, OSPAN$TTScore)

ggplot(OSPAN, aes(Letter.Recall, TTScore)) + 
  geom_point() +
  xlab("OSPAN Score") + 
  ylab("Typing Speed") + 
  theme_apa()

